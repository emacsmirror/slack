;;; slack-block-plan.el --- Block Kit plan layout block  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author:  Andrea <andrea-dev@hotmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Block Kit plan layout block.

;;; Code:

(require 'eieio)
(require 'slack-util)
(require 'slack-block-layout)
(require 'slack-block-rich-text)
(require 'slack-block-element)

(defclass slack-plan-task-card ()
  ((task-id :initarg :task_id :type (or null string) :initform nil)
   (title :initarg :title :type (or null string) :initform nil)
   (status :initarg :status :type (or null string) :initform nil)
   (details :initarg :details :initform nil)))

(defun slack-create-plan-task-card (payload)
  (let ((details-payload (plist-get payload :details)))
    (make-instance 'slack-plan-task-card
                   :task_id (plist-get payload :task_id)
                   :title (plist-get payload :title)
                   :status (plist-get payload :status)
                   :details (when (and details-payload
                                       (string= "rich_text"
                                                (plist-get details-payload :type)))
                              (mapcar #'slack-create-rich-text-block-element
                                      (plist-get details-payload :elements))))))

(defface slack-plan-block-title-face
  '((t (:weight bold :height 1.1)))
  "Face for plan layout block title."
  :group 'slack)

(defface slack-plan-task-complete-face
  '((t (:foreground "#859900")))
  "Face for completed plan tasks."
  :group 'slack)

(defface slack-plan-task-pending-face
  '((t (:foreground "#b58900")))
  "Face for pending plan tasks."
  :group 'slack)

(defun slack-plan-task-status-marker (status)
  (cond ((null status) "•")
        ((string= "complete" status)
         (propertize "✓" 'face 'slack-plan-task-complete-face))
        ((string= "in_progress" status)
         (propertize "◐" 'face 'slack-plan-task-pending-face))
        ((string= "failed" status)
         (propertize "✗" 'face 'slack-button-danger-block-element-face))
        (t (propertize "•" 'face 'slack-plan-task-pending-face))))

(cl-defmethod slack-block-to-string ((this slack-plan-task-card) &optional option)
  (with-slots (title status details) this
    (let* ((marker (slack-plan-task-status-marker status))
           (title-face (if (and status (string= "complete" status))
                           'slack-plan-task-complete-face
                         'slack-plan-task-pending-face))
           (header (format "  %s %s"
                           marker
                           (propertize (or title "") 'face title-face)))
           (details-str (when details
                          (let ((s (mapconcat
                                    #'(lambda (el)
                                        (slack-block-to-string el option))
                                    details
                                    "")))
                            (when (and s (> (length s) 0))
                              (mapconcat #'(lambda (line) (concat "      " line))
                                         (split-string s "\n")
                                         "\n"))))))
      (if details-str
          (concat header "\n" details-str)
        header))))

(defclass slack-plan-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "plan")
   (title :initarg :title :type (or null string) :initform nil)
   (tasks :initarg :tasks :type list :initform nil)))

(defun slack-create-plan-layout-block (payload)
  (make-instance 'slack-plan-layout-block
                 :type (plist-get payload :type)
                 :block_id (plist-get payload :block_id)
                 :title (plist-get payload :title)
                 :tasks (mapcar #'slack-create-plan-task-card
                                (plist-get payload :tasks))
                 :payload payload))

(cl-defmethod slack-block-to-string ((this slack-plan-layout-block) &optional option)
  (with-slots (title tasks) this
    (let ((header (when title
                    (propertize title 'face 'slack-plan-block-title-face)))
          (task-strs (mapcar #'(lambda (task)
                                 (slack-block-to-string task option))
                             tasks)))
      (mapconcat #'identity
                 (cl-remove-if #'null
                               (append (list header) task-strs))
                 "\n"))))

(provide 'slack-block-plan)
;;; slack-block-plan.el ends here
