;;; slack-block-timeline.el --- Block Kit timeline layout block  -*- lexical-binding: t; -*-

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

;; Block Kit timeline layout block.

;;; Code:

(require 'eieio)
(require 'slack-util)
(require 'slack-block-layout)

(defface slack-timeline-point-complete-face
  '((t (:foreground "#859900")))
  "Face for completed timeline points."
  :group 'slack)

(defface slack-timeline-point-in-progress-face
  '((t (:foreground "#b58900")))
  "Face for in-progress timeline points."
  :group 'slack)

(defface slack-timeline-point-pending-face
  '((t (:foreground "#586e75")))
  "Face for pending timeline points."
  :group 'slack)

(defclass slack-timeline-point ()
  ((id :initarg :id :type (or null string) :initform nil)
   (text :initarg :text :type (or null string) :initform nil)
   (status :initarg :status :type (or null string) :initform nil)
   (contents :initarg :contents :type (or null list) :initform nil)
   (icon :initarg :icon :type (or null string) :initform nil)))

(defun slack-create-timeline-point (payload)
  (make-instance 'slack-timeline-point
                 :id (plist-get payload :id)
                 :text (plist-get payload :text)
                 :status (plist-get payload :status)
                 :contents (plist-get payload :contents)
                 :icon (plist-get payload :icon)))

(cl-defmethod slack-block-to-string ((this slack-timeline-point) &optional _option)
  (with-slots (text status) this
    (let ((marker (cond ((string= "complete" status)
                         (propertize "✓" 'face 'slack-timeline-point-complete-face))
                        ((string= "in_progress" status)
                         (propertize "◐" 'face 'slack-timeline-point-in-progress-face))
                        (t (propertize "○" 'face 'slack-timeline-point-pending-face)))))
      (format "  %s %s" marker (or text "")))))

(defclass slack-timeline-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "timeline")
   (points :initarg :points :type list :initform nil)
   (start-ts :initarg :start_ts :type (or null number) :initform nil)
   (end-ts :initarg :end_ts :type (or null number) :initform nil)))

(defun slack-create-timeline-layout-block (payload)
  (make-instance 'slack-timeline-layout-block
                 :type (plist-get payload :type)
                 :block_id (plist-get payload :block_id)
                 :points (mapcar #'slack-create-timeline-point
                                 (plist-get payload :points))
                 :start_ts (plist-get payload :start_ts)
                 :end_ts (plist-get payload :end_ts)
                 :payload payload))

(cl-defmethod slack-block-to-string ((this slack-timeline-layout-block) &optional option)
  (with-slots (points) this
    (if points
        (concat (mapconcat #'(lambda (point) (slack-block-to-string point option))
                            points
                            "\n")
                "\n")
      "")))

(provide 'slack-block-timeline)
;;; slack-block-timeline.el ends here
