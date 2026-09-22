;;; slack-block-composition.el --- Block Kit composition objects  -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  <yuya373@archlinux>
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

;; Block Kit composition objects.

;;; Code:

(require 'eieio)
(require 'slack-util)
(require 'slack-block-util)

;; Message Composition Objects
;; [Reference: Message composition objects | Slack](https://api.slack.com/reference/messaging/composition-objects)
(defclass slack-message-composition-object () ())

(cl-defmethod slack-block-action-id ((_this slack-message-composition-object))
  "")

(cl-defmethod slack-block-to-string ((this slack-message-composition-object) &optional _option)
  (format "Implement `slack-block-to-string' for %S" (eieio-object-class-name this)))

(defclass slack-text-message-composition-object (slack-message-composition-object)
  ((type :initarg :type :type string) ;; plain_text or mrkdwn
   (text :initarg :text :type string)
   (emoji :initarg :emoji :type (or null boolean) :initform nil)
   (verbatim :initarg :verbatim :type (or null boolean) :initform nil)))

(cl-defmethod slack-block-to-string ((this slack-text-message-composition-object) &optional _option)
  (with-slots (text type) this
    (propertize text 'slack-text-type (cond ((string= "plain_text" type) 'plain)
                                            ((string= "mrkdwn" type) 'mrkdwn)
                                            (t nil)))))

(cl-defmethod slack-block-action-payload ((this slack-text-message-composition-object))
  (with-slots (type text emoji verbatim) this
    (list (cons "type" type)
          (cons "text" text)
          (cons "emoji" (or emoji :json-false)))))

(defun slack-create-text-message-composition-object (payload)
  (when payload
    (make-instance 'slack-text-message-composition-object
                   :type (plist-get payload :type)
                   :text (plist-get payload :text)
                   :emoji (eq t (plist-get payload :emoji))
                   :verbatim (eq t (plist-get payload :verbatim)))))

(defclass slack-confirmation-dialog-message-composition-object (slack-message-composition-object)
  ((title :initarg :title :type slack-text-message-composition-object)
   (text :initarg :text :type slack-text-message-composition-object)
   (confirm :initarg :confirm :type slack-text-message-composition-object)
   (deny :initarg :deny :type slack-text-message-composition-object)))

(cl-defmethod slack-block-handle-confirm ((this slack-confirmation-dialog-message-composition-object))
  (with-slots (title text) this
    (yes-or-no-p (format "%s\n%s"
                         (slack-block-to-string title)
                         (slack-block-to-string text)))))

(defun slack-create-confirmation-dialog-message-composition-object (payload)
  (when payload
    (ignore-errors
      (make-instance 'slack-confirmation-dialog-message-composition-object
                     :title (slack-create-text-message-composition-object
                             (plist-get payload :title))
                     :text (slack-create-text-message-composition-object
                            (plist-get payload :text))
                     :confirm (slack-create-text-message-composition-object
                               (plist-get payload :confirm))
                     :deny (slack-create-text-message-composition-object
                            (plist-get payload :deny))))))

(defclass slack-option-message-composition-object (slack-message-composition-object)
  ((text :initarg :text :type slack-text-message-composition-object)
   (value :initarg :value :type string)))

(defun slack-create-option-message-composition-object (payload)
  (when payload
    (make-instance 'slack-option-message-composition-object
                   :text (slack-create-text-message-composition-object
                          (plist-get payload :text))
                   :value (or (plist-get payload :value) ""))))

(cl-defmethod slack-block-to-string ((this slack-option-message-composition-object))
  (with-slots (text) this
    (slack-block-to-string text)))

(defclass slack-option-group-message-composition-object (slack-message-composition-object)
  ((label :initarg :label :type slack-text-message-composition-object)
   (options :initarg :options :type list) ;; list of slack-option-message-composition-object
   ))

(cl-defmethod slack-block-select-from-option-group ((this slack-option-group-message-composition-object))
  (slack-if-let* ((options-alist (mapcar #'(lambda (e) (cons (slack-block-to-string e) e))
                                         (oref this options))))
      (slack-select-from-list (options-alist (format "Select Option (%s): " (slack-block-to-string this))))))

(cl-defmethod slack-block-to-string ((this slack-option-group-message-composition-object))
  (with-slots (label) this
    (slack-block-to-string label)))

(defun slack-create-option-group-message-composition-object (payload)
  (make-instance 'slack-option-group-message-composition-object
                 :label (slack-create-text-message-composition-object
                         (plist-get payload :label))
                 :options (mapcar #'slack-create-option-message-composition-object
                                  (plist-get payload :options))))

(provide 'slack-block-composition)
;;; slack-block-composition.el ends here
