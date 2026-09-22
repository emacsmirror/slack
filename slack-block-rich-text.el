;;; slack-block-rich-text.el --- Block Kit rich text blocks  -*- lexical-binding: t; -*-

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

;; Block Kit rich text blocks.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'eieio)
(require 'slack-util)
(require 'slack-block-util)
(require 'slack-block-rich-text-element)
(require 'slack-mrkdwn)

(defclass slack-rich-text-block ()
  ((type :initarg :type :type string)
   (block-id :initarg :block_id :type (or string null) :initform nil)
   (elements :initarg :elements :type list) ;; list of slack-rich-text-section
   ))

(cl-defmethod slack-block-to-string ((this slack-rich-text-block) &optional option)
  (mapconcat #'(lambda (element) (slack-block-to-string element option))
             (oref this elements)
             ""))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-block) &optional option)
  (mapconcat #'(lambda (element) (slack-block-to-mrkdwn element option))
             (oref this elements)
             ""))

(defun slack-create-rich-text-block (payload)
  (make-instance 'slack-rich-text-block
                 :type (plist-get payload :type)
                 :block_id (plist-get payload :block_id)
                 :elements (mapcar #'slack-create-rich-text-block-element
                                   (plist-get payload :elements))))

(defclass slack-rich-text-block-element ()
  ((type :initarg :type :type string)
   (elements :initarg :elements :type list) ;; list of slack-rich-text-element
   (payload :initarg :payload :type (or null list) :initform nil)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-block-element) &optional _option)
  (format "Implement `slack-block-to-string' for %S\n" (oref this payload)))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-block-element) &optional _option)
  (format "Implement `slack-block-to-mrkdwn' for %S\n" (oref this payload)))

(defun slack-create-rich-text-block-element (payload)
  (let* ((type (plist-get payload :type))
         (element (cond
                   ((string= "rich_text_section" type)
                    (slack-create-rich-text-section payload))
                   ((string= "rich_text_preformatted" type)
                    (slack-create-rich-text-preformatted payload))
                   ((string= "rich_text_quote" type)
                    (slack-create-rich-text-quote payload))
                   ((string= "rich_text_list" type)
                    (slack-create-rich-text-list payload))
                   (t
                    (make-instance 'slack-rich-text-block-element
                                   :type (plist-get payload :type)
                                   :elements (mapcar #'slack-create-rich-text-element
                                                     (plist-get payload :elements)))))))
    (oset element payload payload)
    element))

(defclass slack-rich-text-section (slack-rich-text-block-element) ())

(cl-defmethod slack-block-to-string ((this slack-rich-text-section) &optional option)
  (mapconcat #'(lambda (element) (slack-block-to-string element option))
             (oref this elements)
             ""))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-section) &optional option)
  (mapconcat #'(lambda (element) (slack-block-to-mrkdwn element option))
             (oref this elements)
             ""))

(defun slack-create-rich-text-section (payload)
  (make-instance 'slack-rich-text-section
                 :type (plist-get payload :type)
                 :elements (mapcar #'slack-create-rich-text-element
                                   (plist-get payload :elements))))

;; Code block does not use `style' in `slack-rich-text-element'
(defclass slack-rich-text-preformatted (slack-rich-text-block-element) ())

(cl-defmethod slack-block-to-string ((this slack-rich-text-preformatted) &optional option)
  (let ((text (mapconcat #'(lambda (element) (slack-block-to-string element option))
                         (oref this elements)
                         "")))
    (if (not slack-block-highlight-source)
        (propertize (concat text "\n")
                    'slack-defer-face #'(lambda (beg end)
                                          (overlay-put (make-overlay beg end)
                                                       'face 'slack-mrkdwn-code-block-face))
                    'face 'slack-mrkdwn-code-block-face)
      (pcase-let ((`(,lang . ,hl-text) (slack-block-fontify-text-natively text)))
        (concat
         "\n"
         (propertize
          (format "┌─ %s" lang)
          'face 'slack-block-highlight-source-overlay-face)
         "\n"
         (mapconcat
          'identity
          (mapcar
           (lambda (s)
             (propertize
              (if (string= "" s) " " s)
              'slack-defer-face #'(lambda (beg _end)
                                    (let ((ov (make-overlay beg beg)))
                                      (overlay-put
                                       ov 'before-string
                                       (propertize "│" 'face 'slack-block-highlight-source-overlay-face))))))
           (string-split (string-trim hl-text) "\n"))
          "\n")
         "\n"
         (propertize
          "└─"
          'face 'slack-block-highlight-source-overlay-face)
         "\n")))))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-preformatted) &optional option)
  (let ((text (mapconcat #'(lambda (element) (slack-block-to-mrkdwn element option))
                         (oref this elements)
                         "")))
    (format "```\n%s\n\n```\n" text)))

(defun slack-create-rich-text-preformatted (payload)
  (make-instance 'slack-rich-text-preformatted
                 :type (plist-get payload :type)
                 :elements (mapcar #'slack-create-rich-text-element
                                   (plist-get payload :elements))
                 ))

(defclass slack-rich-text-quote (slack-rich-text-block-element) ())

(cl-defmethod slack-block-to-string ((this slack-rich-text-quote) &optional option)
  (let* ((text (mapconcat #'(lambda (element) (slack-block-to-string element option))
                          (oref this elements)
                          ""))
         (texts (split-string text "[\n\r]" nil nil))
         (text-with-pad (mapconcat #'(lambda (text) (format "%s%s"
                                                            slack-mrkdwn-blockquote-sign
                                                            text))
                                   texts
                                   "\n")))

    (propertize (concat text-with-pad "\n")
                'face 'slack-mrkdwn-blockquote-face)))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-quote) &optional option)
  (let* ((text (mapconcat #'(lambda (element) (slack-block-to-mrkdwn element option))
                          (oref this elements)
                          ""))
         (texts (split-string text "[\n\r]" nil nil)))

    (concat (mapconcat #'(lambda (text) (format "> %s" text))
                       texts
                       "\n")
            "\n")))

(defun slack-create-rich-text-quote (payload)
  (make-instance 'slack-rich-text-quote
                 :type (plist-get payload :type)
                 :elements (mapcar #'slack-create-rich-text-element
                                   (plist-get payload :elements))))

(defclass slack-rich-text-list (slack-rich-text-block-element)
  ((indent :initarg :indent :type number)
   (style :initarg :style :type string) ;; bullet or ordered
   ))

(cl-defmethod slack-block-to-string ((this slack-rich-text-list) &optional option)
  (let ((indent (make-string (* 2 (oref this indent)) ? ))
        (texts (mapcar #'(lambda (element) (slack-block-to-string element option))
                       (oref this elements)))
        (texts-with-dot nil)
        (dot slack-mrkdwn-list-bullet)
        (i 1))
    (dolist (text texts)
      (push (format "%s%s %s"
                    indent
                    (propertize (if (string= (oref this style) "ordered")
                                    (format "%s." i)
                                  dot)
                                'face 'slack-mrkdwn-list-face)
                    text)
            texts-with-dot)
      (setq i (+ i 1)))
    (concat (mapconcat #'identity (reverse texts-with-dot) "\n")
            "\n")))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-list) &optional option)
  (let* ((indent (make-string (* 2 (oref this indent)) ? ))
         (dot "-")
         (i 1))
    (concat (mapconcat #'(lambda (element)
                           (let ((text (format "%s%s %s"
                                               indent
                                               (if (string= (oref this style) "ordered")
                                                   (format "%s." i)
                                                 dot)
                                               (slack-block-to-mrkdwn element option))))
                             (setq i (+ i 1))
                             text))
                       (oref this elements)
                       "\n")
            "\n")))

(defun slack-create-rich-text-list (payload)
  (make-instance 'slack-rich-text-list
                 :type (plist-get payload :type)
                 :elements (mapcar #'slack-create-rich-text-block-element
                                   (plist-get payload :elements))
                 :indent (plist-get payload :indent)
                 :style (plist-get payload :style)))

(provide 'slack-block-rich-text)
;;; slack-block-rich-text.el ends here
