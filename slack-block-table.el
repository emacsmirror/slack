;;; slack-block-table.el --- Block Kit table layout block  -*- lexical-binding: t; -*-

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

;; Block Kit table layout block.

;;; Code:

(require 'eieio)
(require 'slack-util)
(require 'slack-block-layout)
(require 'slack-block-rich-text)

(defface slack-table-border-face
  '((t (:foreground "#586e75")))
  "Face used for table borders."
  :group 'slack)

(defface slack-table-header-face
  '((t (:weight bold)))
  "Face used for table header cells."
  :group 'slack)

(defclass slack-table-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "table")
   (rows :initarg :rows :type list :initform nil)
   (num-columns :initarg :num_columns :type (or null number) :initform nil)
   (border :initarg :border :type (or null number) :initform nil)))

(defun slack-create-table-cell (cell)
  "Build a renderable value for a table CELL.
CELL may be nil, a `raw_text' plist, a `rich_text' plist (with or
without a `block_id'), or something else we don't yet understand."
  (cond
   ((null cell) nil)
   ((and (listp cell) (plist-member cell :type))
    (let ((type (plist-get cell :type)))
      (cond
       ((string= "rich_text" type)
        ;; `slack-rich-text-block' requires a string `block-id'; fall
        ;; back to an empty string when the payload omits one.
        (slack-create-rich-text-block
         (if (plist-get cell :block_id)
             cell
           (plist-put (copy-sequence cell) :block_id ""))))
       ((string= "raw_text" type)
        (or (plist-get cell :text) ""))
       (t (format "%S" cell)))))
   ((stringp cell) cell)
   (t (format "%S" cell))))

(defun slack-create-table-layout-block (payload)
  (make-instance 'slack-table-layout-block
                 :type (plist-get payload :type)
                 :block_id (plist-get payload :block_id)
                 :rows (mapcar #'(lambda (row)
                                   (mapcar #'slack-create-table-cell row))
                                (plist-get payload :rows))
                 :num_columns (plist-get payload :num_columns)
                 :border (plist-get payload :border)
                 :payload payload))

(cl-defmethod slack-block-to-string ((this slack-table-layout-block) &optional option)
  (with-slots (rows) this
    (when (and rows (cl-every #'listp rows))
      (let* ((rendered-rows (mapcar #'(lambda (row)
                                        (mapcar #'(lambda (cell)
                                                    (let ((s (cond
                                                              ((null cell) "")
                                                              ((stringp cell) cell)
                                                              (t (slack-block-to-string cell option)))))
                                                      (or s "")))
                                                  row))
                                      rows))
             (num-cols (apply #'max (mapcar #'length rendered-rows)))
             (col-widths
              (cl-loop for col from 0 below num-cols
                       collect (apply #'max 1
                                      (mapcar #'(lambda (row)
                                                  (string-width
                                                   (substring-no-properties (or (nth col row) ""))))
                                                rendered-rows))))
             (pad (lambda (s w)
                    (let ((plain (substring-no-properties (or s ""))))
                      (concat plain (make-string (max 0 (- w (string-width plain))) ? )))))
             (border (propertize "|" 'face 'slack-table-border-face))
             (hline (concat (propertize "+" 'face 'slack-table-border-face)
                            (mapconcat #'(lambda (w)
                                           (propertize (make-string (+ w 2) ?-)
                                                       'face 'slack-table-border-face))
                                         col-widths
                                         (propertize "+" 'face 'slack-table-border-face))
                            (propertize "+" 'face 'slack-table-border-face)))
             (format-row #'(lambda (row header-p)
                             (apply #'concat border
                                     (cl-loop for col from 0 below num-cols
                                              for w = (nth col col-widths)
                                              for cell = (nth col row)
                                              for padded = (funcall pad cell w)
                                              collect (concat " "
                                                              (if header-p
                                                                  (propertize padded 'face 'slack-table-header-face)
                                                                padded)
                                                              " "
                                                              border)))
                                     )))
        (concat hline "\n"
                (funcall format-row (car rendered-rows) t) "\n"
                hline "\n"
                (mapconcat #'(lambda (row) (funcall format-row row nil))
                           (cdr rendered-rows)
                           (concat "\n"))
                (when (cdr rendered-rows) "\n")
                 hline)))))

(provide 'slack-block-table)
;;; slack-block-table.el ends here
