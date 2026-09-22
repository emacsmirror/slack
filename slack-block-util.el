;;; slack-block-util.el --- Block Kit utilities  -*- lexical-binding: t; -*-

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

;; Utilities shared by Block Kit modules.

;;; Code:

(require 'eieio)
(require 'slack-util)

(defcustom slack-block-highlight-source nil
  "If non-nil, highlight source blocks in messages.
You need to install `language-detection' for this to work."
  :type 'boolean
  :group 'slack)

(defface slack-block-highlight-source-overlay-face
  '((((class grayscale) (background light))
     :foreground "DimGray" :weight bold)
    (((class grayscale) (background dark))
     :foreground "LightGray" :weight bold)
    (((class color) (min-colors 88) (background light))
     :foreground "Firebrick")
    (((class color) (min-colors 88) (background dark))
     :foreground "chocolate1")
    (((class color) (min-colors 16) (background light))
     :foreground "red")
    (((class color) (min-colors 16) (background dark))
     :foreground "red1")
    (((class color) (min-colors 8) (background light))
     :foreground "red")
    (((class color) (min-colors 8) (background dark))
     :foreground "yellow")
    (t :weight bold))
  "If non-nil, highlight source blocks in messages.
You need to install `language-detection' for this to work.")

(defvar slack-completing-read-function)
(defvar slack-channel-button-keymap)
(defvar slack-user-mention-keymap)
(defvar slack-current-buffer)

(cl-defmethod slack-block-to-string ((_this null) &optional _option)
  nil)

(defun slack-block-fontify-text-natively (text)
  (let* ((map '((cpp c++-mode)
                (clojure lisp-mode)
                (csharp java-mode)
                (emacslisp emacs-lisp-mode)
                (matlab octave-mode)
                (objc c-mode)
                (shell shell-script-mode)
                (visualbasic visual-basic-mode)
                (xml sgml-mode)))
         (language (language-detection-string text)))
    (let* ((lang (symbol-name language))
           ;; (ts (intern (concat lang "-ts-mode")))
           (normal (intern (concat lang "-mode")))
           (other (cadr (assoc language map)))
           (mode (cond
                  ;; ((fboundp ts) ts)
                  ((fboundp normal) normal)
                  ((fboundp other) other)
                  (t 'prog-mode))))
      (with-temp-buffer
        (insert text)
        (delay-mode-hooks (funcall mode))
        (font-lock-ensure)
        (cons (or lang "src") (buffer-string))))))

(provide 'slack-block-util)
;;; slack-block-util.el ends here
