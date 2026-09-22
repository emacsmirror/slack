;;; slack-block-video.el --- Block Kit video layout block  -*- lexical-binding: t; -*-

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

;; Block Kit video layout block.

;;; Code:

(require 'eieio)
(require 'slack-util)
(require 'slack-block-layout)
(require 'slack-block-composition)
(require 'slack-block-rich-text-element)

(defface slack-video-block-title-face
  '((t (:weight bold)))
  "Face for video block title."
  :group 'slack)

(defface slack-video-block-meta-face
  '((t (:foreground "#586e75")))
  "Face for video block metadata."
  :group 'slack)

(defclass slack-video-layout-block (slack-layout-block)
  ((type :initarg :type :type string :initform "video")
   (video-url :initarg :video_url :type (or null string) :initform nil)
   (thumbnail-url :initarg :thumbnail_url :type (or null string) :initform nil)
   (alt-text :initarg :alt_text :type (or null string) :initform nil)
   (title :initarg :title :type (or null slack-text-message-composition-object) :initform nil)
   (title-url :initarg :title_url :type (or null string) :initform nil)
   (author-name :initarg :author_name :type (or null string) :initform nil)
   (provider-name :initarg :provider_name :type (or null string) :initform nil)
   (provider-icon-url :initarg :provider_icon_url :type (or null string) :initform nil)
   (description :initarg :description :type (or null slack-text-message-composition-object) :initform nil)))

(defun slack-create-video-layout-block (payload)
  (make-instance 'slack-video-layout-block
                 :type (plist-get payload :type)
                 :block_id (plist-get payload :block_id)
                 :video_url (plist-get payload :video_url)
                 :thumbnail_url (plist-get payload :thumbnail_url)
                 :alt_text (plist-get payload :alt_text)
                 :title (slack-create-text-message-composition-object
                         (plist-get payload :title))
                 :title_url (plist-get payload :title_url)
                 :author_name (plist-get payload :author_name)
                 :provider_name (plist-get payload :provider_name)
                 :provider_icon_url (plist-get payload :provider_icon_url)
                 :description (slack-create-text-message-composition-object
                               (plist-get payload :description))
                 :payload payload))

(cl-defmethod slack-block-to-string ((this slack-video-layout-block) &optional _option)
  (with-slots (video-url title title-url author-name provider-name description alt-text) this
    (let* ((title-str (when title (slack-block-to-string title)))
           (desc-str (when description (slack-block-to-string description)))
           (parts (cl-remove-if #'null
                     (list (when title-str
                             (propertize title-str 'face 'slack-video-block-title-face))
                           (when author-name
                             (propertize (format "by %s" author-name)
                                         'face 'slack-video-block-meta-face))
                           (when provider-name
                             (propertize (format "on %s" provider-name)
                                         'face 'slack-video-block-meta-face))
                           (when desc-str
                             (propertize desc-str 'face 'slack-video-block-meta-face))
                           (when (or title-url video-url)
                             (propertize "[video]"
                                         'face 'slack-channel-button-face
                                         'slack-attachment-mention-url (or title-url video-url)
                                         'keymap slack-attachment-mention-keymap
                                         'help-echo "RET: open video"))))))
      (if parts
          (concat (mapconcat #'identity parts " · ") "\n")
        ""))))

(provide 'slack-block-video)
;;; slack-block-video.el ends here
