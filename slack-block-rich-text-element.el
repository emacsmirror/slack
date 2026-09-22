;;; slack-block-rich-text-element.el --- Block Kit rich text inline elements  -*- lexical-binding: t; -*-

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

;; Block Kit rich text inline elements.

;;; Code:

(require 'eieio)
(require 'slack-util)
(require 'slack-block-util)
(require 'slack-mrkdwn)
(require 'slack-room)
(require 'slack-usergroup)
(require 'slack-vip)
(require 'slack-team)

(defclass slack-rich-text-element-style ()
  ((bold :initarg :bold :type boolean)
   (italic :initarg :italic :type boolean)
   (strike :initarg :strike :type boolean)
   (code :initarg :code :type boolean)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-element-style) text)
  "Add property to TEXT according to THIS rich text."
  (let ((face (progn (or (and (oref this bold) 'slack-mrkdwn-bold-face)
                         (and (oref this italic) 'slack-mrkdwn-italic-face)
                         (and (oref this strike) 'slack-mrkdwn-strike-face)
                         (and (oref this code) 'slack-mrkdwn-code-face)))))
    (propertize text
                'face face
                ;; apparently font-lock is enabled with lui and the 'face ends up ignored
                'font-lock-face face)))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-element-style) text)
  (or (and (oref this bold) (format "*%s*" text))
      (and (oref this italic) (format "_%s_" text))
      (and (oref this strike) (format "~%s~" text))
      (and (oref this code) (format "`%s`" text))
      text))

(defun slack-create-rich-text-element-style (payload)
  (when payload
    (make-instance 'slack-rich-text-element-style
                   :bold (eq t (plist-get payload :bold))
                   :italic (eq t (plist-get payload :italic))
                   :strike (eq t (plist-get payload :strike))
                   :code (eq t (plist-get payload :code)))))

(defclass slack-rich-text-element ()
  ((type :initarg :type :type string)
   (style :initarg :style :type (or null slack-rich-text-element-style))
   (payload :initarg :payload :type (or null list) :initform nil)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-element) &optional _option)
  (format "Implement `slack-block-to-string' for %S\n" (oref this payload)))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-element) &optional _option)
  (format "Implement `slack-block-to-mrkdwn' for %S\n" (oref this payload)))

(defun slack-create-rich-text-element (payload)
  (let* ((type (plist-get payload :type))
         (element (cond
                   ((string= "text" type)
                    (slack-create-rich-text-text-element payload))
                   ((string= "channel" type)
                    (slack-create-rich-text-channel-element payload))
                   ((string= "user" type)
                    (slack-create-rich-text-user-element payload))
                   ((string= "emoji" type)
                    (slack-create-rich-text-emoji-element payload))
                   ((string= "link" type)
                    (slack-create-rich-text-link-element payload))
                   ((string= "team" type)
                    (slack-create-rich-text-team-element payload))
                   ((string= "usergroup" type)
                    (slack-create-rich-text-usergroup-element payload))
                   ((string= "date" type)
                    (slack-create-rich-text-date-element payload))
                   ((string= "broadcast" type)
                    (slack-create-rich-text-broadcast-element payload))
                   ((string= "message_mention" type)
                    (slack-create-rich-text-message-mention-element payload))
                   ((string= "attachment_mention" type)
                    (slack-create-rich-text-attachment-mention-element payload))
                   ((string= "canvas" type)
                    (slack-create-rich-text-canvas-element payload))
                   ((string= "citation" type)
                    (slack-create-rich-text-citation-element payload))
                   (t
                    (make-instance 'slack-rich-text-element
                                   :type (plist-get payload :type)
                                   :style (slack-create-rich-text-element-style
                                           (plist-get payload :style)))))))
    (oset element payload payload)
    element))

(defclass slack-rich-text-text-element (slack-rich-text-element)
  ((text :initarg :text :type string)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-text-element) &optional _option)
  (let ((style (oref this style))
        (text (oref this text)))
    (if style (slack-block-to-string style text)
      (propertize text 'face 'slack-message-output-text))))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-text-element) &optional _option)
  (let ((style (oref this style))
        (text (oref this text)))
    (if style (slack-block-to-mrkdwn style text)
      text)))

(defun slack-create-rich-text-text-element (payload)
  (make-instance 'slack-rich-text-text-element
                 :type (plist-get payload :type)
                 :text (plist-get payload :text)
                 :style (slack-create-rich-text-element-style
                         (plist-get payload :style))))

;; (:type "channel" :channel_id "CE096203E")
(defclass slack-rich-text-channel-element (slack-rich-text-element)
  ((channel-id :initarg :channel_id :type string)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-channel-element) option)
  (let* ((team (plist-get option :team))
         (id (oref this channel-id))
         (room (slack-room-find id team))
         (room-name (if room (slack-room-name room team) "(no room)")))
    (unless team
      (error "`slack-rich-text-channel-element' need team as option"))

    (propertize (format "#%s" room-name)
                'room-id id
                'keymap slack-channel-button-keymap
                'face 'slack-channel-button-face)))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-channel-element) option)
  (let ((team (plist-get option :team))
        (id (oref this channel-id)))
    (unless team
      (error "`slack-rich-text-channel-element' need team as option"))

    (slack-propertize-mention-text 'slack-message-mention-face
                                   (format "#%s" (slack-room-name (slack-room-find id team) team))
                                   (format "<#%s>" id))))

(defun slack-create-rich-text-channel-element (payload)
  (make-instance 'slack-rich-text-channel-element
                 :type (plist-get payload :type)
                 :channel_id (plist-get payload :channel_id)
                 :style (slack-create-rich-text-element-style
                         (plist-get payload :style))))

(defclass slack-rich-text-user-element (slack-rich-text-element)
  ((user-id :initarg :user_id :type string)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-user-element) option)
  (let ((team (plist-get option :team))
        (id (oref this user-id)))
    (unless team
      (error "`slack-rich-text-user-element' need team as option"))
    (let ((text (propertize (format "@%s" (or (slack-user-name id team) id))
                           'user-id id
                           'mouse-face 'highlight
                           'keymap slack-user-mention-keymap
                           'face 'slack-message-mention-face)))
      (when (slack-user-vip-p-id id team)
        (add-face-text-property 0 (length text) 'slack-user-vip-face nil text))
      text)))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-user-element) option)
  (let ((team (plist-get option :team))
        (id (oref this user-id)))
    (unless team
      (error "`slack-rich-text-user-element' need team as option"))
    (slack-propertize-mention-text 'slack-message-mention-face
                                   (format "@%s" (or (slack-user-name id team) id))
                                   (format "<@%s>" id))))

(defun slack-create-rich-text-user-element (payload)
  (make-instance 'slack-rich-text-user-element
                 :type (plist-get payload :type)
                 :user_id (plist-get payload :user_id)
                 :style (slack-create-rich-text-element-style
                         (plist-get payload :style))))

(defclass slack-rich-text-emoji-element (slack-rich-text-element)
  ((name :initarg :name :type string)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-emoji-element) &optional _option)
  (let ((name (oref this name)))
    (format ":%s:" name)))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-emoji-element) &optional option)
  (slack-block-to-string this option))

(defun slack-create-rich-text-emoji-element (payload)
  (make-instance 'slack-rich-text-emoji-element
                 :type (plist-get payload :type)
                 :name (plist-get payload :name)))

(defclass slack-rich-text-link-element (slack-rich-text-element)
  ((url :initarg :url :type string)
   (text :initarg :text :type (or null string) :initform nil)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-link-element) &optional _option)
  (let ((text (oref this text))
        (url (oref this url)))
    (format "<%s|%s>" url (or text url))))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-link-element) &optional _option)
  (let ((text (oref this text))
        (url (oref this url)))
    (if text
        (format "[%s](%s)" text url)
      url)))

(defun slack-create-rich-text-link-element (payload)
  (make-instance 'slack-rich-text-link-element
                 :type (plist-get payload :type)
                 :url (plist-get payload :url)
                 :text (plist-get payload :text)
                 :style (slack-create-rich-text-element-style
                         (plist-get payload :style))))

(defclass slack-rich-text-message-mention-element (slack-rich-text-element)
  ((url :initarg :url :type string)
   (text :initarg :text :type (or null string) :initform nil)
   (channel-id :initarg :channel_id :type (or null string) :initform nil)
   (author-id :initarg :author_id :type (or null string) :initform nil)
   (message-ts :initarg :message_ts :type (or null string) :initform nil)
   (thread-ts :initarg :thread_ts :type (or null string) :initform nil)))

(defvar slack-message-mention-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-open-message-mention-url)
    (define-key keymap [mouse-1] #'slack-open-message-mention-url)
    keymap))

(defun slack-open-message-mention-url ()
  "Open the message mention URL at point."
  (interactive)
  (let ((url (get-text-property (point) 'slack-message-mention-url)))
    (when url
      (slack-open-url url))))

(cl-defmethod slack-block-to-string ((this slack-rich-text-message-mention-element) option)
  (let* ((team (plist-get option :team))
         (url (oref this url))
         (author-name (when (and team (oref this author-id))
                        (or (slack-user-name (oref this author-id) team)
                            (oref this author-id))))
         (channel-name (when (and team (oref this channel-id))
                         (let ((room (slack-room-find (oref this channel-id) team)))
                           (when room
                             (slack-room-name room team)))))
         (label (cond (author-name
                       (if channel-name
                           (format "@%s in #%s" author-name channel-name)
                         (format "@%s" author-name)))
                      (channel-name
                       (format "#%s" channel-name))
                      (t url))))
    (propertize label
                'face 'slack-channel-button-face
                'slack-message-mention-url url
                'keymap slack-message-mention-keymap
                'help-echo (format "RET: open message\n%s" url))))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-message-mention-element) &optional _option)
  (let ((text (oref this text))
        (url (oref this url)))
    (if text
        (format "[%s](%s)" text url)
      url)))

(defun slack-create-rich-text-message-mention-element (payload)
  (make-instance 'slack-rich-text-message-mention-element
                 :type (plist-get payload :type)
                 :url (plist-get payload :url)
                 :text (plist-get payload :text)
                 :channel_id (plist-get payload :channel_id)
                 :author_id (plist-get payload :author_id)
                 :message_ts (plist-get payload :message_ts)
                 :thread_ts (plist-get payload :thread_ts)
                 :style (slack-create-rich-text-element-style
                         (plist-get payload :style))))

(defclass slack-rich-text-attachment-mention-element (slack-rich-text-element)
  ((url :initarg :url :type (or null string) :initform nil)
   (text :initarg :text :type (or null string) :initform nil)
   (app-id :initarg :app_id :type (or null string) :initform nil)
   (entity-id :initarg :entity_id :type (or null string) :initform nil)
   (icon-url :initarg :icon_url :type (or null string) :initform nil)
   (icon-name :initarg :icon_name :type (or null string) :initform nil)
   (product-name :initarg :product_name :type (or null string) :initform nil)
   (ts :initarg :ts :type (or null string) :initform nil)
   (channel-id :initarg :channel_id :type (or null string) :initform nil)))

(defvar slack-attachment-mention-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-open-attachment-mention-url)
    (define-key keymap [mouse-1] #'slack-open-attachment-mention-url)
    keymap))

(defun slack-open-attachment-mention-url ()
  "Browse the attachment mention URL at point."
  (interactive)
  (let ((url (get-text-property (point) 'slack-attachment-mention-url)))
    (when url
      (browse-url url))))

(cl-defmethod slack-block-to-string ((this slack-rich-text-attachment-mention-element) &optional _option)
  (let* ((url (oref this url))
         (text (or (oref this text) url))
         (product (oref this product-name))
         (label (if product
                    (format "%s: %s" product text)
                  text)))
    (propertize label
                'face 'slack-channel-button-face
                'slack-attachment-mention-url url
                'keymap slack-attachment-mention-keymap
                'help-echo (format "RET: open link\n%s" url))))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-attachment-mention-element) &optional _option)
  (let ((text (oref this text))
        (url (oref this url)))
    (if text
        (format "[%s](%s)" text url)
      url)))

(defun slack-create-rich-text-attachment-mention-element (payload)
  (make-instance 'slack-rich-text-attachment-mention-element
                 :type (plist-get payload :type)
                 :url (plist-get payload :url)
                 :text (plist-get payload :text)
                 :app_id (plist-get payload :app_id)
                 :entity_id (plist-get payload :entity_id)
                 :icon_url (plist-get payload :icon_url)
                 :icon_name (plist-get payload :icon_name)
                 :product_name (plist-get payload :product_name)
                 :ts (plist-get payload :ts)
                 :channel_id (plist-get payload :channel_id)
                 :style (slack-create-rich-text-element-style
                         (plist-get payload :style))))

(defclass slack-rich-text-canvas-element (slack-rich-text-element)
  ((file-id :initarg :file_id :type (or null string) :initform nil)
   (url :initarg :url :type (or null string) :initform nil)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-canvas-element) &optional _option)
  (let ((url (oref this url)))
    (propertize (or url "Canvas")
                'face 'slack-channel-button-face
                'slack-attachment-mention-url url
                'keymap slack-attachment-mention-keymap
                'help-echo (format "RET: open link\n%s" url))))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-canvas-element) &optional _option)
  (let ((url (oref this url)))
    (if url
        (format "[%s](%s)" url url)
      "Canvas")))

(defun slack-create-rich-text-canvas-element (payload)
  (make-instance 'slack-rich-text-canvas-element
                 :type (plist-get payload :type)
                 :file_id (plist-get payload :file_id)
                 :url (plist-get payload :url)
                 :style (slack-create-rich-text-element-style
                         (plist-get payload :style))))

(defclass slack-rich-text-citation-element (slack-rich-text-element)
  ((text :initarg :text :type (or null string) :initform nil)
   (url :initarg :url :type (or null string) :initform nil)
   (index :initarg :index :type (or null number) :initform nil)
   (details :initarg :details :type (or null list) :initform nil)))

(defvar slack-citation-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "RET") #'slack-open-citation)
    (define-key keymap [mouse-1] #'slack-open-citation)
    (define-key keymap (kbd "w") #'slack-copy-citation-url)
    keymap))

(defun slack-copy-citation-url ()
  "Copy the citation URL at point to the kill ring."
  (interactive)
  (let ((url (get-text-property (point) 'slack-message-mention-url)))
    (when url
      (kill-new url)
      (message "Copied: %s" url))))

(defun slack-open-citation ()
  "Open the citation message at point."
  (interactive)
  (let ((url (get-text-property (point) 'slack-message-mention-url))
        (channel-id (get-text-property (point) 'slack-citation-channel-id))
        (message-ts (get-text-property (point) 'slack-citation-message-ts)))
    (if (and channel-id message-ts)
        (cl-block nil
          (let* ((team (cl-find-if
                        #'(lambda (team)
                            (slack-room-find channel-id team))
                        (hash-table-values slack-teams-by-token))))
            (when team
              (let ((room (slack-room-find channel-id team)))
                (when room
                  (slack-open-message team room message-ts nil)
                  (cl-return t)))))
          (when url (browse-url url)))
      (when url (browse-url url)))))

(cl-defmethod slack-block-to-string ((this slack-rich-text-citation-element) &optional _option)
  (let ((url (oref this url))
        (text (or (oref this text) ""))
        (details (oref this details))
        (channel-id nil)
        (message-ts nil))
    (when details
      (setq channel-id (plist-get details :channel)
            message-ts (plist-get details :message_ts)))
    (propertize text
                'face 'slack-channel-button-face
                'slack-message-mention-url url
                'slack-citation-channel-id channel-id
                'slack-citation-message-ts message-ts
                'keymap slack-citation-keymap
                'help-echo (format "RET: open message\n%s" url))))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-citation-element) &optional _option)
  (let ((text (oref this text))
        (url (oref this url)))
    (if (and text url)
        (format "[%s](%s)" text url)
      (or text url ""))))

(defun slack-create-rich-text-citation-element (payload)
  (make-instance 'slack-rich-text-citation-element
                 :type (plist-get payload :type)
                 :text (plist-get payload :text)
                 :url (plist-get payload :url)
                 :index (plist-get payload :index)
                 :details (plist-get payload :details)
                 :style (slack-create-rich-text-element-style
                         (plist-get payload :style))))

(defclass slack-rich-text-team-element (slack-rich-text-element)
  ((team-id :initarg :team_id :type string)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-team-element) &optional _option)
  (let* ((team-id (oref this team-id))
         (team (slack-team-find team-id)))
    (propertize (format "%s" (slack-team-name team))
                'face 'slack-message-mention-face)))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-team-element) &optional _option)
  (slack-block-to-string this))

(defun slack-create-rich-text-team-element (payload)
  (make-instance 'slack-rich-text-team-element
                 :type (plist-get payload :type)
                 :team_id (plist-get payload :team_id)
                 :style (slack-create-rich-text-element-style
                         (plist-get payload :style))))

(defclass slack-rich-text-usergroup-element (slack-rich-text-element)
  ((usergroup-id :initarg :usergroup_id :type string)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-usergroup-element) &optional option)
  (let ((team (plist-get option :team))
        (id (oref this usergroup-id)))

    (unless team
      (error "`slack-rich-text-usergroup-element' need team as option"))

    (let ((usergroup (slack-usergroup-find id team)))
      (propertize (slack-format-usergroup usergroup)
                  'face 'slack-message-mention-keyword-face))))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-usergroup-element) &optional option)
  (let ((team (plist-get option :team))
        (id (oref this usergroup-id)))

    (unless team
      (error "`slack-rich-text-usergroup-element' need team as option"))

    (let ((usergroup (slack-usergroup-find id team)))
      (slack-propertize-mention-text 'slack-message-mention-keyword-face
                                     (slack-format-usergroup usergroup)
                                     (format "<!subteam^%s>" id)))))

(defun slack-create-rich-text-usergroup-element (payload)
  (make-instance 'slack-rich-text-usergroup-element
                 :type (plist-get payload :type)
                 :usergroup_id (plist-get payload :usergroup_id)))

(defclass slack-rich-text-date-element (slack-rich-text-element)
  ((timestamp :initarg :timestamp :type number)))

(cl-defmethod slack-block-to-string ((this slack-rich-text-date-element) &optional _option)
  (slack-format-ts (oref this timestamp)))

(defun slack-create-rich-text-date-element (payload)
  (make-instance 'slack-rich-text-date-element
                 :type (plist-get payload :type)
                 :timestamp (if (stringp (plist-get payload :timestamp))
                                (string-to-number (plist-get payload :timestamp))
                              (plist-get payload :timestamp))))

(defclass slack-rich-text-broadcast-element (slack-rich-text-element)
  ((range :initarg :range :type string) ;; channel or everyone or here
   ))

(cl-defmethod slack-block-to-string ((this slack-rich-text-broadcast-element) &optional _option)
  (propertize (format "@%s" (oref this range))
              'face 'slack-message-mention-keyword-face))

(cl-defmethod slack-block-to-mrkdwn ((this slack-rich-text-broadcast-element) &optional _option)
  (slack-propertize-mention-text 'slack-message-mention-keyword-face
                                 (format "@%s" (oref this range))
                                 (format "<!%s>" (oref this range))))

(defun slack-create-rich-text-broadcast-element (payload)
  (make-instance 'slack-rich-text-broadcast-element
                 :type (plist-get payload :type)
                 :range (plist-get payload :range)))

(provide 'slack-block-rich-text-element)
;;; slack-block-rich-text-element.el ends here
