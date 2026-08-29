;;; slack-activity-feed-buffer.el ---                -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author:  <andrea-dev@hotmail.com>
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

;; This buffer allow you to see the latest activity in slack. You can invoke it with `slack-activity-feed-show'.

;;; Code:

(require 'eieio)
(require 'cl-lib)
(require 'slack-util)
(require 'slack-buffer)
(require 'slack-search)
(require 'slack-room)
(require 'slack-message-buffer)
(require 'slack-team)
(require 'dash)
(require 's)

(declare-function slack-message-body "slack-message" (m team))
(declare-function slack-message-get-or-fetch-async "slack-message" (ts room-id team &optional thread-ts after-success))

(defvar slack-activity-feed-url "https://slack.com/api/activity.feed")
(defvar slack-activity-feed-mode-show-only-unread nil "If non-nil, show only unread activity.")

(defun slack-activity-feed-toggle-mode ()
  "Toggle whether the activity feed defaults to the `unreads' view.
This only affects the default offered by `slack-activity-feed-show' and
`slack-activity-feed-switch-view'; pick another view to override it."
  (interactive)
  (setq slack-activity-feed-mode-show-only-unread
        (not slack-activity-feed-mode-show-only-unread))
  (message (if slack-activity-feed-mode-show-only-unread
               "slack-activity-feed will default to the unreads view next time"
             "slack-activity-feed will default to the all view next time")))

(defconst slack-activity-feed-types-all
  "at_user,at_user_group,at_channel,at_everyone,keyword,list_record_assigned,list_user_mentioned,list_todo_notification,list_approval_request,list_approval_reviewed,unjoined_channel_mention,at_user,unjoined_channel_mention,at_channel,at_everyone,at_user_group,keyword,thread_v2,message_reaction,bot_dm_bundle,dm,prejoin_dm_welcome_party_alert,internal_channel_invite,external_channel_invite,external_dm_invite,quietly_added_to_channel,channel,saved_reminder,list_record_edited"
  "Activity types requested for the broad views (all/unreads/vip/...).")

(defconst slack-activity-feed-types-threads "thread_v2"
  "Activity types requested for the threads views.")

(defconst slack-activity-feed-types-mentions
  "at_user,unjoined_channel_mention,at_channel,at_everyone,at_user_group,keyword"
  "Activity types requested for the mentions view.")

(defcustom slack-activity-feed-starred-channel-section-ids nil
  "Channel section id(s) used by the `starred' activity feed view.
Set this to the id of your \"starred channels\" sidebar section (a
string) or a list of ids.  You can discover the id by inspecting the
`activity.feed' request your Slack web client sends for the starred
view.  When nil, the `starred' view behaves like `all'."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Section id")
                 (repeat :tag "Section ids" string))
  :group 'slack)

(defcustom slack-activity-feed-views
  (list
   (cons 'all          `(:types ,slack-activity-feed-types-all :mode "chrono_v1"))
   (cons 'unreads      `(:types ,slack-activity-feed-types-all :mode "chrono_v1"
                                :unread-only t))
   (cons 'vip          `(:types ,slack-activity-feed-types-all :mode "chrono_v1"
                                :priority-only t))
   (cons 'vip-unreads  `(:types ,slack-activity-feed-types-all :mode "chrono_v1"
                                :unread-only t :priority-only t))
   (cons 'threads      `(:types ,slack-activity-feed-types-threads :mode "chrono_v1"))
   (cons 'threads-unreads `(:types ,slack-activity-feed-types-threads :mode "chrono_v1"
                                  :unread-only t))
   (cons 'mentions-unreads `(:types ,slack-activity-feed-types-mentions :mode "chrono_v1"
                                   :unread-only t))
   (cons 'starred
         (lambda ()
           `(:types ,slack-activity-feed-types-all :mode "chrono_v1"
                    :channel-section-ids ,slack-activity-feed-starred-channel-section-ids))))
  "Alist mapping a view name (symbol) to its request params.
Each value is either a plist of params (`:types', `:mode', `:unread-only',
`:priority-only', `:channel-section-ids') or a function returning such a
plist (evaluated each time the view is fetched).  Add your own views
here or with `add-to-list'."
  :type '(alist :key-type symbol :value-type (sexp :tag "params plist or function"))
  :group 'slack)

(defcustom slack-activity-feed-default-view 'all
  "Default view offered by `slack-activity-feed-show' and
`slack-activity-feed-switch-view' when you just press RET."
  :type 'symbol
  :group 'slack)

(defun slack-activity-feed--view-params (view)
  "Return the params plist for VIEW.
VIEW is a symbol from `slack-activity-feed-views', or a params plist,
or a function returning a params plist."
  (let ((spec (if (symbolp view)
                  (cdr (assq view slack-activity-feed-views))
                view)))
    (cond
     ((functionp spec) (funcall spec))
     ((listp spec) spec)
     (t nil))))

(defun slack-activity-feed--fields (team params)
  "Build the alist of multipart form fields for the request from PARAMS."
  (let ((section-ids (plist-get params :channel-section-ids)))
    `(("token" . ,(or (slack-team-enterprise-token team) (slack-team-token team)))
      ("limit" . "20")
      ("types" . ,(plist-get params :types))
      ("mode" . ,(or (plist-get params :mode) "chrono_v1"))
      ("archive_only" . "false")
      ,@(when section-ids
          (list (cons "channel_section_ids" section-ids)))
      ("unread_only" . ,(if (plist-get params :unread-only) "true" "false"))
      ("priority_only" . ,(if (plist-get params :priority-only) "true" "false"))
      ("only_salesforce_channels" . "false")
      ("exclude_automations" . "false")
      ("automations_only" . "false")
      ("is_activity_inbox" . "true"))))

(defun slack-activity-feed--build-body (fields &optional cursor)
  "Build a multipart/form-data body from FIELDS (an alist of name . value).
Nil-valued fields are skipped.  A list value is joined with commas.
CURSOR is the optional pagination cursor."
  (let* ((boundary "----WebKitFormBoundaryemacsSlackActivityFeed")
         (delim (concat "--" boundary))
         (part (lambda (name value)
                 (when value
                   (let ((v (if (listp value)
                                 (mapconcat #'identity value ",")
                               value)))
                     (concat delim "\r\n"
                             "Content-Disposition: form-data; name=\""
                             name "\"\r\n\r\n" v "\r\n"))))))
    (concat
     (mapconcat (lambda (f) (or (funcall part (car f) (cdr f)) ""))
                fields "")
     (or (funcall part "cursor" cursor) "")
     (funcall part "_x_reason" "fetchActivityFeed")
     (funcall part "_x_mode" "online")
     (funcall part "_x_sonic" "true")
     (funcall part "_x_app_name" "client")
     delim "--\r\n")))

(defun slack-activity-feed--boundary ()
  "The multipart boundary string used by `slack-activity-feed-request'."
  "----WebKitFormBoundaryemacsSlackActivityFeed")

(defun slack-activity-feed--jbool (jf)
  "Return nil if JF is JSON false, t otherwise."
  (not (eq jf :json-false)))

(defun slack-activity-feed--parse-item (item-data)
  "Parse a single ITEM-DATA plist from the activity.feed API response."
  (let* ((i (plist-get item-data :item))
         (type (plist-get i :type))
         (m (plist-get i :message))
         (r (plist-get i :reaction))
         (bundle-payload (plist-get (plist-get i :bundle_info) :payload))
         (bundle-msg (plist-get bundle-payload :message))
         ;; thread_v2: thread entry with channel_id, thread_ts, latest_ts
         (thread-entry (plist-get bundle-payload :thread_entry))
         ;; dm: DM entry with latest_message containing ts and channel
         (dm-entry (plist-get (plist-get bundle-payload :dm_entry) :latest_message))
         ;; generic_system_alert: channel invite with click_target_id
         (alert-payload (plist-get i :generic_system_alert_payload))
         ;; Resolve ts and channel from whichever source is available
         (ts (or (plist-get m :ts)
                 (plist-get bundle-msg :ts)
                 (plist-get thread-entry :latest_ts)
                 (plist-get dm-entry :ts)))
         (channel (or (plist-get m :channel)
                      (plist-get bundle-msg :channel)
                      (plist-get thread-entry :channel_id)
                      (plist-get dm-entry :channel)
                      (plist-get alert-payload :click_target_id)))
         (thread-ts (or (plist-get m :thread_ts)
                        (plist-get thread-entry :thread_ts))))
    (make-instance
     'slack-activity
     :is-unread (slack-activity-feed--jbool (plist-get item-data :is_unread))
     :feed-ts (format "%s" (plist-get item-data :feed_ts))
     :item (make-instance
            'activity-item
            :type type
            :message (make-instance
                      'activity-message
                      :ts (format "%s" (or ts "0"))
                      :channel (format "%s" (or channel "unknown"))
                      :is-broadcast (slack-activity-feed--jbool
                                     (plist-get m :is_broadcast))
                      :thread-ts (when thread-ts (format "%s" thread-ts))
                      :author-id (format "%s" (plist-get m :author_user_id)))
            :reaction (when r (make-instance
                               'activity-reaction
                               :user (format "%s" (plist-get r :user))
                               :name (format "%s" (plist-get r :name))))))))

(cl-defun slack-activity-feed-request (team &key view after-success cursor)
  "Request the activity feed for TEAM using VIEW.
VIEW is a symbol from `slack-activity-feed-views' (or a params plist,
or a function returning one).  AFTER-SUCCESS is called with the
response data; CURSOR is the pagination cursor."
  (let* ((params (slack-activity-feed--view-params view))
         (fields (slack-activity-feed--fields team params))
         (body (slack-activity-feed--build-body fields cursor)))
    (cl-labels
        ((on-success (&key data &allow-other-keys)
           (slack-request-handle-error
            (data "slack-activity-feed-request")
            (when (functionp after-success)
              (funcall after-success data)))))
      (slack-request
       (slack-request-create
        slack-activity-feed-url
        team
        :type "POST"
        :success #'on-success
        :data body
        :headers (list (cons "content-type"
                             (format "multipart/form-data; boundary=%s"
                                     (slack-activity-feed--boundary)))))))))

(defclass slack-activity-feed ()
  ((activities :initarg :activities :initform nil :type (or null list))
   (pagination :initarg :pagination :type (or null string))
   (last :initarg :last :type (or null integer))
   (view :initarg :view :initform nil :type (or null symbol)
         :documentation "The `slack-activity-feed-views' name used to fetch
this feed, reused for pagination.")))
(define-derived-mode slack-activity-feed-buffer-mode slack-buffer-mode "Slack Activity Feed"
  (remove-hook 'lui-post-output-hook 'slack-display-image t))

(defclass slack-activity-feed-buffer (slack-buffer)
  ((activity-feed :initarg :activity-feed :type slack-activity-feed)))

(cl-defmethod slack-buffer-name ((_class (subclass slack-activity-feed-buffer)) team)
  (format "*slack: %s Activity Feed %s*"
          (oref team name)
          (format-time-string "%Y-%m-%d %H:%M:%S")
          ))

(cl-defmethod slack-buffer-name ((this slack-activity-feed-buffer))
  (format "*slack: %s Activity Feed %s*"
          (slack-team-name (slack-buffer-team this))
          (format-time-string "%Y-%m-%d %H:%M:%S")
          ))

(cl-defmethod slack-buffer-key ((_class (subclass slack-activity-feed-buffer)))
  "activity feed")

(cl-defmethod slack-buffer-key ((this slack-activity-feed-buffer))
  (slack-buffer-key 'slack-activity-feed-buffer))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-activity-feed-buffer)))
  'slack-activity-feed-buffer)

(defun slack-create-activity-feed-buffer (activity-feed team)
  (let ((buffer (slack-buffer-find 'slack-activity-feed-buffer team)))
    (when buffer (kill-buffer (oref buffer buf)))
    (make-instance 'slack-activity-feed-buffer
                   :team-id (oref team id)
                   :activity-feed activity-feed)))

(defclass activity-message ()
  ((ts :initarg :ts :type string)
   (channel :initarg :channel :type string)
   (is-broadcast :initarg :is-broadcast :type boolean)
   (thread-ts :initarg :thread-ts :type (or null string))
   (author-id :initarg :author-id :type (or null string))))

(cl-defmethod slack-activity-message-to-string ((this activity-message) team &optional activity-type)
  "Format THIS activity-message of TEAM as a string for presentation.
ACTIVITY-TYPE is the activity type string (e.g. \"thread_reply\")."
  (with-slots (channel ts is-broadcast thread-ts author-id) this
    (condition-case err ;; this is to find out more easily messages that we fail to handle
        (let* ((room (slack-room-find channel team))
               (room-name (or (ignore-errors (slack-room-name room team))
                              "name not available - try to update channel list"))
               (location (format "%s%s"
                                 (if (slack-channel-p room) "#" "@")
                                 room-name))
               (type-prefix (pcase activity-type
                              ((or "thread_reply" "thread_v2") "Thread in ")
                              (_ "")))
               (header (propertize (concat type-prefix location)
                                   'face 'slack-search-result-message-header-face)))
          (propertize (concat header
                              (when-let ((author (slack-user-name author-id team)))
                                (format " from %s" author))
                              "\n"
                              (or
                               (condition-case msg-err
                                   (when (and (or ts thread-ts) room)
                                     (let ((msg (slack-room-find-message room ts)))
                                       (when msg
                                         (slack-message-body msg team))))
                                 (error
                                  (message "slack-activity-message-to-string: rendering message failed with: %S"
                                           (error-message-string msg-err))
                                  nil))
                               (if (or ts thread-ts)
                                   (propertize "(loading message...)"
                                               'activity-pending ts)
                                 "TODO")))
                      'ts ts
                      'team-id (oref team id)
                      'room-id (oref room id)
                      'thread-ts thread-ts))
      (error
       (format "TODO there was an error, please report this message at https://github.com/emacs-slack/emacs-slack/issues:\n%s"
               (list this err))))))

(defclass activity-reaction ()
  ((user :initarg :user :type string)
   (name :initarg :name :type string)))

(cl-defmethod slack-activity-reaction-to-string ((this activity-reaction) team)
  (with-slots (user name) this
    (format "  %s reacted with :%s:"
            (slack-user-name user team)
            name
            )))

(defclass activity-item ()
  ((type :initarg :type :type string)
   (message :initarg :message :type activity-message)
   (reaction :initarg :reaction :type (or null activity-reaction))))

(cl-defmethod slack-activity-item-to-string ((this activity-item) team)
  "Convert THIS activity for TEAM into a string."
  (with-slots (type message reaction) this
    (if (equal type "bot_dm_bundle") ;; this bot message seem to have no valuable information
        ""
      (concat
       (slack-activity-message-to-string message team type)
       (when reaction (concat "\n" (slack-activity-reaction-to-string reaction team)))))))

(defclass slack-activity ()
  ((is-unread :initarg :is-unread :type boolean)
   (feed-ts :initarg :feed-ts :type string)
   (item :initarg :item :type activity-item)))

(cl-defmethod slack-activity-to-string ((this slack-activity) team)
  (with-slots (is-unread item) this
    (format "%s %s" (if is-unread "*" " ") (slack-activity-item-to-string item team))))

(cl-defmethod slack-buffer-insert ((this slack-activity-feed-buffer) activity)
  (let* ((team (slack-buffer-team this))
         (time (slack-ts-to-time (oref activity feed-ts)))
         (lui-time-stamp-time time)
         (lui-time-stamp-format "[%Y-%m-%d %H:%M] "))
    (lui-insert (slack-activity-to-string activity team) t)
    (lui-insert "" t)
    (slack-activity-feed--maybe-fetch-body this activity)))

(defun slack-activity-feed--maybe-fetch-body (buffer activity)
  "Dispatch a non-blocking fetch for ACTIVITY's message body if not cached.
The activity feed renders a placeholder for messages not in the local
cache; this fetches them in the background and fills the placeholder in
once the body is available, so opening the feed never blocks."
  (let* ((team (slack-buffer-team buffer))
         (msg (oref (oref activity item) message))
         (live-buf (slack-buffer-buffer buffer)))
    (with-slots (ts channel thread-ts) msg
      (let ((room (slack-room-find channel team)))
        (unless (and room (slack-room-find-message room ts))
          (slack-message-get-or-fetch-async
           ts channel team thread-ts
           (lambda (fetched)
             (when (buffer-live-p live-buf)
               (let ((body (if fetched
                                (slack-message-body fetched team)
                              "(message unavailable)")))
                 (slack-activity-feed--replace-placeholder
                  live-buf msg team body))))))))))

(defun slack-activity-feed--replace-placeholder (buffer activity-message team body)
  "Replace the loading placeholder for ACTIVITY-MESSAGE in BUFFER with BODY."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((ts (oref activity-message ts))
             (inhibit-read-only t)
             (pos (text-property-any (point-min) (point-max)
                                     'activity-pending ts)))
        (when pos
          (let ((end (or (next-single-property-change pos 'activity-pending)
                         (point-max))))
            (save-excursion
              (goto-char pos)
              (delete-region pos end)
              (insert (propertize body
                                  'ts ts
                                  'team-id (oref team id)
                                  'room-id (oref activity-message channel)
                                  'thread-ts (oref activity-message thread-ts))))))))))

(cl-defmethod slack-buffer-has-next-page-p ((this slack-activity-feed-buffer))
  "Tell if there is another page of results for THIS SLACK-ACTIVITY-FEED-BUFFER."
  (with-slots (activity-feed) this
    (oref activity-feed pagination)))

(cl-defmethod slack-buffer-insert-history ((this slack-activity-feed-buffer))
  (with-slots (activity-feed) this
    (let* ((cur-point (point))
           (activities (-drop (oref activity-feed last) (oref activity-feed activities))))
      (cl-loop for m in activities
               do (slack-buffer-insert this m))
      (goto-char cur-point))
    ))

(cl-defmethod slack-buffer-request-history ((this slack-activity-feed-buffer) after-success)
  (with-slots (activity-feed) this
    (slack-activity-feed-request
     (slack-buffer-team this)
     :view (oref activity-feed view)
     :after-success
     (lambda (data)
       (let ((new-activity-feed
              (make-instance
               'slack-activity-feed
               :view (oref activity-feed view)
               :activities
               (append
                (oref activity-feed activities)
                (mapcar #'slack-activity-feed--parse-item
                        (plist-get data :items)))
               :pagination (plist-get (plist-get data :response_metadata)
                                      :next_cursor)
               :last (- (length (oref activity-feed activities)) 1))))
         (oset this activity-feed new-activity-feed)
         (funcall after-success)))
     :cursor (oref activity-feed pagination))))

(cl-defmethod slack-buffer-init-buffer ((this slack-activity-feed-buffer))
  (let ((buffer (cl-call-next-method)))
    (with-current-buffer buffer
      (slack-activity-feed-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (with-slots (activity-feed) this
        (let* ((activities (oref activity-feed activities)))
          (cl-loop for m in activities
                   do (slack-buffer-insert this m)))
        (let ((lui-time-stamp-position nil))
          (if (slack-buffer-has-next-page-p this)
              (slack-buffer-insert-load-more this)))))
    buffer))

(cl-defmethod slack-buffer-loading-message-end-point ((_this slack-activity-feed-buffer))
  (previous-single-property-change (point-max)
                                   'loading-message))

(cl-defmethod slack-buffer-delete-load-more-string ((this slack-activity-feed-buffer))
  (let* ((inhibit-read-only t)
         (loading-message-end
          (slack-buffer-loading-message-end-point this))
         (loading-message-start
          (previous-single-property-change loading-message-end
                                           'loading-message)))
    (delete-region loading-message-start
                   loading-message-end)))

(cl-defmethod slack-buffer-prepare-marker-for-history ((_this slack-activity-feed-buffer)))

(cl-defmethod slack-buffer-insert--history ((this slack-activity-feed-buffer))
  (slack-buffer-insert-history this)
  (if (slack-buffer-has-next-page-p this)
      (slack-buffer-insert-load-more this)
    (let ((lui-time-stamp-position nil))
      (lui-insert "(no more messages)\n" t))))

(defun slack-activity-feed--read-view ()
  "Prompt for an activity feed view, returning the chosen symbol."
  (let* ((names (mapcar #'car slack-activity-feed-views))
         (default (if slack-activity-feed-mode-show-only-unread
                     'unreads
                   slack-activity-feed-default-view))
         (choice (funcall slack-completing-read-function
                         "Activity feed view: " names nil t nil nil
                         (symbol-name default))))
  (intern choice)))

(defun slack-activity-feed--fetch-and-display (team view)
  "Fetch the activity feed for TEAM with VIEW and display its buffer."
  (slack-activity-feed-request
   team
   :view view
   :after-success
   (lambda (data)
     (let* ((activity-feed
             (make-instance
              'slack-activity-feed
              :view view
              :activities (mapcar #'slack-activity-feed--parse-item
                                   (plist-get data :items))
              :pagination (plist-get (plist-get data :response_metadata)
                                     :next_cursor)))
            (buffer (slack-create-activity-feed-buffer activity-feed team)))
       (slack-buffer-display buffer)))))

;;;###autoload
(defun slack-activity-feed-show (&optional view)
  "Show the Slack activity feed for VIEW.
Interactively, prompt for a view from `slack-activity-feed-views'
(defaulting to `slack-activity-feed-default-view', or `unreads' when
`slack-activity-feed-mode-show-only-unread' is non-nil)."
  (interactive (list (slack-activity-feed--read-view)))
  (slack-activity-feed--fetch-and-display (slack-team-select) view))

(defun slack-activity-feed-switch-view (&optional view)
  "Re-fetch the current activity feed buffer for VIEW.
Interactively, prompt for a view.  Convenient to filter between views
(e.g. `unreads', `vip', `threads', `mentions-unreads') without leaving
the activity feed."
  (interactive (list (slack-activity-feed--read-view)))
  (let ((team (if (bound-and-true-p slack-current-buffer)
                  (slack-buffer-team slack-current-buffer)
                (slack-team-select))))
    (slack-activity-feed--fetch-and-display team view)))

(defun slack-activity-feed-open-message ()
  "Open message at point of activity-feed."
  (interactive)
  (if-let* ((ts (get-text-property (point) 'ts))
            (team-id (get-text-property (point) 'team-id))
            (room-id (get-text-property (point) 'room-id))
            (team (slack-team-find team-id)))
      (let ((thread-ts (get-text-property (point) 'thread-ts)))
        (slack-open-message
         team
         (slack-room-find room-id team)
         ts
         thread-ts))
    (error "Not possible to jump to message")))
(define-key slack-activity-feed-buffer-mode-map (kbd "RET") 'slack-activity-feed-open-message)
(define-key slack-activity-feed-buffer-mode-map (kbd "v") 'slack-activity-feed-switch-view)

(provide 'slack-activity-feed-buffer)
;;; slack-activity-feed-buffer.el ends here
