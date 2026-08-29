;;; slack-activity-feed-test.el --- tests for activity feed views -*- lexical-binding: t; -*-

(require 'ert)
(require 'slack-activity-feed-buffer)
(require 'slack-message)

(defun slack-activity-feed-test--team ()
  "A minimal team with a token for field-building tests."
  (make-instance 'slack-team :token "xox-test-token"))

(ert-deftest slack-activity-feed-test-views-present ()
  "All the documented views are registered in `slack-activity-feed-views'."
  (let ((keys (mapcar #'car slack-activity-feed-views)))
    (dolist (view '(all unreads vip vip-unreads
                       threads threads-unreads
                       mentions-unreads starred))
      (should (member view keys)))))

(ert-deftest slack-activity-feed-test-view-params-symbol ()
  "`slack-activity-feed--view-params' resolves a view symbol to its plist."
  (let ((all (slack-activity-feed--view-params 'all)))
    (should (equal slack-activity-feed-types-all (plist-get all :types)))
    (should (equal "chrono_v1" (plist-get all :mode))))
  (should (eq t (plist-get (slack-activity-feed--view-params 'unreads) :unread-only)))
  (should (eq t (plist-get (slack-activity-feed--view-params 'vip) :priority-only)))
  (should (eq t (plist-get (slack-activity-feed--view-params 'vip-unreads) :unread-only)))
  (should (eq t (plist-get (slack-activity-feed--view-params 'vip-unreads) :priority-only)))
  (should (equal slack-activity-feed-types-threads
                 (plist-get (slack-activity-feed--view-params 'threads) :types)))
  (should (eq t (plist-get (slack-activity-feed--view-params 'threads-unreads) :unread-only)))
  (should (equal slack-activity-feed-types-mentions
                 (plist-get (slack-activity-feed--view-params 'mentions-unreads) :types))))

(ert-deftest slack-activity-feed-test-view-params-starred-function ()
  "The starred view is a function returning a plist with section ids."
  (let ((params (slack-activity-feed--view-params 'starred)))
    (should (equal slack-activity-feed-types-all (plist-get params :types)))
    (should (member :channel-section-ids params))))

(ert-deftest slack-activity-feed-test-view-params-passes-plist ()
  "A raw plist (not a symbol) is returned unchanged."
  (let ((plist `(:types ,slack-activity-feed-types-threads :mode "chrono_v1"
                        :unread-only t)))
    (should (equal plist (slack-activity-feed--view-params plist)))))

(ert-deftest slack-activity-feed-test-fields-unread-and-priority ()
  "`--fields' translates the params flags into the request form values."
  (let ((team (slack-activity-feed-test--team)))
    (let ((fields (slack-activity-feed--fields
                   team
                   `(:types ,slack-activity-feed-types-all :mode "chrono_v1"
                            :unread-only t :priority-only t))))
      (should (equal "xox-test-token" (cdr (assoc "token" fields))))
      (should (equal "true" (cdr (assoc "unread_only" fields))))
      (should (equal "true" (cdr (assoc "priority_only" fields))))
      (should (null (assoc "channel_section_ids" fields))))
    (let ((fields (slack-activity-feed--fields
                   team
                   `(:types ,slack-activity-feed-types-all :mode "chrono_v1"))))
      (should (equal "false" (cdr (assoc "unread_only" fields))))
      (should (equal "false" (cdr (assoc "priority_only" fields)))))))

(ert-deftest slack-activity-feed-test-fields-section-ids ()
  "Channel section ids become a `channel_section_ids' field when present."
  (let* ((team (slack-activity-feed-test--team))
         (fields (slack-activity-feed--fields
                  team
                  `(:types ,slack-activity-feed-types-all :mode "chrono_v1"
                           :channel-section-ids "S12345")))
         (entry (assoc "channel_section_ids" fields)))
    (should entry)
    (should (equal "S12345" (cdr entry)))))

(ert-deftest slack-activity-feed-test-build-body-contains-fields ()
  "`--build-body' emits a multipart body with the field name and value."
  (let* ((team (slack-activity-feed-test--team))
         (fields (slack-activity-feed--fields
                  team
                  `(:types ,slack-activity-feed-types-threads :mode "chrono_v1"
                           :unread-only t)))
         (body (slack-activity-feed--build-body fields)))
    (should (string-match-p
             (regexp-quote (format "name=\"types\"\r\n\r\n%s"
                                   slack-activity-feed-types-threads))
             body))
    (should (string-match-p "name=\"unread_only\"\r\n\r\ntrue" body))
    (should (string-match-p "name=\"token\"\r\n\r\nxox-test-token" body))
    ;; the body terminates with the closing boundary
    (should (string-match-p
             (regexp-quote
              (concat "--" (slack-activity-feed--boundary) "--\r\n"))
             body))))

(ert-deftest slack-activity-feed-test-build-body-includes-cursor ()
  "A pagination cursor is emitted as its own multipart part."
  (let* ((team (slack-activity-feed-test--team))
         (fields (slack-activity-feed--fields
                  team
                  `(:types ,slack-activity-feed-types-all :mode "chrono_v1")))
         (body (slack-activity-feed--build-body fields "CUR123")))
    (should (string-match-p "name=\"cursor\"\r\n\r\nCUR123" body))))

(ert-deftest slack-activity-feed-test-build-body-skips-nil ()
  "Nil-valued fields are omitted from the body."
  (let ((body (slack-activity-feed--build-body
               `(("present" . "yes") ("absent" . nil)))))
    (should (string-match-p "name=\"present\"\r\n\r\nyes" body))
    (should (not (string-match-p "name=\"absent\"" body)))))

(ert-deftest slack-activity-feed-test-render-uncached-no-block ()
  "An activity whose message is not in the local cache renders a
placeholder instead of calling the blocking `slack-message-get-or-fetch'."
  (slack-test-setup
    (oset team id "T00001")
    (let* ((am (make-instance 'activity-message
                              :ts "1600000001.000000"
                              :channel channel-id
                              :is-broadcast nil
                              :thread-ts nil
                              :author-id "U0"))
           (called nil))
      (cl-letf (((symbol-function 'slack-message-get-or-fetch)
                 (lambda (&rest _) (setq called t) nil)))
        (let ((rendered (slack-activity-message-to-string am team "thread_v2")))
          (should (string-match-p "loading message" rendered))
          (should (string-match-p (format "#%s" channel-name) rendered))))
      (should (null called)))))

(ert-deftest slack-activity-feed-test-render-cached-body ()
  "An activity whose message is already cached renders its body locally
without fetching from the network."
  (slack-test-setup
    (oset team id "T00001")
    (let* ((ts "1600000002.000000")
           (msg (make-instance 'slack-message :ts ts :text "hello body"
                               :channel channel-id))
           (am (make-instance 'activity-message
                              :ts ts
                              :channel channel-id
                              :is-broadcast nil
                              :thread-ts nil
                              :author-id "U0"))
           (called nil))
      (puthash ts msg (oref channel messages))
      (cl-letf (((symbol-function 'slack-message-get-or-fetch)
                 (lambda (&rest _) (setq called t) nil)))
        (let ((rendered (slack-activity-message-to-string am team "thread_v2")))
          (should (string-match-p "hello body" rendered))
          (should (not (string-match-p "loading message" rendered)))))
      (should (null called)))))

(ert-deftest slack-activity-feed-test-replace-placeholder ()
  "`slack-activity-feed--replace-placeholder' swaps the loading placeholder
for the fetched body, carrying the ts text property so RET still works."
  (let* ((ts "1600000003.000000")
         (am (make-instance 'activity-message
                            :ts ts
                            :channel "C999"
                            :is-broadcast nil
                            :thread-ts nil
                            :author-id "U0"))
         (team (make-instance 'slack-team :id "T00001" :token "xox"))
         (buf (generate-new-buffer " *test-activity-feed*")))
    (with-current-buffer buf
      (insert (propertize (concat "Thread in #chan\n"
                                  (propertize "(loading message...)"
                                              'activity-pending ts))
                          'ts ts 'team-id "T00001" 'room-id "C999")))
    (slack-activity-feed--replace-placeholder buf am team "the real body")
    (with-current-buffer buf
      (should (string-match-p "the real body" (buffer-string)))
      (should (not (string-match-p "loading message" (buffer-string))))
      (should (string= ts (get-text-property
                           (text-property-any (point-min) (point-max) 'ts ts)
                           'ts))))
    (kill-buffer buf)))

(ert-deftest slack-activity-feed-test-maybe-fetch-body-dispatches-async ()
  "`slack-activity-feed--maybe-fetch-body' dispatches the non-blocking
fetch for an uncached message and fills the placeholder on completion."
  (slack-test-setup
    (oset team id "T00001")
    (let* ((ts "1600000004.000000")
           (fetched (make-instance 'slack-message :ts ts :text "async body"
                                    :channel channel-id))
           (am (make-instance 'activity-message
                              :ts ts
                              :channel channel-id
                              :is-broadcast nil
                              :thread-ts nil
                              :author-id "U0"))
           (activity (make-instance 'slack-activity
                                    :is-unread t
                                    :feed-ts ts
                                    :item (make-instance 'activity-item
                                                         :type "thread_v2"
                                                         :message am)))
           (callback nil)
           (buf (generate-new-buffer " *test-activity-feed*")))
      (with-current-buffer buf
        (insert (propertize (concat "#chan\n"
                                    (propertize "(loading message...)"
                                                'activity-pending ts))
                            'ts ts 'team-id "T00001" 'room-id channel-id)))
      ;; Stub buffer/team resolution and the async fetch to avoid the network.
      (cl-letf (((symbol-function 'slack-buffer-team) (lambda (_) team))
                ((symbol-function 'slack-buffer-buffer) (lambda (_) buf))
                ((symbol-function 'slack-message-get-or-fetch-async)
                 (lambda (_ts _room _team _thread-ts cb)
                   (setq callback cb))))
        (slack-activity-feed--maybe-fetch-body nil activity))
      (should callback)
      ;; Simulate the async fetch completing with the cached message.
      (puthash ts fetched (oref channel messages))
      (funcall callback fetched)
      (with-current-buffer buf
        (should (string-match-p "async body" (buffer-string)))
        (should (not (string-match-p "loading message" (buffer-string)))))
      (kill-buffer buf))))

;;; slack-activity-feed-test.el ends here
