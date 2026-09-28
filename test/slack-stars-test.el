;;; slack-stars-test.el --- tests for the saved-for-later buffer -*- lexical-binding: t; -*-

;; Network paths are covered by stubbing `slack-message-get-or-fetch-async'
;; and `slack-message-to-string', never by hitting the API.

(require 'ert)
(require 'slack-stars-buffer)
(require 'slack-star)
(require 'slack-message)

(defun slack-stars-test--star-item (ts room-id &optional item-type)
  "A saved item for the message with TS in ROOM-ID."
  (make-instance 'slack-star-item :item-id room-id
                 :item-type (or item-type "message")
                 :ts ts))

(defun slack-stars-test--insert-dummy-line ()
  "Insert a filler message so tested messages are not the first one.

`lui-replace' never replaces the message that starts at
`point-min'; real stars buffers always have the (load more) line
above the messages."
  (let ((lui-time-stamp-position nil))
    (lui-insert "dummy\n" t)))

(ert-deftest slack-stars-test-load-items-fills-placeholders-async ()
  "Opening the stars buffer renders one placeholder per saved message and
fills them through the non-blocking fetch, never a blocking API call."
  (slack-test-setup
    (oset team id "T00001")
    (oset team star
          (make-instance 'slack-star
                         :items (list (slack-stars-test--star-item
                                       (slack-test-ts 2) channel-id)
                                      (slack-stars-test--star-item
                                       (slack-test-ts 3) channel-id))))
    (let* ((fetches nil)
           (buffer (generate-new-buffer " *test-stars*"))
           (stars (slack-stars-buffer :team-id (oref team id))))
      (with-current-buffer buffer
        (lui-mode)
        (lui-set-prompt "test> ")
        (slack-stars-test--insert-dummy-line))
      (cl-letf (((symbol-function 'slack-buffer-team) (lambda (_) team))
                ((symbol-function 'slack-buffer-buffer) (lambda (_) buffer))
                ((symbol-function 'slack-message-get-or-fetch-async)
                 (lambda (ts room-id _team _thread-ts after-success)
                   (should (string= room-id channel-id))
                   (push (cons ts after-success) fetches)))
                ((symbol-function 'slack-message-to-string)
                 (lambda (message _team)
                   (format "rendered: %s\n" (oref message text)))))
        (slack-stars-buffer--load-items stars (slack-star-items (oref team star)))
        ;; one non-blocking fetch per saved item, placeholders visible meanwhile
        (should (equal 2 (length fetches)))
        (with-current-buffer buffer
          (should (equal 2 (count-matches "loading message"
                                          (point-min) (point-max))))
          (should (slack-buffer-ts-eq (point-min) (point-max)
                                     (slack-test-ts 2))))
        ;; deliver the fetched messages: each placeholder is replaced in place
        (dolist (entry (reverse fetches))
          (funcall (cdr entry)
                   (make-instance 'slack-message
                                  :ts (car entry)
                                  :channel channel-id
                                  :text (format "body-%s" (car entry)))))
        (with-current-buffer buffer
          (should (string-match-p (regexp-quote
                                   (format "rendered: body-%s" (slack-test-ts 2)))
                                  (buffer-string)))
          (should (not (string-match-p "loading message" (buffer-string))))
          ;; the ts text property survives the replacement, so RET still works
          (should (slack-buffer-ts-eq (point-min) (point-max)
                                     (slack-test-ts 2))))
      ;; the oldest rendered boundary is the oldest saved item
      (should (string= (slack-test-ts 2) (oref stars oldest)))
      (kill-buffer buffer)))))

(ert-deftest slack-stars-test-load-items-real-fetch-chain ()
  "Placeholders fill through the real `slack-message-get-or-fetch-async':
room lookup, conversations.history dispatch, room push and the fill
callback.  Only the HTTP layer is stubbed."
  (slack-test-with-registered-team (team channel)
    (let* ((ts (slack-test-ts 5))
           (buffer (generate-new-buffer " *test-stars*"))
           (stars (slack-stars-buffer :team-id (oref team id))))
      (oset stars buf buffer)
      (with-current-buffer buffer
        (lui-mode)
        (lui-set-prompt "test> ")
        (slack-stars-test--insert-dummy-line))
      (cl-letf (((symbol-function 'slack-conversations-history)
                 (lambda (room team &rest args)
                   (funcall (plist-get args :after-success)
                            (list (slack-test-message team room ts
                                                      "fetched body"))
                            "" t)))
                ((symbol-function 'slack-message-to-string)
                 (lambda (message _team)
                   (format "rendered: %s\n" (oref message text)))))
        (slack-stars-buffer--load-items
         stars (list (slack-stars-test--star-item ts (oref channel id)))))
      (with-current-buffer buffer
        (should (string-match-p "rendered: fetched body" (buffer-string)))
        (should (not (string-match-p "loading message" (buffer-string))))
        (should (slack-buffer-ts-eq (point-min) (point-max) ts)))
      (kill-buffer buffer))))

(ert-deftest slack-stars-test-load-items-thread-reply-resolves ()
  "A saved message that only exists as a thread reply still fills its
placeholder: channel history answers with the nearest older message,
the fetch falls back to `conversations.replies' anchored at the saved
timestamp, and the placeholder is matched by that timestamp, not the
fetched message's own."
  (slack-test-with-registered-team (team channel)
    (let* ((reply-ts (slack-test-ts 6))
           (root-ts (slack-test-ts 5))
           (buffer (generate-new-buffer " *test-stars*"))
           (stars (slack-stars-buffer :team-id (oref team id))))
      (oset stars buf buffer)
      (with-current-buffer buffer
        (lui-mode)
        (lui-set-prompt "test> ")
        (slack-stars-test--insert-dummy-line))
      (cl-letf (((symbol-function 'slack-conversations-history)
                 (lambda (room _team &rest args)
                   ;; nearest older channel message: the thread root
                   (funcall (plist-get args :after-success)
                            (list (slack-test-message team room root-ts
                                                      "the root" root-ts))
                            "" t)))
                ((symbol-function 'slack-conversations-replies)
                 (lambda (_room ts _team &rest args)
                   (should (string= reply-ts ts))
                   (funcall (plist-get args :after-success)
                            (list (slack-test-message team channel reply-ts
                                                      "the reply" root-ts))
                            nil nil)))
                ((symbol-function 'slack-message-to-string)
                 (lambda (message _team)
                   (format "rendered: %s\n" (oref message text)))))
        (slack-stars-buffer--load-items
         stars (list (slack-stars-test--star-item reply-ts
                                                  (oref channel id)))))
      (with-current-buffer buffer
        (should (string-match-p "rendered: the reply" (buffer-string)))
        (should (not (string-match-p "loading message" (buffer-string))))
        (should (slack-buffer-ts-eq (point-min) (point-max) reply-ts)))
      (kill-buffer buffer))))

(ert-deftest slack-stars-test-fill-placeholder-unavailable ()
  "A fetch that finds nothing replaces the placeholder with the
unavailable notice instead of waiting forever."
  (slack-test-setup
    (oset team id "T00001")
    (let* ((ts (slack-test-ts 2))
           (buffer (generate-new-buffer " *test-stars*"))
           (stars (slack-stars-buffer :team-id (oref team id))))
      (with-current-buffer buffer
        (lui-mode)
        (lui-set-prompt "test> ")
        (slack-stars-test--insert-dummy-line))
      (cl-letf (((symbol-function 'slack-buffer-team) (lambda (_) team))
                ((symbol-function 'slack-buffer-buffer) (lambda (_) buffer)))
        (slack-stars-buffer--insert-placeholder stars ts channel-id)
        (with-current-buffer buffer
          (should (string-match-p "loading message" (buffer-string))))
        (slack-stars-buffer--fill-placeholder stars ts nil)
        (with-current-buffer buffer
          (should (string-match-p "message unavailable" (buffer-string)))
          (should (not (string-match-p "loading message" (buffer-string))))
          (should (slack-buffer-ts-eq (point-min) (point-max) ts))))
      (kill-buffer buffer))))

(ert-deftest slack-stars-test-insert-history-loads-older-only ()
  "Loading the next page only renders messages below the oldest rendered
boundary, and moves the boundary down to the oldest saved item."
  (slack-test-setup
    (oset team id "T00001")
    (oset team star
          (make-instance 'slack-star
                         :items (list (slack-stars-test--star-item
                                       (slack-test-ts 1) channel-id)
                                      (slack-stars-test--star-item
                                       (slack-test-ts 2) channel-id)
                                      (slack-stars-test--star-item
                                       (slack-test-ts 3) channel-id))))
    (let* ((inserted nil)
           (stars (slack-stars-buffer :team-id (oref team id))))
      (oset stars oldest (slack-test-ts 2))
      (cl-letf (((symbol-function 'slack-buffer-team) (lambda (_) team))
                ((symbol-function 'slack-buffer-buffer)
                 (lambda (_) (current-buffer)))
                ((symbol-function 'slack-message-get-or-fetch-async)
                 (lambda (ts _room-id _team _thread-ts after-success)
                   (funcall after-success
                            (make-instance 'slack-message
                                           :ts ts
                                           :channel channel-id
                                           :text "x"))))
                ((symbol-function 'slack-buffer-insert)
                 (lambda (_this message &optional _not-tracked-p)
                   (push (slack-ts message) inserted)))
                ((symbol-function 'slack-buffer-ts-eq) (lambda (&rest _) nil)))
        (slack-buffer-insert-history stars))
      (should (equal (list (slack-test-ts 1)) inserted))
      (should (string= (slack-test-ts 1) (oref stars oldest))))))

(ert-deftest slack-stars-test-update-oldest-never-fetches ()
  "The oldest boundary is taken from the star item's own ts, so setting it
does not need any message fetch."
  (slack-test-setup
    (oset team id "T00001")
    (let* ((stars (slack-stars-buffer :team-id (oref team id)))
           (fetch-called nil))
      (should (string= "" (oref stars oldest)))
      (cl-letf (((symbol-function 'slack-message-get-or-fetch-async)
                 (lambda (&rest _) (setq fetch-called t) nil)))
        (slack-buffer-update-oldest
         stars (slack-stars-test--star-item (slack-test-ts 1) channel-id))
        (slack-buffer-update-oldest
         stars (slack-stars-test--star-item (slack-test-ts 3) channel-id))
        ;; an older item does not move the boundary back
        (slack-buffer-update-oldest
         stars (slack-stars-test--star-item (slack-test-ts 1) channel-id)))
      (should (string= (slack-test-ts 3) (oref stars oldest)))
      (should (null fetch-called)))))

(ert-deftest slack-stars-test-replace-fetches-async-and-rerenders ()
  "`slack-buffer--replace' re-renders a saved message through the
non-blocking fetch and keeps the ts text property."
  (slack-test-setup
    (oset team id "T00001")
    (oset team star
          (make-instance 'slack-star
                         :items (list (slack-stars-test--star-item
                                       (slack-test-ts 2) channel-id))))
    (let* ((callback nil)
           (buffer (generate-new-buffer " *test-stars*"))
           (stars (slack-stars-buffer :team-id (oref team id))))
      (with-current-buffer buffer
        (lui-mode)
        (lui-set-prompt "test> ")
        (slack-stars-test--insert-dummy-line)
        (let ((lui-time-stamp-position nil))
          (lui-insert-with-text-properties "old rendering\n"
                                           'ts (slack-test-ts 2)
                                           'team-id (oref team id)
                                           'room-id channel-id)))
      (cl-letf (((symbol-function 'slack-buffer-team) (lambda (_) team))
                ((symbol-function 'slack-buffer-buffer) (lambda (_) buffer))
                ((symbol-function 'slack-message-get-or-fetch-async)
                 (lambda (_ts _room-id _team _thread-ts after-success)
                   (setq callback after-success)))
                ((symbol-function 'slack-message-to-string)
                 (lambda (message _team)
                   (format "new rendering: %s\n" (oref message text)))))
        (slack-buffer--replace stars (slack-test-ts 2))
        (should callback)
        (funcall callback (make-instance 'slack-message
                                         :ts (slack-test-ts 2)
                                         :channel channel-id
                                         :text "updated"))
        (with-current-buffer buffer
          (should (string-match-p "new rendering: updated" (buffer-string)))
          (should (not (string-match-p "old rendering" (buffer-string))))
          (should (slack-buffer-ts-eq (point-min) (point-max)
                                     (slack-test-ts 2)))))
      (kill-buffer buffer))))

(ert-deftest slack-stars-test-api-request-logs-and-notifies-on-success ()
  "A successful stars API request logs an info message and runs the
after-success callback, so the user knows the change landed."
  (slack-test-setup
    (oset team id "T00001")
    (let ((logged nil)
          (ran nil))
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (req)
                   ;; only the HTTP layer is stubbed: the request object
                   ;; still goes through `slack-request-create'
                   (funcall (oref req success) :data '(:ok t))))
                ((symbol-function 'slack-log)
                 (lambda (msg _team &rest rest)
                   (push (list msg (plist-get rest :level)) logged))))
        (slack-star-api-request "https://slack.com/api/saved.add" nil
                                team
                                "Successfully starred message."
                                (lambda () (push :ran ran))))
      (should (equal (list (list "Successfully starred message." 'info))
                     logged))
      (should (equal '(:ran) ran)))))

(ert-deftest slack-stars-test-api-request-silent-on-error ()
  "A failed stars API request logs nothing and runs no callback."
  (slack-test-setup
    (oset team id "T00001")
    (let ((logged nil)
          (ran nil))
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (req)
                   (funcall (oref req success)
                            :data '(:ok :json-false :error "not_allowed"))))
                ((symbol-function 'slack-log)
                 (lambda (&rest _) (push :logged logged))))
        (slack-star-api-request "https://slack.com/api/saved.add" nil
                                team
                                "Successfully starred message."
                                (lambda () (push :ran ran))))
      (should (null logged))
      (should (null ran)))))

(ert-deftest slack-stars-test-api-request-message-updates-and-rerenders ()
  "Starring a message flips its saved state and redraws its buffers;
unstarring clears it again.  Both log what happened at info level."
  (slack-test-setup
    (oset team id "T00001")
    (let* ((ts (slack-test-ts 1))
           (message (make-instance 'slack-message :ts ts
                                              :channel channel-id
                                              :text "hi"))
           (replaced nil)
           (logged nil))
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (req)
                   (funcall (oref req success) :data '(:ok t))))
                ((symbol-function 'slack-log)
                 (lambda (msg _team &rest rest)
                   (push (list msg (plist-get rest :level)) logged)))
                ((symbol-function 'slack-message-replace-buffer)
                 (lambda (m _team) (push (slack-ts m) replaced))))
        (slack-star-api-request-message "https://slack.com/api/saved.add"
                                        nil team message t)
        (should (slack-message-starred-p message))
        (should (equal (list ts) replaced))
        (slack-star-api-request-message "https://slack.com/api/saved.delete"
                                        nil team message nil)
        (should (not (slack-message-starred-p message)))
        (should (equal (list ts ts) replaced))
        (should (equal (list (list "Successfully unstarred message." 'info)
                             (list "Successfully starred message." 'info))
                       logged))))))

(ert-deftest slack-stars-test-room-buffer-add-star-requests-and-updates ()
  "Starring from a room buffer sends saved.add with the channel and the
message timestamp, and flips the message's saved state on success."
  (slack-test-with-registered-team (team channel)
    (let* ((ts (slack-test-ts 1))
           (message (slack-test-message team channel ts "star me"))
           (requests nil)
           (replaced nil))
      (slack-room-set-messages channel (list message) team)
      (cl-letf (((symbol-function 'slack-request)
                 (lambda (req)
                   (push (list (oref req url) (oref req params)) requests)
                   (funcall (oref req success) :data '(:ok t))))
                ((symbol-function 'slack-log)
                 (lambda (&rest _) nil))
                ((symbol-function 'slack-message-replace-buffer)
                 (lambda (m _team) (push (slack-ts m) replaced))))
        (slack-buffer-add-star
         (make-instance 'slack-message-buffer
                        :team-id "T99999"
                        :room-id "C99999")
         ts))
      (should (equal (list (list slack-message-stars-add-url
                                  (list (cons "item_id" "C99999")
                                        (cons "ts" ts)
                                        (cons "item_type" "message"))))
                     requests))
      (should (slack-message-starred-p message))
      (should (equal (list ts) replaced)))))

(ert-deftest slack-stars-test-remove-star-syncs-model-and-buffer ()
  "Removing a star drops the saved item on success and deletes its line
from the stars buffer, without waiting for a star event."
  (slack-test-setup
    (oset team id "T00001")
    (oset team star
          (make-instance 'slack-star
                         :items (list (slack-stars-test--star-item
                                       (slack-test-ts 2) channel-id)
                                      (slack-stars-test--star-item
                                       (slack-test-ts 3) channel-id))))
    (let* ((buffer (generate-new-buffer " *test-stars*"))
           (stars (slack-stars-buffer :team-id (oref team id))))
      (with-current-buffer buffer
        (lui-mode)
        (lui-set-prompt "test> ")
        (slack-stars-test--insert-dummy-line)
        (let ((lui-time-stamp-position nil))
          (lui-insert-with-text-properties "saved message 2\n"
                                           'ts (slack-test-ts 2))
          (lui-insert-with-text-properties "saved message 3\n"
                                           'ts (slack-test-ts 3))))
      (cl-letf (((symbol-function 'slack-buffer-team) (lambda (_) team))
                ((symbol-function 'slack-buffer-buffer) (lambda (_) buffer))
                ((symbol-function 'slack-request)
                 (lambda (req)
                   (funcall (oref req success) :data '(:ok t))))
                ((symbol-function 'slack-log) (lambda (&rest _) nil)))
        (slack-buffer-remove-star stars (slack-test-ts 2)))
      (should (equal (list (slack-test-ts 3))
                     (mapcar #'slack-ts (slack-star-items (oref team star)))))
      (with-current-buffer buffer
        (should (not (string-match-p "saved message 2" (buffer-string))))
        (should (string-match-p "saved message 3" (buffer-string)))
        (should (not (slack-buffer-ts-eq (point-min) (point-max)
                                         (slack-test-ts 2)))))
      (kill-buffer buffer))))

(defun slack-stars-test--faces-at (text pos)
  "The face property of TEXT at POS, always as a list."
  (let ((faces (get-text-property pos 'face text)))
    (if (listp faces) faces (list faces))))

(defun slack-stars-test--highlighted-p (text)
  "Is TEXT drawn with the starred highlight?"
  (and (memq 'slack-starred-message-face
             (slack-stars-test--faces-at text 0))
       t))

(ert-deftest slack-stars-test-starred-message-is-highlighted ()
  "A starred message renders with the starred face on top of its other
faces; an unstarred one does not.  The :star: header marker stays so the
highlight is not the only cue."
  (slack-test-setup
    (oset team id "T00001")
    (let* ((message (slack-test-message team channel (slack-test-ts 1) "hi"))
           (plain (slack-message-to-string message team)))
      (should (not (slack-stars-test--highlighted-p plain)))
      (slack-message-star-added message)
      (let ((starred (slack-message-to-string message team)))
        (should (slack-stars-test--highlighted-p starred))
        (should (string-match-p ":star:" starred))
        ;; the header face survives on top of the highlight
        (should (memq 'slack-message-output-header
                      (slack-stars-test--faces-at starred 0)))))))

(ert-deftest slack-stars-test-saved-list-drives-highlight ()
  "Messages saved before this session carry nothing on their own slot,
so the highlight comes from the team's saved for later list.  A saved
timestamp only counts for the room it was saved in."
  (slack-test-setup
    (oset team id "T00001")
    (let ((message (slack-test-message team channel (slack-test-ts 1) "hi"))
          (other (slack-test-message team channel (slack-test-ts 2) "ho")))
      (should (not (slack-message-starred-p message)))
      (oset team star
            (make-instance 'slack-star
                           :items (list (slack-stars-test--star-item
                                         (slack-test-ts 1) channel-id))))
      (should (slack-stars-test--highlighted-p
               (slack-message-to-string message team)))
      (should (not (slack-stars-test--highlighted-p
                    (slack-message-to-string other team))))
      ;; same timestamp, a room the message does not belong to
      (oset team star
            (make-instance 'slack-star
                           :items (list (slack-stars-test--star-item
                                         (slack-test-ts 1) "C99999"))))
      (should (not (slack-stars-test--highlighted-p
                    (slack-message-to-string message team)))))))

(ert-deftest slack-stars-test-highlight-can-be-turned-off ()
  "`slack-highlight-starred-messages' nil drops the highlight, and the
stars buffer turns it off for itself: every message there is saved."
  (slack-test-setup
    (oset team id "T00001")
    (let ((message (slack-test-message team channel (slack-test-ts 1) "hi")))
      (slack-message-star-added message)
      (let ((slack-highlight-starred-messages nil))
        (should (not (slack-stars-test--highlighted-p
                      (slack-message-to-string message team)))))
      (with-temp-buffer
        (slack-stars-buffer-mode)
        (should (not slack-highlight-starred-messages))
        (should (not (slack-stars-test--highlighted-p
                      (slack-message-to-string message team))))))))

(ert-deftest slack-stars-test-refresh-buffer-bound-to-g ()
  "Both g and G refresh the stars buffer."
  (should (eq 'slack-stars-refresh-buffer
              (lookup-key slack-stars-buffer-mode-map "g")))
  (should (eq 'slack-stars-refresh-buffer
              (lookup-key slack-stars-buffer-mode-map "G"))))

;;; slack-stars-test.el ends here
