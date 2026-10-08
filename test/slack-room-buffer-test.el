;;; slack-room-buffer-test.el --- room buffer tests -*- lexical-binding: t; -*-

;; Covers how URLs attached to buttons open: a Slack permalink is opened
;; in emacs-slack (`slack-open-url'), anything else in the browser.  Also
;; covers what `slack-open-url' does with links that name a room rather
;; than a message, and with rooms emacs-slack has not loaded.

(require 'ert)
(require 'eieio)
(require 'slack-room-buffer)
(require 'slack-message-buffer)
(require 'slack-create-message)
(require 'slack-block)

(ert-deftest slack-room-buffer-test-open-url-or-browse-permalink ()
  "A Slack permalink goes to `slack-open-url', not to the browser."
  (let ((opened nil)
        (browsed nil))
    (cl-letf (((symbol-function 'slack-open-url) (lambda (url) (push url opened)))
              ((symbol-function 'browse-url) (lambda (&rest _) (push :browsed browsed))))
      (slack-open-url-or-browse-url
       "https://test-team.slack.com/archives/C12345678/p1790227143905189")
      (should (equal (list "https://test-team.slack.com/archives/C12345678/p1790227143905189")
                     opened))
      (should (null browsed)))))

(ert-deftest slack-room-buffer-test-open-url-or-browse-external-url ()
  "A URL outside *.slack.com goes straight to the browser."
  (let ((opened nil)
        (browsed nil))
    (cl-letf (((symbol-function 'slack-open-url) (lambda (&rest _) (push :opened opened)))
              ((symbol-function 'browse-url) (lambda (url) (push url browsed))))
      (slack-open-url-or-browse-url "https://example.com/some/app")
      (should (null opened))
      (should (equal (list "https://example.com/some/app") browsed)))))

(ert-deftest slack-room-buffer-test-open-url-or-browse-permalink-fallback ()
  "A permalink that `slack-open-url' cannot open falls back to the browser."
  (let ((browsed nil))
    (cl-letf (((symbol-function 'slack-open-url)
               (lambda (_) (error "Not an url: mock")))
              ((symbol-function 'browse-url) (lambda (url) (push url browsed))))
      (slack-open-url-or-browse-url
       "https://other-team.slack.com/archives/C1/p1730182493679269")
      (should (equal (list "https://other-team.slack.com/archives/C1/p1730182493679269")
                     browsed)))))

(ert-deftest slack-room-buffer-test-button-permalink-opens-in-emacs-slack ()
  "Pressing RET on a Block Kit button whose URL is a Slack permalink opens
the permalink with `slack-open-url' instead of the browser."
  (slack-test-with-registered-team (team channel)
    (let* ((ts (slack-test-ts 1))
           (permalink "https://test-team.slack.com/archives/C99999/p1730182493679269")
           (message (slack-message-create
                     (list :type "message"
                           :ts ts
                           :user "U11111"
                           :text "metrics suffice before activation."
                           :blocks (list (list :type "actions"
                                               :block_id "b1"
                                               :elements
                                               (list (list :type "button"
                                                           :action_id "a1"
                                                           :url permalink
                                                           :text (list :type "plain_text"
                                                                       :text "Request"))))))
                     team
                     channel))
           (opened nil)
           (browsed nil))
      (slack-room-set-messages channel (list message) team)
      (with-temp-buffer
        (let ((button (slack-block-find-action
                       (slack-message-find-block message "b1")
                       "a1")))
          (insert (propertize "metrics suffice before activation. " 'ts ts))
          (insert (slack-block-to-string button))
          ;; `slack-get-ts' reads the ts property from the cursor's line
          (put-text-property (point-min) (point-max) 'ts ts)
          (goto-char (point-min))
          (search-forward "Request")
          (goto-char (match-beginning 0))
          (cl-letf (((symbol-function 'slack-open-url) (lambda (url) (push url opened)))
                    ((symbol-function 'browse-url) (lambda (&rest _) (push :browsed browsed))))
            (slack-buffer-execute-button-block-action
             (make-instance 'slack-message-buffer
                            :team-id "T99999"
                            :room-id "C99999")))
          (should (equal (list permalink) opened))
          (should (null browsed)))))))

(ert-deftest slack-room-buffer-test-open-url-room-level-link ()
  "A link naming a room, with no message part, opens the room itself."
  (slack-test-with-registered-team (team channel)
    (let ((displayed nil))
      (cl-letf (((symbol-function 'slack-room-display)
                 (lambda (room _team) (push (oref room id) displayed)))
                ((symbol-function 'slack-open-message)
                 (lambda (&rest _) (push :message displayed))))
        (slack-open-url "https://test-team.slack.com/archives/C99999"))
      (should (equal (list "C99999") displayed)))))

(ert-deftest slack-room-buffer-test-open-url-enterprise-domain ()
  "An enterprise grid link names the organization, which matches no
workspace domain, so the team is found through the room it names."
  (slack-test-with-registered-team (team channel)
    (let ((displayed nil))
      (cl-letf (((symbol-function 'slack-room-display)
                 (lambda (room _team) (push (oref room id) displayed)))
                ;; a prompt here would mean the room lookup did not work
                ((symbol-function 'slack-team-select)
                 (lambda (&rest _) (error "Should not ask which team"))))
        (slack-open-url
         "https://example-org.enterprise.slack.com/archives/C99999"))
      (should (equal (list "C99999") displayed)))))

(ert-deftest slack-room-buffer-test-open-url-fetches-unknown-room ()
  "A room emacs-slack does not know, an archived channel for instance, is
fetched before being opened."
  (slack-test-with-registered-team (team channel)
    (let ((fetched nil)
          (displayed nil))
      (cl-letf (((symbol-function 'slack-conversations-info)
                 (lambda (room-id team &optional after-success)
                   (push room-id fetched)
                   ;; conversations.info caches the room on the team
                   (slack-team-set-room
                    team (make-instance 'slack-channel :id room-id :name "archived"))
                   (funcall after-success)))
                ((symbol-function 'slack-room-display)
                 (lambda (room _team) (push (oref room id) displayed))))
        (slack-open-url "https://test-team.slack.com/archives/C01ARCHIVED"))
      (should (equal (list "C01ARCHIVED") fetched))
      (should (equal (list "C01ARCHIVED") displayed)))))

(ert-deftest slack-room-buffer-test-open-url-message-link-still-opens-message ()
  "A message permalink still lands on the message, not on the room."
  (slack-test-with-registered-team (team channel)
    (let ((opened nil))
      (cl-letf (((symbol-function 'slack-open-message)
                 (lambda (_team room ts thread-ts)
                   (push (list (oref room id) ts thread-ts) opened)))
                ((symbol-function 'slack-room-display)
                 (lambda (&rest _) (push :room opened))))
        (slack-open-url
         "https://test-team.slack.com/archives/C99999/p1730182493679269"))
      (should (equal (list (list "C99999" "1730182493.679269" "1730182493.679269"))
                     opened)))))

(ert-deftest slack-room-buffer-test-room-level-link-is-opened-in-emacs-slack ()
  "Clicking a link to a channel opens it in emacs-slack, not the browser."
  (let ((opened nil)
        (browsed nil))
    (cl-letf (((symbol-function 'slack-open-url) (lambda (url) (push url opened)))
              ((symbol-function 'browse-url) (lambda (&rest _) (push :browsed browsed))))
      (slack-open-url-or-browse-url
       "https://example-org.enterprise.slack.com/archives/C12345678")
      (should (equal (list "https://example-org.enterprise.slack.com/archives/C12345678")
                     opened))
      (should (null browsed)))))
