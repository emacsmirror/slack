;;; slack-room-buffer-test.el --- room buffer tests -*- lexical-binding: t; -*-

;; Covers how URLs attached to buttons open: a Slack permalink is opened
;; in emacs-slack (`slack-open-url'), anything else in the browser.

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
       "https://writerai.slack.com/archives/C0B8JAQN2S2/p1790227143905189")
      (should (equal (list "https://writerai.slack.com/archives/C0B8JAQN2S2/p1790227143905189")
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
