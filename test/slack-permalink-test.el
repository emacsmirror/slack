;;; slack-permalink-test.el --- tests for permalink round-trips -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest slack-test-permalink-to-info ()
  "A standard permalink parses to team-domain, room-id, ts and thread-ts."
  (should (equal
           (list :team-domain "clojurians"
                 :room-id "C099W16KZ"
                 :ts "1730182493.679269"
                 :thread-ts "1730182493.679269")
           (slack-permalink-to-info
            "https://clojurians.slack.com/archives/C099W16KZ/p1730182493679269?thread_ts=1730182493.679269&cid=C099W16KZ")))
  ;; without thread_ts the message is its own thread root
  (should (equal
           (list :team-domain "clojurians"
                 :room-id "C099W16KZ"
                 :ts "1730182493.679269"
                 :thread-ts "1730182493.679269")
           (slack-permalink-to-info
            "https://clojurians.slack.com/archives/C099W16KZ/p1730182493679269"))))

(ert-deftest slack-test-info-to-permalink ()
  "Permalinks are well-formed query strings in both thread variants."
  (should (equal
           "https://clojurians.slack.com/archives/C099W16KZ/p1730182493679269?thread_ts=1730182493.679269&cid=C099W16KZ"
           (slack-info-to-permalink
            (list :team-domain "clojurians"
                  :room-id "C099W16KZ"
                  :ts "1730182493.679269"
                  :thread-ts "1730182493.679269"))))
  ;; no thread-ts: a single ?cid query parameter (not a dangling &cid)
  (should (equal
           "https://clojurians.slack.com/archives/C099W16KZ/p1730182493679269?cid=C099W16KZ"
           (slack-info-to-permalink
            (list :team-domain "clojurians"
                  :room-id "C099W16KZ"
                  :ts "1730182493.679269"
                  :thread-ts nil)))))

(ert-deftest slack-test-permalink-round-trip ()
    "info -> permalink -> info preserves all fields."
    (let ((info (list :team-domain "clojurians"
                      :room-id "C099W16KZ"
                      :ts "1730182493.679269"
                      :thread-ts "1730182493.679269")))
      (should (equal info (slack-permalink-to-info
                           (slack-info-to-permalink info))))))

(ert-deftest slack-test-permalink-room-level ()
  "A link to a room, with no message part, parses with a nil ts."
  (should (equal
           (list :team-domain "clojurians"
                 :room-id "C099W16KZ"
                 :ts nil
                 :thread-ts nil)
           (slack-permalink-to-info
            "https://clojurians.slack.com/archives/C099W16KZ")))
  ;; a trailing slash or query string is not part of the room id
  (should (equal "C099W16KZ"
                 (plist-get (slack-permalink-to-info
                             "https://clojurians.slack.com/archives/C099W16KZ/")
                            :room-id))))

(ert-deftest slack-test-permalink-enterprise-domain ()
  "An enterprise grid link carries the organization domain, which is kept
whole: it is the workspace lookup, not the parser, that has to cope."
  (let ((info (slack-permalink-to-info
               "https://example-org.enterprise.slack.com/archives/C12345678")))
    (should (equal "example-org.enterprise" (plist-get info :team-domain)))
    (should (equal "C12345678" (plist-get info :room-id)))
    (should (null (plist-get info :ts))))
  (let ((info (slack-permalink-to-info
               "https://example-org.enterprise.slack.com/archives/C12345678/p1730182493679269")))
    (should (equal "C12345678" (plist-get info :room-id)))
    (should (equal "1730182493.679269" (plist-get info :ts)))))

(ert-deftest slack-test-permalink-non-slack-url ()
  "A URL that is not a Slack link gives nil, and leaves no stale match
data behind for the next caller to pick up."
  (should (null (slack-permalink-to-info "https://example.com/archives/C099W16KZ")))
  (should (null (slack-permalink-to-info "https://clojurians.slack.com/team/U123")))
  ;; a successful parse followed by a failed one must not leak the first
  (should (slack-permalink-to-info
           "https://clojurians.slack.com/archives/C099W16KZ/p1730182493679269"))
  (should (null (slack-permalink-to-info "not a url at all"))))

;;; slack-permalink-test.el ends here
