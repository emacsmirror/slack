;;; slack-modeline-test.el --- tests for the modeline unread summary -*- lexical-binding: t; -*-

(require 'ert)
(require 'slack-team)
(require 'slack-channel)
(require 'slack-counts)
(require 'slack-room)
(require 'slack-modeline)

(defun slack-modeline-test--conv (id has-unreads mention-count)
  "Build a `slack-counts-conversation' for the test."
  (make-instance 'slack-counts-conversation
                 :id id
                 :has_unreads has-unreads
                 :mention_count mention-count
                 :latest "0"))

(defun slack-modeline-test--counts (channels)
  "Build a `slack-counts' with CHANNELS (a list of conversation counts)."
  (make-instance 'slack-counts
                 :threads (make-instance 'slack-counts-threads
                                        :has_unreads nil
                                        :mention_count 0)
                 :channels channels
                 :mpims nil
                 :ims nil))

(defmacro slack-modeline-test-setup (&rest body)
  "Run BODY with a team holding a subscribed and a non-subscribed channel."
  (declare (indent 0) (debug t))
  `(let* ((sub-channel (make-instance 'slack-channel
                                      :id "C11111"
                                      :name "Subscribed"))
          (other-channel (make-instance 'slack-channel
                                        :id "C22222"
                                        :name "Other"))
          (channels (let ((h (make-hash-table :test 'equal)))
                      (puthash (oref sub-channel id) sub-channel h)
                      (puthash (oref other-channel id) other-channel h)
                      h))
          (team (make-instance 'slack-team
                               :self-id "U00000"
                               :id "T99999"
                               :channels channels
                               :subscribed-channels (list 'Subscribed))))
     ,@body))

(ert-deftest slack-test-modeline-subscribed-channel-unread ()
  "With the subscription filter on, an unread subscribed channel turns the
modeline red."
  (slack-modeline-test-setup
    (let ((slack-modeline-count-only-subscribed-channel t)
          (counts (slack-modeline-test--counts
                   (list (slack-modeline-test--conv "C11111" t 2)))))
      (oset team counts counts)
      (let ((summary (slack-team-counts-summary team)))
        (should (equal '((thread . (nil . 0))
                         (channel . (t . 2)))
                       summary))))))

(ert-deftest slack-test-modeline-non-subscribed-channel-not-counted ()
  "With the subscription filter on, an unread channel that is NOT subscribed
does not turn the modeline red and is not counted."
  (slack-modeline-test-setup
    (let ((slack-modeline-count-only-subscribed-channel t)
          (counts (slack-modeline-test--counts
                   (list (slack-modeline-test--conv "C22222" t 3)))))
      (oset team counts counts)
      (let ((summary (slack-team-counts-summary team)))
        (should (equal '((thread . (nil . 0))
                         (channel . (nil . 0)))
                       summary))))))

(ert-deftest slack-test-modeline-mixed-subscribed-and-not ()
  "With the filter on, only the subscribed channel's mentions are counted,
but unreads from either would still flag red because the subscribed one is
unread."
  (slack-modeline-test-setup
    (let ((slack-modeline-count-only-subscribed-channel t)
          (counts (slack-modeline-test--counts
                   (list (slack-modeline-test--conv "C22222" t 5)
                         (slack-modeline-test--conv "C11111" t 2)))))
      (oset team counts counts)
      (let ((summary (slack-team-counts-summary team)))
        (should (equal '((thread . (nil . 0))
                         (channel . (t . 2)))
                       summary))))))

(ert-deftest slack-test-modeline-filter-off-counts-everything ()
  "With the subscription filter off, every unread channel is counted even
if it is not subscribed."
  (slack-modeline-test-setup
    (let ((slack-modeline-count-only-subscribed-channel nil)
          (counts (slack-modeline-test--counts
                   (list (slack-modeline-test--conv "C22222" t 5)
                         (slack-modeline-test--conv "C11111" nil 2)))))
      (oset team counts counts)
      (let ((summary (slack-team-counts-summary team)))
        (should (equal '((thread . (nil . 0))
                         (channel . (t . 7)))
                       summary))))))

(ert-deftest slack-test-modeline-no-counts ()
  "A team without counts reports no unreads."
  (slack-modeline-test-setup
    (let ((summary (slack-team-counts-summary team)))
      (should (equal '((thread . (nil . 0))
                       (channel . (nil . 0)))
                     summary)))))

;;; slack-modeline-test.el ends here
