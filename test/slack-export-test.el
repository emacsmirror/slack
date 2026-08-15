;;; slack-export-test.el --- tests for flattened room exports -*- lexical-binding: t; -*-

(require 'ert)
(require 'slack-export)
(require 'slack-team)
(require 'slack-channel)

(defun slack-export-test-ts (n)
  "Return a fixed-width timestamp for export test message N."
  (format "16000000%02d.000000" n))

(defun slack-export-test-message (team room ts text &optional thread-ts)
  "Create an export test message."
  (slack-message-create
   (append (list :type "message" :ts ts :user "U11111" :text text)
           (and thread-ts (list :thread_ts thread-ts)))
   team room))

(defmacro slack-export-test-setup (&rest body)
  "Run BODY with a test team and channel."
  (declare (indent 0) (debug t))
  `(let* ((users (make-hash-table :test 'equal))
          (channels (make-hash-table :test 'equal))
          (team (make-instance 'slack-team
                               :self-id "U00000"
                               :id "T99999"
                               :token "xoxb-export-test"
                               :name "test-team"
                               :users users
                               :channels channels))
          (channel (make-instance 'slack-channel
                                  :id "C99999"
                                  :name "chan")))
     (puthash "U11111"
              (list :name "tester"
                    :id "U11111"
                    :profile (list :display_name_normalized "Tester"
                                   :real_name_normalized "Tester"))
              users)
     (puthash (oref channel id) channel channels)
     ,@body))

(ert-deftest slack-test-export-indents-loaded-thread-replies ()
  "The flattened export keeps roots and indents their loaded replies."
  (slack-export-test-setup
    (let* ((root-ts (slack-export-test-ts 1))
           (reply-ts (slack-export-test-ts 2))
           (root (slack-export-test-message team channel root-ts
                                            "root" root-ts))
           (reply (slack-export-test-message team channel reply-ts
                                             "reply" root-ts)))
      (slack-room-set-messages channel (list root reply) team)
      (slack-message-set-replies channel root-ts (list reply))
      (with-temp-buffer
        (slack-export--insert-room channel team)
        (let ((text (buffer-string)))
          (should (string-match-p "root" text))
          (should (string-match-p "^  .*reply" text))
          (should (not (string-match-p "^  .*root" text))))))))

(ert-deftest slack-test-export-fetches-thread-pages ()
  "The exporter follows every conversations.replies cursor."
  (slack-export-test-setup
    (let* ((root-ts (slack-export-test-ts 1))
           (reply-one-ts (slack-export-test-ts 2))
           (reply-two-ts (slack-export-test-ts 3))
           (root (slack-export-test-message team channel root-ts
                                            "root" root-ts))
           (reply-one (slack-export-test-message team channel reply-one-ts
                                                 "reply one" root-ts))
           (reply-two (slack-export-test-message team channel reply-two-ts
                                                 "reply two" root-ts))
           (calls 0))
      (oset root reply-count 2)
      (slack-room-set-messages channel (list root) team)
      (cl-letf (((symbol-function 'slack-conversations-replies)
                 (lambda (room ts team &rest args)
                   (cl-incf calls)
                   (funcall (plist-get args :after-success)
                            (if (= calls 1)
                                (list root reply-one)
                              (list reply-two))
                            (and (= calls 1) "page-2")
                            (= calls 1)))))
        (slack-export--fetch-thread
         root channel team
         (lambda ()
           (should (= 2 calls))
           (should (equal (list reply-one-ts reply-two-ts)
                          (oref root replies))))))
      (should (= 2 calls)))))
