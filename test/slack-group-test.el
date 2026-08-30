;;; slack-group-test.el --- tests for group/MPIM creation -*- lexical-binding: t; -*-

(require 'ert)
(require 'slack-group)
(require 'slack-im)

(ert-deftest slack-test-group-mpim-open-from-current-im ()
  "From an IM buffer, `slack-group-mpim-open-from-current' seeds the new
group with the IM's existing user plus the additionally selected users."
  (slack-test-setup
    (oset team id "T00001")
    (let* ((im-user-id "U22222")
           (im (make-instance 'slack-im :id "D111" :user im-user-id))
           (selected-user (list :id "U33333"
                                :name "Bob"
                                :profile (list :display_name_normalized "Bob"
                                               :real_name_normalized "Bob")))
           (opened-ids nil))
      (cl-letf (((symbol-function 'slack-buffer-team) (lambda (_) team))
                ((symbol-function 'slack-buffer-room) (lambda (_) im))
                ((symbol-function 'slack-user-names)
                 (lambda (_team) (list (cons "Bob" selected-user))))
                ((symbol-function 'slack-select-multiple)
                 (lambda (_prompt _coll) (list selected-user)))
                ((symbol-function 'slack-conversations-open)
                 (lambda (_team &rest plist)
                   (setq opened-ids (plist-get plist :user-ids)))))
        (let ((slack-current-buffer 'fake-buf))
          (slack-group-mpim-open-from-current)))
      (should (equal (list im-user-id "U33333") opened-ids)))))

(ert-deftest slack-test-group-mpim-open-from-current-not-dm ()
  "Outside an IM/group DM the command declines instead of creating a group."
  (slack-test-setup
    (oset team id "T00001")
    (let* ((channel (make-instance 'slack-channel :id "C111" :name "chan"))
           (opened nil))
      (cl-letf (((symbol-function 'slack-buffer-team) (lambda (_) team))
                ((symbol-function 'slack-buffer-room) (lambda (_) channel))
                ((symbol-function 'slack-conversations-open)
                 (lambda (&rest _) (setq opened t))))
        (let ((slack-current-buffer 'fake-buf))
          (slack-group-mpim-open-from-current)))
      (should (null opened)))))

;;; slack-group-test.el ends here
