;;; slack-room-event-test.el --- room event tests -*- lexical-binding: t; -*-

;; Room events from the websocket (read markers, renames, archives, members
;; joining or leaving) are applied through `slack-event-update'.  The network
;; and the UI are stubbed: only the room the event names is updated here.

(require 'ert)
(require 'eieio)
(require 'slack-room-event)
(require 'slack-websocket)

(ert-deftest slack-room-event-test-channel-marked-sets-last-read ()
  "A channel_marked event moves the channel's read marker.
`slack-event-save-room' of a synchronous room event receives a callback
argument as asynchronous ones do, so the update reaches the room."
  (slack-test-with-registered-team (team channel)
    (oset channel created 1657626419)
    (let (updated)
      (cl-letf (((symbol-function 'slack-event-update-ui)
                 (lambda (_this room _team) (setq updated room))))
        (slack-ws-handle-room-marked
         (list :type "channel_marked" :channel "C99999"
               :ts "1657626419.612969"
               :unread_count_display 0 :mention_count_display 0)
         team)
        (should (equal (oref channel last-read) "1657626419.612969"))
        (should (eq updated channel))))))

(ert-deftest slack-room-event-test-channel-rename-sets-name ()
  "A channel_rename event renames the channel."
  (slack-test-with-registered-team (team channel)
    (oset channel created 1657626419)
    (cl-letf (((symbol-function 'slack-event-update-ui) #'ignore))
      (slack-ws-handle-room-rename
       (list :type "channel_rename"
             :channel (list :id "C99999" :name "renamed"
                            :name_normalized "renamed"))
       team)
      (should (equal (oref channel name) "renamed")))))

;;; slack-room-event-test.el ends here
