;;; slack-room.el --- slack generic room interface    -*- lexical-binding: t; -*-

;; Copyright (C) 2015  南優也

;; Author: 南優也 <yuyaminami@minamiyuunari-no-MacBook-Pro.local>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'eieio)
(require 'lui)
(require 'slack-util)
(require 'slack-request)
(require 'slack-user)
(require 'slack-counts)

(defface slack-room-unread-face
  '((t (:weight bold)))
  ;; '((t (:box (:line-width 1 :style released-button))))
  "Face used to mark a room as unread when selecting channels."
  :group 'slack)

(defvar slack-buffer-function)
(defvar slack-completing-read-function)
(defvar slack-display-team-name)
(defvar slack-current-buffer)
(defvar slack-buffer-create-on-notify)

(defclass slack-room ()
  ((id :initarg :id)
   (created :initarg :created)
   (unread-count :initarg :unread_count :initform 0 :type integer)
   (unread-count-display :initarg :unread_count_display :initform 0 :type integer)
   (message-ids :initform '() :type list)
   (messages :initform (make-hash-table :test 'equal :size 300))
   ;; See "Message ranges" below.
   (message-ranges :initform '() :type list)
   (history-start-reached :initform nil)
   (last-read :initarg :last_read :type string :initform "0")
   (topic :initarg :topic :initform nil)))

(cl-defgeneric slack-room-name (room team))

(cl-defmethod slack-equalp ((this slack-room) other)
  (string= (oref this id)
           (oref other id)))

(cl-defmethod slack-merge ((this slack-room) other)
  "except MESSAGES"
  (oset this id (oref other id))
  (oset this created (oref other created))
  (oset this unread-count (oref other unread-count))
  (oset this unread-count-display (oref other unread-count-display))
  (unless (string= "0" (oref other last-read))
    (oset this last-read (oref other last-read))))

(defun slack-room-create (payload class)
  (let* ((attributes (slack-collect-slots class payload)))
    (apply #'make-instance class attributes)))

(cl-defmethod slack-room-subscribedp ((_room slack-room) _team)
  nil)

(cl-defmethod slack-room-muted-p ((_this slack-room) _team)
  nil)

(cl-defmethod slack-room-hidden-p ((room slack-room))
  (slack-room-hiddenp room))

(defun slack-room-hiddenp (room)
  (or (not (slack-room-member-p room))
      (slack-room-archived-p room)
      (not (slack-room-open-p room))))

(defun slack-room-names (rooms team &optional filter collecter)
  (cl-labels
      ((latest-ts (room)
                  (slack-room-latest room team))
       (sort-rooms (rooms)
                   (nreverse (cl-sort (append rooms nil)
                                      #'string< :key #'latest-ts))))
    (cl-loop for room in (sort-rooms (if filter
                                         (funcall filter rooms)
                                       rooms))
             as label = (slack-room-label room team)
             collect (if (functionp collecter)
                         (funcall collecter label room)
                       (cons label room)))))

(defun slack-room-select (rooms team)
  (let* ((alist (slack-room-names
                 rooms team #'(lambda (rs) (cl-remove-if #'slack-room-hidden-p rs)))))
    (slack-select-from-list (alist "Select Channel: "))))

(defun slack-room-find-message (room ts)
  (with-slots (messages) room
    (gethash ts messages)))

(cl-defmethod slack-room-display-name ((room slack-room) team)
  (let ((room-name (slack-room-name room team)))
    (if slack-display-team-name
        (format "%s - %s"
                (slack-team-name team)
                room-name)
      room-name)))

(cl-defmethod slack-room-label-prefix ((_room slack-room) _team)
  "  ")

(cl-defmethod slack-room-mention-count-display ((room slack-room) team)
  (let ((count (slack-room-mention-count room team)))
    (if (< 0 count) (format "(%s)" count) "")))

(cl-defmethod slack-room-mention-count ((this slack-room) team)
  (with-slots (counts) team
    (if counts
        (slack-counts-channel-mention-count counts this)
      0)))

(cl-defmethod slack-room-set-mention-count ((this slack-room) count team)
  (slack-if-let* ((counts (oref team counts)))
      (slack-counts-channel-set-mention-count counts
                                              this
                                              count)))

(cl-defmethod slack-room-set-has-unreads ((this slack-room) value team)
  (slack-if-let* ((counts (oref team counts)))
      (slack-counts-channel-set-has-unreads counts this value)))

(cl-defmethod slack-room-label ((room slack-room) team)
  (let ((str (format "%s %s%s"
                     (slack-room-label-prefix room team)
                     (slack-room-display-name room team)
                     (slack-room-mention-count-display room team))))
    (if (slack-room-has-unread-p room team)
        (propertize str 'face 'slack-room-unread-face)
      str)))

(cl-defmethod slack-room-name ((room slack-room) _team)
  (oref room name))

(defun slack-room-sort-messages (messages)
  (cl-sort messages #'string< :key #'slack-ts))

(cl-defmethod slack-room-sorted-messages ((room slack-room) &optional message-ids)
  (with-slots (messages) room
    (let ((ids (or message-ids (oref room message-ids)))
          (ret))
      (cl-loop for id in (reverse ids)
               do (slack-if-let* ((message (gethash id messages)))
                      (push message ret)))
      ret)))

(cl-defmethod slack-room-latest ((this slack-room) team)
  (with-slots (counts) team
    (or (when counts
          (slack-room--latest this counts))
        "0")))

(cl-defmethod slack-room--latest ((this slack-room) counts)
  (slack-counts-channel-latest counts this))

(cl-defmethod slack-room--update-latest ((this slack-room) counts ts)
  (slack-counts-channel-update-latest counts this ts))

;;; Message ranges
;;
;; WHY THIS EXISTS, IN PLAIN WORDS
;;
;; `messages' is a bag of messages keyed by timestamp.  A bag cannot tell you
;; whether two messages are neighbours in the channel or have ten thousand
;; unloaded messages between them.  That is fine while we only ever load the
;; newest page and then page backwards, but as soon as the user jumps to an old
;; message (from search, say) the bag holds islands:
;;
;;     bag: {msg3, msg7}            <- are 3 and 7 neighbours?  No way to know.
;;
;; A range answers exactly that.  It is a cons (OLDEST-TS . LATEST-TS) meaning
;; "every message between these two, inclusive, has been downloaded":
;;
;;     ranges: ((ts3 . ts3) (ts7 . ts7))    two islands, one hole between them
;;
;; The hole between two ranges is what the message buffer draws as
;; "load older / load newer".  Filling a hole means downloading a chunk and
;; adding its range:
;;
;;     add (ts2 . ts3)  =>  ((ts2 . ts3) (ts7 . ts7))    hole is smaller
;;     add (ts4 . ts5)  =>  ((ts2 . ts5) (ts7 . ts7))    hole is smaller still
;;     add (ts5 . ts7)  =>  ((ts2 . ts7))                hole gone, one island
;;
;; That last line is the whole trick: we never diff message lists to notice
;; that a newly loaded chunk rejoined the rest of the history.  The ranges
;; overlap, and overlapping ranges get merged into one, so the hole (and its
;; buttons) simply stops existing.
;;
;; Slack cooperates: ask for a window with oldest=A and latest=B, and if the
;; reply says has_more=false then that window is complete.  In that case record
;; (A . B) itself instead of the first/last timestamp that came back - that is
;; what makes neighbouring ranges touch and merge.  Recording only what came
;; back would leave a hole with nothing in it, and its buttons would never go
;; away.
;;
;; Timestamps are strings like "1657626419.612969", so plain `string<' orders
;; them correctly and is used throughout.

(defun slack-ranges-normalize (ranges)
  "Sort RANGES oldest first and merge any that overlap or touch.

>> (slack-ranges-normalize (list '(\"d\" . \"e\") '(\"a\" . \"b\")))
=> ((\"a\" . \"b\") (\"d\" . \"e\"))"

  (let ((sorted (cl-sort (cl-remove-if #'null (copy-sequence ranges))
                         #'string< :key #'car))
        (ret '()))
    (dolist (range sorted)
      (let ((prev (car ret)))
        (if (and prev (not (string< (cdr prev) (car range))))
            ;; RANGE starts at or before the end of the previous block, so the
            ;; two are really one block: stretch the previous one if needed.
            (when (string< (cdr prev) (cdr range))
              (setcdr prev (cdr range)))
          (push (cons (car range) (cdr range)) ret))))
    (nreverse ret)))

(defun slack-ranges-add (ranges oldest latest)
  "Return RANGES with the block running from OLDEST to LATEST merged in.

>> (slack-ranges-add '((\"e\" . \"f\")) \"a\" \"d\")
=> ((\"a\" . \"d\") (\"e\" . \"f\"))"
  (slack-ranges-normalize
   (if (or (null oldest) (null latest))
       ranges
     (let ((lo (if (string< latest oldest) latest oldest))
           (hi (if (string< latest oldest) oldest latest)))
       (cons (cons lo hi) ranges)))))

(defun slack-ranges-gaps (ranges)
  "Return the holes between RANGES as a list of (TOP-TS . BOTTOM-TS).
TOP-TS is the newest message we have before the hole and BOTTOM-TS the oldest
message we have after it, so the missing messages lie strictly between them.

>> (slack-ranges-gaps '((\"a\" . \"d\") (\"f\" . \"g\")))
=> ((\"d\" . \"f\"))"
  (cl-loop for rest on (slack-ranges-normalize ranges)
           while (cdr rest)
           collect (cons (cdar rest)
                         (caadr rest))))

(defun slack-ranges-contain-p (ranges ts)
  "Return non-nil when TS falls inside one of RANGES.

>> (slack-ranges-contain-p '((\"a\" . \"e\")) \"d\")
=> (\"a\" . \"e\")

>> (slack-ranges-contain-p '((\"a\" . \"e\")) \"z\")
=> nil"
  (cl-find-if #'(lambda (range)
                  (and (not (string< ts (car range)))
                       (not (string< (cdr range) ts))))
              ranges))

(defun slack-ranges-clip (ranges oldest-kept)
  "Return RANGES with everything older than OLDEST-KEPT removed.
Call this after dropping old messages from the store, otherwise the ranges
would claim we still have history that we just threw away.

>> (slack-ranges-clip '((\"a\" . \"c\") (\"e\" . \"d\")) \"b\")
=> ((\"b\" . \"c\") (\"e\" . \"d\"))"
  (when oldest-kept
    (cl-loop for range in (slack-ranges-normalize ranges)
             ;; whole block is older than what we kept: forget it
             unless (string< (cdr range) oldest-kept)
             collect (if (string< (car range) oldest-kept)
                         (cons oldest-kept (cdr range))
                       range))))

(defun slack-messages-oldest-ts (messages)
  (car (cl-sort (mapcar #'slack-ts messages) #'string<)))

(defun slack-messages-latest-ts (messages)
  (car (last (cl-sort (mapcar #'slack-ts messages) #'string<))))

(cl-defmethod slack-room-ranges ((room slack-room))
  "Blocks of contiguous history loaded for ROOM, oldest first.
When nothing was ever recorded but messages exist, treat them as one block:
that is how the buffer behaved before ranges existed."
  (or (oref room message-ranges)
      (slack-if-let* ((ids (oref room message-ids)))
          (list (cons (car ids) (car (last ids)))))))

(cl-defmethod slack-room-gaps ((room slack-room))
  "Holes in ROOM's loaded history, as (TOP-TS . BOTTOM-TS) pairs."
  (slack-ranges-gaps (slack-room-ranges room)))

(cl-defmethod slack-room-ensure-ranges ((room slack-room))
  "Write down what ROOM holds right now, if nothing was written down yet.
Call this before adding messages that are NOT next to the ones already there,
otherwise the fallback in `slack-room-ranges' would look at the store after the
newcomers landed and cheerfully declare the whole thing one contiguous block,
hiding the very hole we are about to create."
  (unless (oref room message-ranges)
    (oset room message-ranges (slack-room-ranges room)))
  (oref room message-ranges))

(cl-defmethod slack-room-add-range ((room slack-room) oldest latest)
  "Record that ROOM has every message between OLDEST and LATEST."
  (oset room message-ranges
        (slack-ranges-add (slack-room-ranges room) oldest latest))
  (oref room message-ranges))

(cl-defmethod slack-room-range-messages ((room slack-room) range)
  "Messages of ROOM inside RANGE, oldest first."
  (cl-loop for ts in (oref room message-ids)
           if (and (not (string< ts (car range)))
                   (not (string< (cdr range) ts)))
           collect (slack-room-find-message room ts) into ret
           finally return (cl-remove-if #'null ret)))

(cl-defmethod slack-room-extend-latest-range ((room slack-room) ts)
  "Stretch ROOM's newest block up to TS.
Used for messages arriving live: while the websocket is connected we see every
new message, so nothing can be missing between the previous newest and TS."
  (slack-if-let* ((ranges (oref room message-ranges))
                  (newest (car (last ranges))))
      (when (string< (cdr newest) ts)
        (setcdr newest ts))))

(cl-defmethod slack-room-record-fetched-range ((room slack-room) messages
                                               &key oldest latest reached-start)
  "Record the block of history just fetched for ROOM.
MESSAGES is what came back.  OLDEST and LATEST are the window bounds you asked
Slack for: pass one only when you know the reply covered it (the request was
anchored there, or has_more came back nil), because then the block reaches that
bound even if no message sits exactly on it.  REACHED-START means there is
nothing older left to fetch."
  (let ((lo (or oldest (slack-messages-oldest-ts messages)))
        (hi (or latest (slack-messages-latest-ts messages))))
    (when (and lo hi)
      (slack-room-add-range room lo hi)))
  (when reached-start
    (oset room history-start-reached t))
  (oref room message-ranges))

(cl-defmethod slack-room-delete-message ((this slack-room) ts)
  (remhash ts (oref this messages))
  (oset this
        message-ids
        (cl-remove-if #'(lambda (e) (string= ts e))
                      (oref this message-ids))))

(cl-defmethod slack-room-push-message ((this slack-room) message team)
  (let ((ts (slack-ts message)))
    (puthash ts message (oref this messages))
    (cl-pushnew ts (oref this message-ids)
                :test #'string=)
    (oset this message-ids
          (cl-sort (oref this message-ids) #'string<))

    (slack-room-extend-latest-range this ts)

    (slack-if-let* ((counts (oref team counts)))
        (slack-room--update-latest this counts ts))))

(cl-defmethod slack-room-clear-messages ((room slack-room))
  (oset room messages (make-hash-table :test 'equal :size 300))
  (oset room message-ids '())
  (oset room message-ranges '())
  (oset room history-start-reached nil))


(cl-defmethod slack-room-trim-messages ((room slack-room) &optional (n 100))
  "Keep only the last N messages in ROOM.
Defaults to 100. Used to reduce memory after closing buffers."
  (with-slots (messages message-ids) room
    (let* ((len (length message-ids))
           (keep-ids (if (> len n)
                         (last message-ids n)
                       message-ids))
           (oldest-kept (car keep-ids))
           (new-ht (make-hash-table :test 'equal :size (max n 10))))
      (dolist (ts keep-ids)
        (slack-if-let* ((m (gethash ts messages)))
            (puthash ts m new-ht)))
      (oset room messages new-ht)
      (oset room message-ids (cl-sort keep-ids #'string<))
      ;; We just threw history away, so the ranges must forget it too,
      ;; otherwise they would promise messages the store no longer holds.
      (when (> len (length keep-ids))
        (oset room message-ranges
              (slack-ranges-clip (oref room message-ranges) oldest-kept))
        (oset room history-start-reached nil)))))

(cl-defmethod slack-room-set-messages ((room slack-room) messages team)
  (cl-loop for m in messages
           do (let ((ts (slack-ts m)))
                (puthash ts m (oref room messages))
                (cl-pushnew ts (oref room message-ids)
                            :test #'string=)))
  (oset room
        message-ids
        (cl-sort (oref room message-ids) #'string<))

  (slack-if-let* ((counts (oref team counts))
                  (latest (car (last (oref room message-ids)))))
      (slack-room--update-latest room counts latest)))

(cl-defmethod slack-room-update-mark ((room slack-room) team ts)
  (slack-conversations-mark room team ts))

(cl-defmethod slack-room-member-p ((_room slack-room)) t)

(cl-defmethod slack-room-archived-p ((_room slack-room)) nil)

(cl-defmethod slack-room-open-p ((_room slack-room)) t)

(cl-defmethod slack-room-equal-p ((room slack-room) other)
  (string= (oref room id) (oref other id)))

(cl-defmethod slack-room-inc-unread-count ((room slack-room))
  (cl-incf (oref room unread-count-display)))

(cl-defmethod slack-user-find ((room slack-room) team)
  (slack-user--find (oref room user) team))

(cl-defmethod slack-room-member-p ((_this slack-room))
  t)

(cl-defmethod slack-room-find ((id string) team)
  (if (and id team)
      (cl-labels ((find-room (room)
                             (string= id (oref room id))))
        (cond
         ((string-prefix-p "Q" id) (cl-find-if #'find-room (oref team search-results)))
         (t
          (or (gethash id (oref team channels))
              (gethash id (oref team groups))
              (gethash id (oref team ims))))))))

(cl-defmethod slack-room-has-unread-p ((this slack-room) team)
  (with-slots (counts) team
    (when counts
      (slack-room--has-unread-p this counts))))

(cl-defmethod slack-room--has-unread-p ((this slack-room) counts)
  (slack-counts-channel-unread-p counts this))

(cl-defmethod slack-mpim-p ((_this slack-room))
  nil)

(cl-defmethod slack-room-members ((_this slack-room))
  nil)

(cl-defmethod slack-room-set-members ((_this slack-room) _members))

(cl-defmethod slack-room-members-loaded-p ((_this slack-room))
  nil)

(cl-defmethod slack-room-members-loaded ((_this slack-room)))

(cl-defmethod slack-team-set-room ((this slack-team) room)
  (cl-case (eieio-object-class-name room)
    (slack-channel (slack-team-set-channels this (list room)))
    (slack-group (slack-team-set-groups this (list room)))
    (slack-im (slack-team-set-ims this (list room)))))

(cl-defmethod slack-team-set-channels ((this slack-team) channels)
  (let ((table (oref this channels)))
    (cl-loop for channel in channels
             do (slack-if-let* ((old (gethash (oref channel id) table)))
                    (slack-merge old channel)
                  (puthash (oref channel id) channel table)))))

(cl-defmethod slack-team-set-groups ((this slack-team) groups)
  (let ((table (oref this groups)))
    (cl-loop for group in groups
             do (slack-if-let* ((old (gethash (oref group id) table)))
                    (slack-merge old group)
                  (puthash (oref group id) group table)))))

(cl-defmethod slack-team-set-ims ((this slack-team) ims)
  (let ((table (oref this ims)))
    (cl-loop for im in ims
             do (slack-if-let* ((old (gethash (oref im id) table)))
                    (slack-merge old im)
                  (puthash (oref im id) im table)))))

(provide 'slack-room)
;;; slack-room.el ends here
