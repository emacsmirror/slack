;;; slack-export.el --- flattened room export -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrea <andrea-dev@hotmail.com>

;;; Commentary:

;; This is deliberately a snapshot, not another live Slack buffer.  It copies
;; the messages currently held by a room and puts replies underneath their
;; parent with indentation.  The normal command fetches missing replies first;
;; with a prefix argument it exports only what is already cached.

;;; Code:

(require 'dash)
(require 'subr-x)
(require 'slack-message-buffer)
(require 'slack-message-formatter)
(require 'slack-conversations)

(defgroup slack-export nil
  "Read-only flattened exports of Slack rooms."
  :group 'slack)

(defcustom slack-export-reply-indent "  "
  "Prefix used for every line of a thread reply in an export."
  :type 'string
  :group 'slack-export)

(defun slack-export--root-p (message)
  "Return non-nil when MESSAGE is not a thread reply."
  (not (slack-thread-message-p message)))

(defun slack-export--replies (messages root-ts)
  "Return loaded replies in MESSAGES belonging to ROOT-TS."
  (-sort (lambda (left right)
           (string< (slack-ts left) (slack-ts right)))
         (-filter (lambda (message)
                    (and (slack-thread-message-p message)
                         (equal root-ts (slack-thread-ts message))))
                  messages)))

(defun slack-export--roots-and-orphans (room)
  "Return `(ROOTS . ORPHANS)' for the messages currently held by ROOM.
ORPHANS are replies whose parent is not in ROOM.  They are still exported,
because this view promises to show every loaded message."
  (let* ((messages (slack-room-sorted-messages room))
         (roots (-filter #'slack-export--root-p messages))
         (root-ts (-map #'slack-ts roots))
         (orphans (-filter
                   (lambda (message)
                     (and (slack-thread-message-p message)
                          (not (-contains-p root-ts
                                             (slack-thread-ts message)))))
                   messages)))
    (cons roots orphans)))

(defun slack-export--missing-threads (room)
  "Return roots in ROOM whose reply count exceeds loaded replies."
  (-filter
   (lambda (root)
     (let ((reply-count (oref root reply-count)))
       (and (< 0 reply-count)
            (< (length (slack-export--replies
                        (slack-room-sorted-messages room)
                        (slack-ts root)))
               reply-count))))
   (car (slack-export--roots-and-orphans room))))

(defun slack-export--fetch-thread (root room team done)
  "Fetch every reply for ROOT, then call DONE.
The Slack endpoint is paginated.  Each page is stored in ROOM immediately,
while the root's `replies' list is rebuilt from the room store so duplicate
pages or replies received from the websocket do not accumulate."
  (let ((thread-ts (slack-ts root)))
    (cl-labels
        ((page (cursor)
           (slack-conversations-replies
            room thread-ts team
            :cursor cursor
            :oldest thread-ts
            :after-success
            (lambda (messages next-cursor _has-more)
              (slack-room-set-messages room messages team)
              (slack-message-set-replies
               room thread-ts
               (slack-export--replies
                (slack-room-sorted-messages room)
                thread-ts))
              (if (and next-cursor
                       (not (string-empty-p next-cursor)))
                  (page next-cursor)
                (funcall done))))))
      (page nil))))

(defun slack-export--fetch-missing-threads (room team done)
  "Fetch missing replies for ROOM one thread at a time, then call DONE."
  (let ((pending (slack-export--missing-threads room))
        (total nil)
        (completed 0))
    (setq total (length pending))
    (if (zerop total)
        (funcall done)
      (cl-labels
          ((next ()
             (if (null pending)
                 (funcall done)
               (let ((root (pop pending)))
                 (cl-incf completed)
                 (message "Loading thread %d/%d in %s..."
                          completed total
                          (slack-room-name room team))
                 (slack-export--fetch-thread root room team #'next)))))
        (next)))))

(defun slack-export--insert-message (message team &optional prefix)
  "Insert MESSAGE for TEAM, prefixing every line with PREFIX."
  (let ((text (slack-message-to-string message team))
        (timestamp (format-time-string
                    "[%Y-%m-%d %H:%M] "
                    (slack-message-time-stamp message))))
    (insert (or prefix "") timestamp)
    (insert (replace-regexp-in-string
             "^"
             (or prefix "")
             text
             t
             t))
    (insert "\n\n")))

(defun slack-export--insert-room (room team)
  "Insert the flattened snapshot of ROOM for TEAM at point."
  (let* ((roots-and-orphans (slack-export--roots-and-orphans room))
         (roots (car roots-and-orphans))
         (orphans (cdr roots-and-orphans))
         (messages (slack-room-sorted-messages room)))
    (dolist (root roots)
      (slack-export--insert-message root team)
      (dolist (reply (slack-export--replies messages (slack-ts root)))
        (slack-export--insert-message reply team slack-export-reply-indent)))
    (when orphans
      (insert "[Loaded replies whose parent is not loaded]\n\n")
      (dolist (reply orphans)
        (slack-export--insert-message reply team slack-export-reply-indent)))))

(defun slack-export--buffer-name (room team)
  "Return the snapshot buffer name for ROOM and TEAM."
  (format "*slack-export: %s / %s - %s*"
          (slack-team-name team)
          (slack-room-name room team)
          (format-time-string "%Y%m%d%H%M%S")))

(defun slack-export--display (room team)
  "Create and display a read-only flattened snapshot of ROOM."
  (let ((buffer (generate-new-buffer (slack-export--buffer-name room team))))
    (with-current-buffer buffer
      (fundamental-mode)
      (setq buffer-read-only nil)
      (slack-export--insert-room room team)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (pop-to-buffer buffer)
    (message "Exported %s" (slack-room-name room team))
    buffer))

;;;###autoload
(defun slack-export-room-with-threads (&optional cache-only)
  "Export the current room with thread replies indented.
Without CACHE-ONLY, fetch missing replies before creating the snapshot.  With a
prefix argument, export only replies already held in the room store."
  (interactive "P")
  (slack-if-let* ((source slack-current-buffer)
                  (team (slack-buffer-team source))
                  (room (slack-buffer-room source)))
      (if cache-only
          (slack-export--display room team)
        (message "Preparing export for %s..." (slack-room-name room team))
        (slack-export--fetch-missing-threads
         room team
         (lambda ()
           (slack-export--display room team))))
    (user-error "Run this command from a Slack room buffer")))

(provide 'slack-export)
;;; slack-export.el ends here
