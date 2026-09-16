;;; slack-stars-buffer.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2017  南優也

;; Author: 南優也 <yuyaminami@minamiyuuya-no-MacBook.local>
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
(require 'dash)
(require 'slack-util)
(require 'slack-team)
(require 'slack-buffer)
(require 'slack-message-buffer)
(require 'slack-star)
(require 'slack-message)

(define-derived-mode slack-stars-buffer-mode slack-buffer-mode "Slack Stars Buffer")

(defclass slack-stars-buffer (slack-buffer)
  ((oldest :type string :initform "")))

(cl-defmethod slack-buffer-name ((this slack-stars-buffer))
  (let ((team (slack-buffer-team this)))
    (format "*slack: %s : Stars*" (oref team name))))

(cl-defmethod slack-buffer-key ((_class (subclass slack-stars-buffer)) &rest _args)
  'slack-stars-buffer)

(cl-defmethod slack-buffer-key ((_this slack-stars-buffer))
  (slack-buffer-key 'slack-stars-buffer))

(cl-defmethod slack-team-buffer-key ((_class (subclass slack-stars-buffer)))
  'slack-stars-buffer)

(cl-defmethod slack-buffer-toggle-email-expand ((this slack-stars-buffer) file-id)
  (slack-if-let* ((team (slack-buffer-team this))
                  (ts (get-text-property (point) 'ts))
                  (items (slack-star-items (oref team star)))
                  (item (cl-find-if #'(lambda (e) (string= ts (slack-ts e)))
                                    items))
                  (file (slack-star-item-file item file-id)))
      (progn
        (oset file is-expanded (not (oref file is-expanded)))
        (slack-buffer--replace this ts))))

(cl-defmethod slack-buffer-insert ((this slack-stars-buffer) message &optional not-tracked-p)
  (let ((lui-time-stamp-format "[%Y-%m-%d %H:%M] ")
        (lui-time-stamp-time (seconds-to-time
                              (string-to-number
                               (slack-ts
                                message)))))
    (lui-insert-with-text-properties
     (slack-message-to-string message (slack-buffer-team this))
     'ts (slack-ts message)
     'team-id (oref (slack-buffer-team this) id)
     'room-id (oref message channel)
     'not-tracked-p not-tracked-p)
    (lui-insert "" t)))

(cl-defmethod slack-buffer-has-next-page-p ((this slack-stars-buffer))
  (let ((team (slack-buffer-team this)))
    (slack-star-has-next-page-p (oref team star))))

(cl-defmethod slack-buffer-insert-history ((this slack-stars-buffer))
  (let* ((team (slack-buffer-team this))
         (star-items (slack-star-items (oref team star)))
         (before-oldest (oref this oldest))
         (buffer (slack-buffer-buffer this)))
    (when star-items
      (oset this oldest (slack-ts (car star-items))))
    (dolist (i star-items)
      (slack-message-get-or-fetch-async
       (oref i ts) (oref i item-id) team nil
       (lambda (message)
         ;; Only render what is below the oldest boundary already shown;
         ;; everything else was inserted when the buffer was opened.
         (when (and message
                    (buffer-live-p buffer)
                    (string< (slack-ts message) before-oldest))
           (with-current-buffer buffer
             (slack-buffer-insert this message t)
             (slack-if-let* ((point (slack-buffer-ts-eq (point-min)
                                                        (point-max)
                                                        before-oldest)))
                 (goto-char point)))))))))

(cl-defmethod slack-buffer-update-oldest ((this slack-stars-buffer) item)
  "Track THIS's `oldest' slot, the oldest saved item shown so far.

The slot starts empty and is set from the first saved item when
the buffer is opened; loading an older page lowers it directly in
`slack-buffer-insert-history'.  ITEM can be a `slack-star-item' or
a message, both know their `slack-ts', so this never needs to
fetch anything."
  (when (and item (string< (oref this oldest) (slack-ts item)))
    (oset this oldest (slack-ts item))))

(cl-defmethod slack-buffer-request-history ((this slack-stars-buffer) after-success)
  (let ((team (slack-buffer-team this)))
    (slack-stars-list-request team
                              (oref (oref team star) cursor)
                              after-success)))

(cl-defmethod slack-buffer-init-buffer ((this slack-stars-buffer))
  (let* ((buf (cl-call-next-method))
         (team (slack-buffer-team this))
         (star (oref team star))
         (items (slack-star-items star)))
    (with-current-buffer buf
      (slack-stars-buffer-mode)
      (slack-buffer-set-current-buffer this)
      (slack-buffer-insert-load-more this)
      ;; Render placeholders right away and fill them as the
      ;; non-blocking fetches come back, instead of freezing Emacs
      ;; on a synchronous API call per saved message.
      (slack-stars-buffer--load-items this items)
      (goto-char (point-max)))
    buf))

(defun slack-stars-buffer--load-items (this items)
  "Insert placeholders for the saved ITEMS of THIS, then fill them.

The placeholders are replaced by the real messages as the
non-blocking fetches complete, so opening the buffer never blocks
on the API."
  (let ((team (slack-buffer-team this))
        (buffer (slack-buffer-buffer this)))
    (when items
      (slack-buffer-update-oldest this (car items)))
    (dolist (i items)
      (let ((ts (oref i ts))
            (room-id (oref i item-id)))
        (slack-stars-buffer--insert-placeholder this ts room-id)
        (slack-message-get-or-fetch-async
         ts room-id team nil
         (lambda (message)
           (when (buffer-live-p buffer)
             (slack-stars-buffer--fill-placeholder this ts message))))))))

(defun slack-stars-buffer--insert-placeholder (this ts room-id)
  "Insert a placeholder line for the starred message TS in ROOM-ID."
  (let ((team (slack-buffer-team this)))
    (with-current-buffer (slack-buffer-buffer this)
      (let ((lui-time-stamp-format "[%Y-%m-%d %H:%M] ")
            (lui-time-stamp-time (seconds-to-time
                                  (string-to-number ts))))
        (lui-insert-with-text-properties
         (concat slack-loading-message-string "\n")
         'ts ts
         'team-id (oref team id)
         'room-id room-id
         'not-tracked-p t)
        (lui-insert "" t)))))

(defun slack-stars-buffer--fill-placeholder (this ts message)
  "Render MESSAGE (or an unavailable notice) in place of the placeholder for TS.

The placeholder is matched by TS, the timestamp of the saved item,
not by the fetched MESSAGE's own timestamp: when the message only
exists as a thread reply, the fetch resolves it through
`conversations.replies' and both are the same, but a fetch that
finds nothing must still clear the placeholder."
  (let ((team (slack-buffer-team this)))
    (with-current-buffer (slack-buffer-buffer this)
      (lui-replace (if message
                       (slack-message-to-string message team)
                     (concat slack-unavailable-message-string "\n"))
                   (lambda ()
                     (string= (get-text-property (point) 'ts)
                              ts))))))

(defun slack-create-stars-buffer (team)
  (slack-if-let* ((buf (slack-buffer-find 'slack-stars-buffer team)))
      buf
    (make-instance 'slack-stars-buffer :team-id (oref team id))))

(cl-defmethod slack-buffer-remove-star ((this slack-stars-buffer) ts)
  "Remove THIS star at TS."
  (let ((team (slack-buffer-team this)))
    (with-slots (star) team
      (slack-star-remove-star star ts team))))

(cl-defmethod slack-buffer-message-delete ((this slack-stars-buffer) ts)
  (let ((buffer (slack-buffer-buffer this))
        (inhibit-read-only t))
    (with-current-buffer buffer
      (slack-if-let* ((beg (slack-buffer-ts-eq (point-min) (point-max) ts))
                      (end (next-single-property-change beg 'ts)))
          (delete-region beg end)))))

(cl-defmethod slack-buffer--replace ((this slack-stars-buffer) ts)
  (let* ((team (slack-buffer-team this))
         (buffer (slack-buffer-buffer this)))
    (slack-if-let* ((star-items (slack-star-items (oref team star)))
                    (star-item (-find (lambda (e) (string= ts (slack-ts e)))
                                      star-items)))
        (slack-message-get-or-fetch-async
         (oref star-item ts) (oref star-item item-id) team nil
         (lambda (message)
           (when (and message (buffer-live-p buffer))
             (with-current-buffer buffer
               (lui-replace (slack-message-to-string message team)
                            (lambda ()
                              (string= (get-text-property (point) 'ts)
                                       ts))))))))))

(defun slack-stars-list ()
  "Show the buffer with the saved for later messages."
  (interactive)
  (let* ((team (slack-team-select))
         (buf (slack-buffer-find 'slack-stars-buffer team)))
    (if buf (slack-buffer-display buf)
      (slack-stars-list-request
       team nil
       #'(lambda () (slack-buffer-display (slack-create-stars-buffer team)))))))

(defun slack-stars-open-message ()
  "Open message at point of activity-feed."
  (interactive)
  (if-let* ((ts (get-text-property (point) 'ts))
            (team-id (get-text-property (point) 'team-id))
            (room-id (get-text-property (point) 'room-id))
            (thread-ts (or (get-text-property (point) 'thread-ts) ts))
            (team (slack-team-find team-id)))
      (slack-open-message
       team
       (slack-room-find room-id team)
       (--find (s-matches-p "[0-9]" it) (list ts))
       ;; found out that when a ts is nil, it comes "nil"
       (--find (s-matches-p "[0-9]" it) (list thread-ts)))
    (error "Not possible to jump to message")))
(define-key slack-stars-buffer-mode-map (kbd "RET") 'slack-stars-open-message)

(define-key slack-stars-buffer-mode-map (kbd "K") 'slack-message-remove-star)

(defun slack-stars-refresh-buffer ()
  "Close and reopen star buffer to refresh contents."
  (interactive)
  (kill-buffer)
  (slack-stars-list))
(define-key slack-stars-buffer-mode-map (kbd "G") 'slack-stars-refresh-buffer)

(provide 'slack-stars-buffer)
;;; slack-stars-buffer.el ends here
