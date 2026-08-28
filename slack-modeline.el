;;; slack-modeline.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  南優也

;; Author: 南優也 <yuya373@yuya373noMacBook-Pro.local>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'slack-team)
(require 'slack-counts)
(require 'slack-room)

(defvar slack-modeline nil)

(defcustom slack-enable-global-mode-string nil
  "If true, add `slack-modeline' to `global-mode-string'"
  :type 'boolean
  :group 'slack)

(defcustom slack-modeline-formatter #'slack-default-modeline-formatter
  "Format modeline with Arg '((team-name . (has-unreads . mention-count)))."
  :type 'function
  :group 'slack)

(defface slack-modeline-has-unreads-face
  '((t (:weight bold :foreground "#d33682")))
  "Face used to team has unreads message in modeline"
  :group 'slack)

(defface slack-modeline-thread-has-unreads-face
  '((t (:weight bold :foreground "#d33682")))
  "Face used to thread has unreads message in modeline"
  :group 'slack)

(defface slack-modeline-channel-has-unreads-face
  '((t (:weight bold :foreground "#d33682")))
  "Face used to channel has unreads message in modeline"
  :group 'slack)

(defun slack-default-modeline-formatter (alist)
  "Element in ALIST is  '((team-name . ((thread . (has-unreads . mention-count)) (channel . (has-unreads . mention-count)))))"
  (mapconcat #'(lambda (e)
                 (let* ((team-name (car e))
                        (summary (cdr e))
                        (thread (cdr (cl-assoc 'thread summary)))
                        (channel (cdr (cl-assoc 'channel summary)))
                        (thread-has-unreads (car thread))
                        (channel-has-unreads (car channel))
                        (has-unreads (or thread-has-unreads
                                         channel-has-unreads))
                        (thread-mention-count (cdr thread))
                        (channel-mention-count (cdr channel)))
                   (format "[ %s: %s, %s ]"
                           (if has-unreads
                               (propertize team-name
                                           'face 'slack-modeline-has-unreads-face)
                             team-name)
                           (if (or channel-has-unreads (< 0 channel-mention-count))
                               (propertize (number-to-string channel-mention-count)
                                           'face 'slack-modeline-channel-has-unreads-face)
                             channel-mention-count)
                           (if (or thread-has-unreads (< 0 thread-mention-count))
                               (propertize (number-to-string thread-mention-count)
                                           'face 'slack-modeline-thread-has-unreads-face)
                             thread-mention-count))))
             alist " "))

(defun slack-enable-modeline ()
  (when slack-enable-global-mode-string
    (add-to-list 'global-mode-string '(:eval slack-modeline) t)))

(defun slack-update-modeline ()
  (interactive)
  (let ((teams (cl-remove-if-not #'slack-team-modeline-enabledp
                                 (hash-table-values slack-teams-by-token))))
    (when (< 0 (length teams))
      (setq slack-modeline
            (funcall slack-modeline-formatter
                     (mapcar #'(lambda (e)
                                 (cons (or (oref e modeline-name)
                                           (slack-team-name e))
                                       (slack-team-counts-summary e)))
                             teams)))
      (force-mode-line-update))))

(defun slack-team-counts-summary (team)
  (with-slots (counts) team
    (if counts
        (with-slots (threads channels mpims ims) counts
          (let ((thread (cons (oref threads has-unreads)
                              (oref threads mention-count)))
                (channel (slack-modeline--conversation-summary
                          team channels mpims ims)))
            (list (cons 'thread thread)
                  (cons 'channel channel))))
      (list (cons 'thread (cons nil 0))
            (cons 'channel (cons nil 0))))))

(defun slack-modeline--conversation-summary (team channels mpims ims)
  "Return (has-unreads . mention-count) for TEAM's conversation counts.
CHANNELS, MPIMS and IMS are the per-conversation count lists of a
`slack-counts' object.  When
`slack-modeline-count-only-subscribed-channel' is non-nil, only
conversations whose room satisfies `slack-room-subscribedp' are
counted; otherwise every conversation is counted.  A conversation
whose room is not yet loaded (see `slack-room-find') is skipped
while the filter is active, since its subscription cannot be
decided."
  (let (unreads
        (total 0))
    (dolist (cc (append channels mpims ims))
      (let ((room (slack-room-find (oref cc id) team)))
        (when (or (not slack-modeline-count-only-subscribed-channel)
                  (and room (slack-room-subscribedp room team)))
          (cl-incf total (oref cc mention-count))
          (when (and (oref cc has-unreads) (null unreads))
            (setq unreads t)))))
    (cons unreads total)))

(cl-defmethod slack-counts-update ((team slack-team))
  "Update counts for TEAM."
  (slack-client-counts team
                       #'(lambda (counts)
                           (oset team counts counts)
                           (when (slack-team-modeline-enabledp team)
                             (slack-update-modeline)))))

(provide 'slack-modeline)
;;; slack-modeline.el ends here
