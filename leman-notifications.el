;;; leman-notifications.el --- Notifications support  -*- lexical-binding: t; -*-

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

;; This library implements support for Matrix notifications.  It differs from
;; `leman-notify', which implements a kind of bespoke notification system for events
;; received via sync requests rather than Matrix's own notifications endpoint.  These two
;; libraries currently integrate somewhat, as newly arriving events are handled and
;; notified about by `leman-notify', and old notifications are fetched and listed by
;; `leman-notifications' in the same "*Leman Notifications*" buffer.

;; In the future, these libraries will likely be consolidated and enhanced to more closely
;; follow the Matrix API's and Element client's examples.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'map)

(require 'leman-lib)
(require 'leman-room)
(require 'leman-notify)

;;;; Structs

(cl-defstruct leman-notification
  "Represents a Matrix notification."
  room-id event readp)

(defun leman-notifications--make (notification)
  "Return an `leman-notification' struct for NOTIFICATION.
NOTIFICATION is an alist representing a notification returned
from the \"/notifications\" endpoint.  The notification's event
is passed through `leman--make-event'."
  (declare (function leman--make-event "leman"))
  (pcase-let (((map room_id _actions _ts event read) notification))
    (make-leman-notification :room-id room_id :readp read
                             :event (leman--make-event event))))

;;;; Variables

(declare-function leman-room-list "leman-room-list")
(defvar leman-notifications-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") #'leman-notifications-jump)
    (define-key map [mouse-1] #'leman-notifications-jump-mouse)
    (define-key map [mouse-2] #'leman-notifications-jump-mouse)
    (define-key map (kbd "S-<return>") #'leman-notify-reply)
    (define-key map (kbd "M-g M-l") #'leman-room-list)
    (define-key map (kbd "M-g M-m") #'leman-notify-switch-to-mentions-buffer)
    (define-key map (kbd "M-g M-n") #'leman-notify-switch-to-notifications-buffer)
    (define-key map [remap scroll-down-command] #'leman-notifications-scroll-down-command)
    (define-key map [remap mwheel-scroll] #'leman-notifications-mwheel-scroll)
    (make-composed-keymap (list map) 'view-mode-map))
  "Map for Leman notification buffers.")

(cl-defun leman-notifications-jump (&optional (pos (point)))
  "Jump to Matrix event at POS."
  (interactive)
  (let ((session (get-text-property pos 'session))
        (room (get-text-property pos 'room))
        (event (get-text-property pos 'event)))
    (leman-view-room room session)
    (leman-room-goto-event event)))

(defun leman-notifications-jump-mouse (event)
  "Jump to Matrix event at EVENT."
  (interactive "e")
  (let ((pos (posn-point (event-start event))))
    (if (button-at pos)
        (push-button pos)
      (leman-notifications-jump pos))))

(defvar leman-notifications-hook '(leman-notifications-log-to-buffer)
  "Functions called for `leman-notifications' notifications.
Each function is called with two arguments, the session and the
`leman-notification' struct.")

(defvar-local leman-notifications-retro-loading nil
  "Non-nil when earlier messages are being loaded.
Used to avoid overlapping requests.")

(defvar-local leman-notifications-metadata nil
  "Metadata for `leman-notifications' buffers.")

;; Variables from other files.
(defvar leman-ewoc)
(defvar leman-session)
(defvar leman-notify-prism-background)
(defvar leman-room-message-format-spec)
(defvar leman-room-sender-in-left-margin)

;;;; Commands

;;;###autoload
(cl-defun leman-notifications
    (session &key from limit only
             (then (apply-partially #'leman-notifications-callback session)) else)
  "Show the notifications buffer for SESSION.
FROM may be a \"next_token\" token from a previous request.
LIMIT may be a maximum number of events to return.  ONLY may be
the string \"highlight\" to only return notifications that have
the highlight tweak set.  THEN and ELSE may be callbacks passed
to `leman-api', which see."
  (interactive (list (leman-complete-session)
                     :only (when current-prefix-arg
                             "highlight")))
  (if-let ((buffer (get-buffer "*Leman Notifications*")))
      (switch-to-buffer buffer)
    (let ((endpoint "notifications")
          (params (remq nil
                        (list (when from
                                (list "from" from))
                              (when limit
                                (list "limit" (number-to-string limit)))
                              (when only
                                (list "only" only))))))
      (leman-api session endpoint :params params :then then :else else)
      (leman-message "Fetching notifications for <%s>..." (leman-user-id (leman-session-user session))))))

(cl-defun leman-notifications-callback (session data &key (buffer (leman-notifications--log-buffer)))
  "Callback for `leman-notifications' on SESSION which receives DATA."
  (pcase-let (((map notifications next_token) data))
    (with-current-buffer buffer
      (setf (map-elt leman-notifications-metadata :next-token) next_token)
      (cl-loop for notification across notifications
               do (run-hook-with-args 'leman-notifications-hook
                                      session (leman-notifications--make notification)))
      (switch-to-buffer (current-buffer)))))

(defun leman-notifications-scroll-down-command ()
  "Scroll down, and load NUMBER earlier messages when at top."
  (interactive)
  (condition-case _err
      (scroll-down nil)
    (beginning-of-buffer
     (call-interactively #'leman-notifications-retro))))

(defun leman-notifications-mwheel-scroll (event)
  "Scroll according to EVENT, loading earlier messages when at top."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (mwheel-scroll event)
    (when (= (point-min) (window-start))
      (call-interactively #'leman-notifications-retro))))

(cl-defun leman-notifications-retro (session number)
  ;; FIXME: Naming things is hard.
  "Retrieve NUMBER older notifications on SESSION."
  ;; FIXME: Support multiple sessions.
  (interactive (list (leman-complete-session)
                     (cl-typecase current-prefix-arg
                       (null 100)
                       (list (read-number "Number of messages: "))
                       (number current-prefix-arg))))
  (cl-assert (eq 'leman-notifications-mode major-mode))
  (cl-assert (map-elt leman-notifications-metadata :next-token) nil
             "No more notifications for %s" (leman-user-id (leman-session-user leman-session)))
  (let ((buffer (current-buffer)))
    (unless leman-notifications-retro-loading
      (leman-notifications
       session :limit number
       :from (map-elt leman-notifications-metadata :next-token)
       ;; TODO: Use a :finally for resetting `leman-notifications-retro-loading'?
       :then (lambda (data)
               (unwind-protect
                   (leman-notifications-callback session data :buffer buffer)
                 (setf (buffer-local-value 'leman-notifications-retro-loading buffer) nil)))
       :else (lambda (plz-error)
               (setf (buffer-local-value 'leman-notifications-retro-loading buffer) nil)
               (leman-api-error plz-error)))
      (leman-message "Loading %s earlier messages..." number)
      (setf leman-notifications-retro-loading t))))

;;;; Functions

(cl-defun leman-notifications-log-to-buffer (session notification &key (buffer-name "*Leman Notifications*"))
  "Log EVENT in ROOM on SESSION to \"*Leman NOTIFICATIONS*\" buffer."
  (with-demoted-errors "leman-notifications-log-to-buffer: %S"
    (with-current-buffer (leman-notifications--log-buffer :name buffer-name)
      (save-window-excursion
        (when-let ((buffer-window (get-buffer-window (current-buffer))))
          ;; Select the buffer's window to avoid EWOC bug.  (See #191.)
          (select-window buffer-window))
        ;; TODO: Use the :readp slot to mark unread events.
        (save-mark-and-excursion
          (pcase-let* (((cl-struct leman-notification room-id event) notification)
                       (leman-session session)
                       (leman-room (or (cl-find room-id (leman-session-rooms session)
                                                :key #'leman-room-id :test #'equal)
                                       (error "leman-notifications-log-to-buffer: Can't find room <%s>; discarding notification" room-id)))
                       (leman-room-sender-in-left-margin nil)
                       (leman-room-message-format-spec "%o%O »%W %S> %B%R%t")
                       (new-node (leman-room--insert-event event))
                       (inhibit-read-only t)
                       (start) (end))
            (ewoc-goto-node leman-ewoc new-node)
            ;; Apply the button properties only to the room and sender names,
            ;; allowing buttons in the rest of the message to remain separate.
            (setf start (point)
                  end (save-excursion
                        (re-search-forward (rx "> "))))
            (add-text-properties start end '( button (t)
                                              category default-button
                                              action leman-notify-button-action))
            ;; Apply the session, room, and event properties to the whole event.
            (setf end (save-excursion
                        (if-let ((next-node (ewoc-next leman-ewoc new-node)))
                            (ewoc-location next-node)
                          (point-max))))
            (add-text-properties start end
                                 (list 'session session
                                       'room leman-room
                                       'event event))
            ;; Remove button face property from the whole event.
            (alter-text-property start end 'face
                                 (lambda (face)
                                   (pcase face
                                     ('button nil)
                                     ((pred listp) (remq 'button face))
                                     (_ face))))
            (when leman-notify-prism-background
              (add-face-text-property start end (list :background (leman-notifications--room-background-color leman-room)
                                                      :extend t)))))))))

(defun leman-notifications--room-background-color (room)
  "Return a background color on which to display ROOM's messages."
  (or (alist-get 'notify-background-color (leman-room-local room))
      (setf (alist-get 'notify-background-color (leman-room-local room))
            (let ((color (color-desaturate-name
                          (leman--prism-color (leman-room-id room) :contrast-with (face-foreground 'default))
                          50)))
              (if (leman--color-dark-p (color-name-to-rgb (face-background 'default)))
                  (color-darken-name color 25)
                (color-lighten-name color 25))))))

(cl-defun leman-notifications--log-buffer (&key (name "*Leman Notifications*"))
  "Return an Leman notifications buffer named NAME."
  (or (get-buffer name)
      (with-current-buffer (get-buffer-create name)
        (leman-notifications-mode)
        (current-buffer))))

;;;; Mode

(define-derived-mode leman-notifications-mode leman-room-mode "Leman Notifications"
  (setf leman-room-sender-in-left-margin nil
        left-margin-width 0
        right-margin-width 8)
  (setq-local leman-room-message-format-spec "[%o%O] %S> %B%R%t"
              bookmark-make-record-function #'leman-notifications-bookmark-make-record))

;;;; Bookmark support

(require 'bookmark)

(defun leman-notifications-bookmark-make-record ()
  "Return a bookmark record for the current `leman-notifications' buffer."
  (list (buffer-name)
        ;; It seems silly to have to record the buffer name twice, but the
        ;; `bookmark-make-record' function seems to override the bookmark name sometimes,
        ;; which makes the result useless unless we save the buffer name separately.
        (cons 'buffer-name (buffer-name))
        (cons 'handler #'leman-notifications-bookmark-handler)))

(defun leman-notifications-bookmark-handler (_bookmark)
  "Show `leman-notifications' buffer for BOOKMARK."
  ;; FIXME: Handle multiple sessions.
  ;; FIXME: This doesn't work quite correctly when the buffer isn't already open, because
  ;; the command is asynchronous in that case, so the buffer can be displayed in the wrong
  ;; window.  Fixing this would be hacky and awkward, but a partial solution is probably
  ;; possible.
  (leman-notifications (leman-complete-session)))

;;; Footer

(provide 'leman-notifications)

;;; leman-notifications.el ends here
