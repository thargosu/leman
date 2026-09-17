;;; leman-notify.el --- Notifications for Leman events  -*- lexical-binding: t; -*-

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

;; This library implements notifications for Leman events.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'map)
(require 'notifications)

(require 'leman-lib)
(require 'leman-room)

(eval-when-compile
  (require 'leman-structs))

;;;; Variables

(defun leman-notify--dbus-service-available-p ()
  "Return non-nil when a notification service is usable on the session bus.
Calls the service's GetServerInformation method rather than
`dbus-ping', whose Peer.Ping probe is not answered by all
notification services (e.g. awesome's), which would make leman
permanently conclude that no service is available."
  (and (featurep 'dbusbind)
       (require 'dbus nil :no-error)
       (dbus-ignore-errors (dbus-get-unique-name :session))
       (dbus-ignore-errors
         (dbus-call-method :session "org.freedesktop.Notifications"
                           "/org/freedesktop/Notifications"
                           "org.freedesktop.Notifications"
                           "GetServerInformation"
                           :timeout 2000))))

(defvar leman-notify-dbus-p
  (leman-notify--dbus-service-available-p)
  "Whether D-Bus notifications are usable.
Detected once at load time; when it failed (e.g. Emacs starts as a
daemon before the desktop's notification service),
`leman-notify--dbus-recheck' retries periodically.")

(defvar leman-notify-dbus-next-check nil
  "Next time `leman-notify--dbus-recheck' retries the D-Bus ping.")

(defcustom leman-notify-dbus-retry-interval 300
  "Seconds between retries of the D-Bus notification service ping.
When the service wasn't reachable at load time (e.g. Emacs starts
as a daemon before the desktop's notification service), leman
retries at this interval until it succeeds."
  :type 'natnum
  :group 'leman-notify)

;;;; Customization

(defgroup leman-notify nil
  "Notification options."
  :group 'leman)

(defcustom leman-notify-ignore-predicates
  '(leman-notify--event-not-message-p leman-notify--event-from-session-user-p)
  "Display notification if none of these return non-nil for an event.
Each predicate is called with three arguments: the event, the
room, and the session (each the respective struct)."
  :type '(repeat (choice (function-item leman-notify--event-not-message-p)
                         (function-item leman-notify--event-from-session-user-p)
                         (function :tag "Custom predicate"))))

(defcustom leman-notify-log-predicates
  '(leman-notify--event-mentions-session-user-p
    leman-notify--event-mentions-room-p
    leman-notify--room-buffer-live-p
    leman-notify--room-unread-p)
  "Predicates to determine whether to log an event to the notifications buffer.
If one of these returns non-nil for an event, the event is logged."
  :type 'hook
  :options '(leman-notify--event-mentions-session-user-p
             leman-notify--event-mentions-room-p
             leman-notify--room-buffer-live-p
             leman-notify--room-unread-p))

(defcustom leman-notify-mark-frame-urgent-predicates
  '(leman-notify--event-mentions-session-user-p
    leman-notify--event-mentions-room-p)
  "Predicates to determine whether to mark a frame as urgent.
If one of these returns non-nil for an event, the frame that most
recently showed the event's room's buffer is marked
urgent.  (Only works on X, not other GUI platforms.)"
  :type 'hook
  :options '(leman-notify--event-mentions-session-user-p
             leman-notify--event-mentions-room-p))

(defcustom leman-notify-mention-predicates
  '(leman-notify--event-mentions-session-user-p
    leman-notify--event-mentions-room-p)
  "Predicates to determine whether to log an event to the mentions buffer.
If one of these returns non-nil for an event, the event is logged."
  :type 'hook
  :options '(leman-notify--event-mentions-session-user-p
             leman-notify--event-mentions-room-p))

(defcustom leman-notify-notification-predicates
  '(leman-notify--event-mentions-session-user-p
    leman-notify--event-mentions-room-p
    leman-notify--room-buffer-live-p
    leman-notify--room-unread-p)
  "Predicates to determine whether to send a desktop notification.
If one of these returns non-nil for an event, the notification is sent."
  :type 'hook
  :options '(leman-notify--event-mentions-session-user-p
             leman-notify--event-mentions-room-p
             leman-notify--room-buffer-live-p
             leman-notify--room-unread-p))

(defcustom leman-notify-sound nil
  "Sound to play for notifications."
  :type '(choice (file :tag "Sound file")
                 (string :tag "XDG sound name")
                 (const :tag "Default XDG message sound" "message-new-instant")
                 (const :tag "Don't play a sound" nil)))

(defcustom leman-notify-limit-room-name-width nil
  "Limit the width of room display names in mentions and notifications buffers.
This prevents the margin from being made excessively wide."
  :type '(choice (integer :tag "Maximum width")
                 (const :tag "Unlimited width" nil)))

(defcustom leman-notify-prism-background nil
  "Add distinct background color by room to messages in notification buffers.
The color is specific to each room, generated automatically, and
can help distinguish messages by room."
  :type 'boolean)

(defcustom leman-notify-room-avatars t
  "Show room avatars in the notifications buffers.
This shows room avatars at the left of the window margin in
notification buffers.  It's not customizable beyond that due to
limitations and complexities of displaying strings and images in
margins in Emacs.  But it's useful, anyway."
  :type 'boolean)

(defcustom leman-notify-app-icon
  (ignore-errors
    (expand-file-name "assets/leman.svg"
                      (file-name-directory (locate-library "leman-notify"))))
  "File used as the desktop notification's app icon.
Shown when the event's room has no avatar; otherwise the room's
avatar is shown.  Set to nil to send no icon."
  :type '(choice (file :tag "Icon file")
                 (const :tag "No icon" nil)))

;;;; Commands

(declare-function leman-room-goto-event "leman-room")
(defun leman-notify-button-action (button)
  "Show BUTTON's event in its room buffer."
  ;; TODO: Is `interactive' necessary here?
  (interactive)
  (let* ((session (button-get button 'session))
         (room (button-get button 'room))
         (event (button-get button 'event)))
    (leman-view-room room session)
    (leman-room-goto-event event)))

(defun leman-notify-reply ()
  "Send a reply to event at point."
  (interactive)
  (save-window-excursion
    ;; Not sure why `call-interactively' doesn't work for `push-button' but oh well.
    (push-button)
    (call-interactively #'leman-room-write-reply)))

(defun leman-notify-switch-to-notifications-buffer ()
  "Switch to \"*Leman Notifications*\" buffer."
  (declare (function leman-notifications "leman-notifications"))
  (interactive)
  (call-interactively #'leman-notifications))

(defvar leman-notifications-mode-map)
(defun leman-notify-switch-to-mentions-buffer ()
  "Switch to \"*Leman Mentions*\" buffer."
  (declare (function leman-notifications--log-buffer "leman-notifications"))
  (interactive)
  (switch-to-buffer (leman-notifications--log-buffer :name "*Leman Mentions*"))
  ;; HACK: Undo remapping of scroll commands which don't apply in this buffer.
  (let ((map (copy-keymap leman-notifications-mode-map)))
    (define-key map [remap scroll-down-command] nil)
    (define-key map [remap mwheel-scroll] nil)
    (use-local-map map)))

;;;; Functions

(defun leman-notify--dbus-recheck ()
  "Return non-nil when D-Bus notifications are usable.
When the load-time detection failed, retry at most every
`leman-notify-dbus-retry-interval' seconds, so that starting Emacs
before the desktop's notification service doesn't disable
notifications for the whole session."
  (when (and (not leman-notify-dbus-p)
             (or (null leman-notify-dbus-next-check)
                 (time-less-p leman-notify-dbus-next-check (current-time))))
    (setf leman-notify-dbus-next-check
          (time-add (current-time) leman-notify-dbus-retry-interval))
    (setf leman-notify-dbus-p (leman-notify--dbus-service-available-p)))
  leman-notify-dbus-p)

(defun leman-notify (event room session)
  "Send notifications for EVENT in ROOM on SESSION.
Sends if all of `leman-notify-ignore-predicates' return nil.
Does not do anything if session hasn't finished initial sync."
  (with-demoted-errors "leman-notify: Error: %S"
    (when (and (leman-session-has-synced-p session)
               (cl-loop for pred in leman-notify-ignore-predicates
                        never (funcall pred event room session)))
      (when (and (leman-notify--dbus-recheck)
                 (run-hook-with-args-until-success 'leman-notify-notification-predicates event room session))
        (leman-notify--notifications-notify event room session))
      (when (run-hook-with-args-until-success 'leman-notify-log-predicates event room session)
        (leman-notify--log-to-buffer event room session))
      (when (run-hook-with-args-until-success 'leman-notify-mention-predicates event room session)
        (leman-notify--log-to-buffer event room session :buffer-name "*Leman Mentions*"))
      (when (run-hook-with-args-until-success 'leman-notify-mark-frame-urgent-predicates event room session)
        (leman-notify--mark-frame-urgent event room session)))))

(defun leman-notify--mark-frame-urgent (_event room _session)
  "Mark frame showing ROOM's buffer as urgent.
If ROOM has no existing buffer, do nothing."
  (declare
   ;; These silence lint warnings on our GitHub CI runs, which use a build of Emacs
   ;; without GUI support.
   (function dbus-get-unique-name "dbusbind.c")
   (function x-change-window-property "xfns.c")
   (function x-window-property "xfns.c"))
  (cl-labels ((mark-frame-urgent (frame)
                (let* ((prop "WM_HINTS")
                       (hints (cl-coerce
                               (x-window-property prop frame prop nil nil t)
                               'list)))
                  (setf (car hints) (logior (car hints) 256))
                  (x-change-window-property prop hints nil prop 32 t))))
    (when-let* ((buffer (alist-get 'buffer (leman-room-local room)))
                (frames (cl-loop for frame in (frame-list)
                                 when (eq 'x (framep frame))
                                 collect frame))
                (frame (pcase (length frames)
                         (1 (car frames))
                         (_
                          ;; Use the frame that most recently showed ROOM's buffer.
                          (car (sort frames
                                     (lambda (frame-a frame-b)
                                       (let ((a-pos (cl-position buffer (buffer-list frame-a)))
                                             (b-pos (cl-position buffer (buffer-list frame-b))))
                                         (cond ((and a-pos b-pos)
                                                (< a-pos b-pos))
                                               (a-pos)
                                               (b-pos))))))))))
      (mark-frame-urgent frame))))

(defun leman-notify--notifications-notify (event room _session)
  "Call `notifications-notify' for EVENT in ROOM on SESSION."
  (pcase-let* (((cl-struct leman-event sender content) event)
               ((cl-struct leman-room avatar (display-name room-displayname)) room)
               ((map body) content)
               (room-name (or room-displayname (leman--room-display-name room)))
               (sender-name (leman--user-displayname-in room sender))
               (title (format "%s in %s" sender-name room-name)))
    ;; TODO: Encode HTML entities.
    (when (stringp body)
      ;; If event has no body, it was probably redacted or something, so don't notify.
      (setf body (truncate-string-to-width body 60))
      (notifications-notify :title title :body body
                            :app-name "Léman"
                            :app-icon (or (when avatar
                                            (leman-notify--temp-file
                                             (plist-get (cdr (get-text-property 0 'display avatar)) :data)))
                                          leman-notify-app-icon)
                            :category "im.received"
                            :timeout 5000
                            ;; FIXME: Using :sound-file seems to do nothing, ever.  Maybe a bug in notifications-notify?
                            :sound-file (when (and leman-notify-sound
                                                   (file-name-absolute-p leman-notify-sound))
                                          leman-notify-sound)
                            :sound-name (when (and leman-notify-sound
                                                   (not (file-name-absolute-p leman-notify-sound)))
                                          leman-notify-sound)
                            ;; TODO: Show when action used.
                            ;; :actions '("default" "Show")
                            ;; :on-action #'leman-notify-show
                            ))))

(cl-defun leman-notify--temp-file (content &key (timeout 5))
  "Return a filename holding CONTENT, and delete it after TIMEOUT seconds."
  (let ((filename (make-temp-file "leman-notify--temp-file-"))
        (coding-system-for-write 'no-conversion))
    (with-temp-file filename
      (insert content))
    (run-at-time timeout nil (lambda ()
                               (delete-file filename)))
    filename))

(cl-defun leman-notify--log-to-buffer (event room session &key (buffer-name "*Leman Notifications*"))
  "Log EVENT in ROOM on SESSION to \"*Leman Notifications*\" buffer."
  (declare (function leman-notifications-log-to-buffer "leman-notifications")
           (function make-leman-notification "leman-notifications"))
  (pcase-let* (((cl-struct leman-room (id room-id)) room)
               (notification (make-leman-notification :room-id room-id :event event)))
    (leman-notifications-log-to-buffer session notification :buffer-name buffer-name)))

;;;;; Predicates

(defun leman-notify--event-mentions-session-user-p (event room session)
  "Return non-nil if EVENT in ROOM mentions SESSION's user.
If EVENT's sender is SESSION's user, returns nil."
  (pcase-let* (((cl-struct leman-session user) session)
               ((cl-struct leman-event sender) event))
    (unless (equal (leman-user-id user) (leman-user-id sender))
      (leman-room--event-mentions-user-p event user room))))

(defun leman-notify--room-buffer-live-p (_event room _session)
  "Return non-nil if ROOM has a live buffer."
  (buffer-live-p (alist-get 'buffer (leman-room-local room))))

(defun leman-notify--room-unread-p (_event room _session)
  "Return non-nil if ROOM has unread notifications.
According to the room's notification configuration on the server."
  (pcase-let* (((cl-struct leman-room unread-notifications) room)
               ((map notification_count highlight_count) unread-notifications))
    (not (and (equal 0 notification_count)
              (equal 0 highlight_count)))))

(defun leman-notify--event-message-p (event _room _session)
  "Return non-nil if EVENT is an \"m.room.message\" event."
  (equal "m.room.message" (leman-event-type event)))

(defun leman-notify--event-not-message-p (event _room _session)
  "Return non-nil if EVENT is not an \"m.room.message\" event."
  (not (equal "m.room.message" (leman-event-type event))))

(defun leman-notify--event-from-session-user-p (event _room session)
  "Return non-nil if EVENT is sent by SESSION's user."
  (equal (leman-user-id (leman-session-user session))
         (leman-user-id (leman-event-sender event))))

(defalias 'leman-notify--event-mentions-room-p #'leman--event-mentions-room-p)

;;;; Bookmark support

;; Especially useful with Burly: <https://github.com/alphapapa/burly.el>

(require 'bookmark)

(defun leman-notify-bookmark-make-record ()
  "Return a bookmark record for the current `leman-notify' buffer."
  (list (buffer-name)
        ;; It seems silly to have to record the buffer name twice, but the
        ;; `bookmark-make-record' function seems to override the bookmark name sometimes,
        ;; which makes the result useless unless we save the buffer name separately.
        (cons 'buffer-name (buffer-name))
        (cons 'handler #'leman-notify-bookmark-handler)))

(defun leman-notify-bookmark-handler (bookmark)
  "Show Leman notifications buffer for BOOKMARK."
  (pcase-let ((`(,_bookmark-name . ,(map buffer-name)) bookmark))
    (switch-to-buffer (leman-notifications--log-buffer :name buffer-name))))

;;;; Footer

(provide 'leman-notify)

;;; leman-notify.el ends here
