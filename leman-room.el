;;; leman-room.el --- Leman room buffers             -*- lexical-binding: t; -*-

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

;; This library implements buffers displaying events in a room.

;; EWOC is a great library.  If I had known about it and learned it
;; sooner, it would have saved me a lot of time in other projects.
;; I'm glad I decided to try it for this one.

;;; Code:

;;;; Debugging

;; NOTE: Uncomment this form and `emacs-lisp-byte-compile-and-load' the file to enable
;; `leman-debug' messages.  This is commented out by default because, even though the
;; messages are only displayed when `warning-minimum-log-level' is `:debug' at runtime, if
;; that is so at expansion time, the expanded macro calls format the message and check the
;; log level at runtime, which is not zero-cost.

;; (eval-and-compile
;;   (setq-local warning-minimum-log-level nil)
;;   (setq-local warning-minimum-log-level :debug))

;;;; Requirements

(require 'color)
(require 'ewoc)
(require 'mailcap)
(require 'shr)
(require 'subr-x)
(require 'mwheel)
(require 'dnd)
(require 'url-cache)

(require 'leman-api)
(require 'leman-lib)
(require 'leman-macros)
(require 'leman-structs)

;;;; Structs

(cl-defstruct leman-room-membership-events
  "Struct grouping membership events.
After adding events, use `leman-room-membership-events--update'
to sort events and update other slots."
  (events nil :documentation "Membership events, latest first.")
  (earliest-ts nil :documentation "Timestamp of earliest event.")
  (latest-ts nil :documentation "Timestamp of latest event."))

(defun leman-room-membership-events--update (struct)
  "Return STRUCT having sorted its events and updated its slots."
  ;; Like the room timeline slot, events are sorted latest-first.  We also deduplicate
  ;; them , because it seems that we can end up with multiple copies of a membership event
  ;; (e.g. when loading old messages).
  (setf (leman-room-membership-events-events struct) (cl-delete-duplicates (leman-room-membership-events-events struct)
                                                                           :key #'leman-event-id :test #'equal)
        (leman-room-membership-events-events struct) (cl-sort (leman-room-membership-events-events struct) #'>
                                                              :key #'leman-event-origin-server-ts)
        (leman-room-membership-events-earliest-ts struct) (leman-event-origin-server-ts
                                                           (car (last (leman-room-membership-events-events struct))))
        (leman-room-membership-events-latest-ts struct) (leman-event-origin-server-ts
                                                         (car (leman-room-membership-events-events struct))))
  struct)

;;;; Variables

(defvar-local leman-ewoc nil
  "EWOC for Leman room buffers.")

(defvar-local leman-room nil
  "Leman room for current buffer.")

(defvar-local leman-session nil
  "Leman session for current buffer.")

;; TODO: Convert some of these buffer-local variables into keys in one buffer-local map variable.

(defvar-local leman-room-retro-loading nil
  "Non-nil when earlier messages are being loaded.
Used to avoid overlapping requests.")

(defvar-local leman-room-editing-event nil
  "When non-nil, the user is editing this event.
Used by `leman-room-send-message'.")

(defvar-local leman-room-replying-to-event nil
  "When non-nil, the user is replying to this event.
Used by `leman-room-send-message'.")

(defvar-local leman-room-replying-to-overlay nil
  "Used by `leman-room-write-reply'.")

(defvar-local leman-room-read-receipt-request nil
  "Maps event ID to request updating read receipt to that event.
An alist of one entry.")

(defvar leman-room-read-string-setup-hook nil
  "Normal hook run by `leman-room-read-string' after switching to minibuffer.
Should be used to, e.g. propagate variables to the minibuffer.")

(defvar leman-room-compose-hook nil
  "Hook run in compose buffers when created.
Used to, e.g. call `leman-room-compose-org'.")

(declare-function leman-room-list "leman-room-list")
(declare-function leman-view-space "leman-directory")
(declare-function leman-notify-switch-to-mentions-buffer "leman-notify")
(declare-function leman-notify-switch-to-notifications-buffer "leman-notify")
(declare-function leman--update-unread-indicator "leman")
(declare-function leman--make-event "leman")
(declare-function leman-e2ee-encrypt-file "leman-e2ee")
(declare-function leman-e2ee-decrypt-file "leman-e2ee")
(declare-function leman-e2ee--decrypt-event-struct "leman")
(declare-function leman-e2ee--retry-decryption "leman")

(defvar leman-room-mode-self-insert-keymap (make-sparse-keymap)
  "The `leman-room-mode' keymap under `leman-room-self-insert-mode'.

Set as the parent keymap of `leman-room-mode-effective-keymap'
when `leman-room-self-insert-mode' is enabled.

This keymap is derived from the `leman-room-self-insert-chars'
and `leman-room-self-insert-commands' user options, along with
`leman-room-mode-map-prefix-key' which provides access to the
full `leman-room-mode-map'.  (Non-conflicting key bindings from
`leman-room-mode-map' are also available directly).

This keymap is generated when `leman-room-self-insert-mode' is
enabled, and after customizing any of the above options when the
minor mode is enabled.

The hook `leman-room-mode-self-insert-keymap-update-hook' runs
after generating this keymap.

Note: Emacs bug#66792 may cause `describe-keymap' to include
unreachable key bindings from the parent `leman-room-mode-map' in
its help output.  This problem affects only the help, and we work
around it for the `leman-room-mode' help; but when viewing the
keymap directly the issue may be visible.")

(defvar leman-room-mode-map
  (let ((map (make-sparse-keymap))
        (prefixes '(("M-g" . "group:switching")
                    ("s" . "group:messages")
                    ("u" . "group:users")
                    ("r" . "group:room")
                    ("R" . "group:membership"))))
    ;; Use symbols for prefix maps so that `which-key' can display their names.
    (dolist (prefix prefixes)
      (let ((cmd (define-prefix-command (make-symbol (cdr prefix)))))
        (define-key map (kbd (car prefix)) cmd)))

    ;; Menu
    (define-key map (kbd "?") #'leman-room-transient)

    ;; Movement
    (define-key map (kbd "n") #'leman-room-goto-next)
    (define-key map (kbd "N") #'end-of-buffer)
    (define-key map (kbd "p") #'leman-room-goto-prev)
    (define-key map (kbd "SPC") #'leman-room-scroll-up-mark-read)
    (define-key map (kbd "S-SPC") #'leman-room-scroll-down-command)
    (define-key map (kbd "M-g M-p") #'leman-room-goto-fully-read-marker)
    (define-key map (kbd "m") #'leman-room-mark-read)
    (define-key map [remap scroll-down-command] #'leman-room-scroll-down-command)
    (define-key map [remap mwheel-scroll] #'leman-room-mwheel-scroll)
    (define-key map (kbd "<tab>") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)

    ;; Switching
    (define-key map (kbd "M-g M-l") #'leman-room-list)
    (define-key map (kbd "M-g M-r") #'leman-view-room)
    (define-key map (kbd "M-g M-m") #'leman-notify-switch-to-mentions-buffer)
    (define-key map (kbd "M-g M-n") #'leman-notify-switch-to-notifications-buffer)
    (define-key map (kbd "q") #'quit-window)

    ;; Messages
    (define-key map (kbd "RET") #'leman-room-dispatch-new-message)
    (define-key map (kbd "M-RET") #'leman-room-dispatch-new-message-alt)
    (define-key map (kbd "S-<return>") #'leman-room-dispatch-reply-to-message)
    (define-key map (kbd "<insert>") #'leman-room-dispatch-edit-message)
    (define-key map (kbd "C-k") #'leman-room-delete-message)
    (define-key map (kbd "s r") #'leman-room-send-reaction)
    (define-key map (kbd "s t") #'leman-room-view-thread)
    (define-key map (kbd "s e") #'leman-room-send-emote)
    (define-key map (kbd "s f") #'leman-room-send-file)
    (define-key map (kbd "s i") #'leman-room-send-image)
    (define-key map (kbd "v") #'leman-room-view-event)
    (define-key map (kbd "D") #'leman-room-download-file)

    ;; Users
    (define-key map (kbd "u RET") #'leman-send-direct-message)
    (define-key map (kbd "u i") #'leman-invite-user)
    (define-key map (kbd "u I") #'leman-ignore-user)

    ;; Room
    (define-key map (kbd "M-s o") #'leman-room-occur)
    (define-key map (kbd "r d") #'leman-describe-room)
    (define-key map (kbd "r m") #'leman-list-members)
    (define-key map (kbd "r t") #'leman-room-set-topic)
    (define-key map (kbd "r f") #'leman-room-set-message-format)
    (define-key map (kbd "r n") #'leman-room-set-notification-state)
    (define-key map (kbd "r N") #'leman-room-override-name)
    (define-key map (kbd "r T") #'leman-tag-room)

    ;; Room membership
    (define-key map (kbd "R c") #'leman-create-room)
    (define-key map (kbd "R j") #'leman-join-room)
    (define-key map (kbd "R l") #'leman-leave-room)
    (define-key map (kbd "R F") #'leman-forget-room)
    (define-key map (kbd "R n") #'leman-room-set-display-name)
    (define-key map (kbd "R s") #'leman-room-toggle-space)

    ;; Other
    (define-key map (kbd "g") #'leman-room-sync)
    map)
  "Keymap for Leman room buffers.")

(defvar leman-room-mode-effective-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map leman-room-mode-map)
    map)
  "The actual keymap used in `leman-room-mode'.

This keymap reflects the state of `leman-room-self-insert-mode',
with a parent of `leman-room-mode-map' when the mode is disabled,
or `leman-room-mode-self-insert-keymap' when the mode is enabled.")

(defvar leman-room-mode--advertised-keymap leman-room-mode-map
  "The keymap advertised by `leman-room-mode'.

This keymap should represent the functional behaviour of
`leman-room-mode-effective-keymap' without the confusion arising
from Emacs bug#66792 on account of the effective keymap having
`leman-room-mode-map' as a parent if `leman-room-self-insert-mode'
is enabled.

Because it does not always have `leman-room-mode-map' as a
parent, it is possible for that map to get out of sync with the
advertised map, but `leman-room-mode-self-insert-keymap-update'
makes a best effort to keep it accurate.")

(defvar leman-room-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "C-c '") #'leman-room-compose-from-minibuffer)
    map)
  "Keymap used in `leman-room-read-string'.")

(defvar leman-room-reaction-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'insert-char)
    (when (commandp 'emoji-insert)
      (define-key map "i" 'emoji-insert))
    (when (commandp 'emoji-search)
      (define-key map "s" 'emoji-search))
    (when (assoc "emoji" input-method-alist)
      (define-key map "m" 'leman-room-use-emoji-input-method))
    map)
  "Keymap used in `leman-room-send-reaction'.")

(defvar leman-room-sender-in-headers nil
  "Non-nil when sender is displayed in headers.
In that case, sender names are aligned to the margin edge.")

(defvar leman-room-messages-filter
  '((lazy_load_members . t))
  ;; NOTE: The confusing differences between what /sync and /messages
  ;; expect.  See <https://github.com/matrix-org/matrix-doc/issues/706>.
  "Default RoomEventFilter for /messages requests.")

(defvar leman-room-typing-timer nil
  "Timer used to send notifications while typing.")

(defvar leman-room-matrix.to-url-regexp
  (rx "http" (optional "s") "://"
      "matrix.to" "/#/"
      (group (1+ (not (any "/"))))
      (optional "/" (group "$" (1+ (not (any "?" "/")))))
      (optional "?" (group (1+ anything))))
  "Regexp matching \"matrix.to\" URLs.
The first group is the URL-encoded room ID, alias, or user ID;
the second, the optional event ID.  Both may be percent-encoded.")

(defcustom leman-room-animate-message-images t
  "Animate images in message bodies, if the format supports it.
Rooms containing many animated images can overload redisplay, in
which case setting this to nil shows their first frame instead."
  :type 'boolean
  :group 'leman-room-images)

(defvar leman-room-message-history nil
  "History list of messages entered with `leman-room' commands.
Does not include filenames, emotes, etc.")

(defvar leman-room-emote-history nil
  "History list of emotes entered with `leman-room' commands.")

;; Variables from other files.
(defvar leman-sessions)
(defvar leman-syncs)
(defvar leman-auto-sync)
(defvar leman-users)
(defvar leman-images-queue)
(defvar leman-notify-limit-room-name-width)
(defvar leman-view-room-display-buffer-action)

;; Defined later in this file.
(defvar leman-room-images)
(defvar leman-thread-root-id)

;; In-flight HTML image fetches: see `leman-room--fetch-html-image'.
(defvar leman-room--fetching-html-images nil
  "Alist of HTML image fetches already queued.
Each element is (URL . (EVENT . ROOM)...): the fetches currently
in flight, and the events to re-render when their image arrives.
Re-renders while a fetch is in flight register the event as
waiting instead of queueing a duplicate fetch for the same URL.")

;; Defined in Emacs 28.1: silence byte-compilation warning in earlier versions.
(defvar browse-url-handlers)

;;;; Customization

(defgroup leman-room-faces nil
  "Faces for room buffers."
  :group 'leman-room
  :group 'leman-faces)

(defgroup leman-room nil
  "Options for room buffers."
  :group 'leman)

(defcustom leman-room-timestamp-header-align 'right
  "Where to align timestamp headers."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Center" center)
                 (const :tag "Right" right)))

(defcustom leman-room-view-hook
  '(leman-room-view-hook-room-list-auto-update)
  "Functions called when `leman-room-view' is called.
Called with two arguments, the room and the session."
  :type 'hook)

(defcustom leman-room-reaction-emoji-max-size 20
  "Maximum width/height in pixels of custom-emoji images shown as reaction keys.
Custom emoji reactions (e.g. as sent by, e.g. Element) have
mxc:// URIs as their keys; the images are downloaded through the
session's authenticated media endpoint and shown at this size."
  :type 'natnum)

(defcustom leman-room-hide-redacted-message-content t
  "Hide content in redacted messages.
If nil, redacted messages' content will remain visible with a
strikethrough face until the session is terminated (a new session
will not receive the redacted content).

Disabling this option may be useful for room administrators and
moderators, so they can see content redacted by other users and
handle it appropriately.  However, one should use this option
with caution, as it will keep unpleasant content visible even
after it has been redacted.

After changing this option, a room's buffer must be killed and
reopened for existing messages to be rendered accordingly."
  :type '(choice (const :tag "Hide content" t)
                 (const :tag "Strikethrough" nil)))

;;;;; Faces

(defface leman-room-name
  '((t (:inherit font-lock-function-name-face)))
  "Room name shown in header line."
  :group 'leman-room-faces)

(defface leman-room-membership
  '((t (:height 0.8 :inherit font-lock-comment-face)))
  "Membership events (join/part)."
  :group 'leman-room-faces)

(defface leman-room-reactions
  '((t (:inherit font-lock-comment-face :height 0.9)))
  "Reactions to messages (including the user count)."
  :group 'leman-room-faces)

(defface leman-room-reactions-key
  '((t (:inherit leman-room-reactions :height 1.5)))
  "Reactions to messages (the key, i.e. the emoji part).
Uses a separate face to allow the key to be shown at a different
size, because in some fonts, emojis are too small relative to
normal text."
  :group 'leman-room-faces)

(defface leman-room-timestamp
  '((t (:inherit font-lock-comment-face)))
  "Event timestamps."
  :group 'leman-room-faces)

(defface leman-room-user
  '((t (:inherit font-lock-function-name-face :weight bold :overline t)))
  "Usernames."
  :group 'leman-room-faces)

(defface leman-room-self
  '((t (:inherit (font-lock-variable-name-face leman-room-user) :weight bold)))
  "Own username."
  :group 'leman-room-faces)

(defface leman-room-message-text
  '((t (:inherit default)))
  "Text message bodies."
  :group 'leman-room-faces)

(defface leman-room-message-emote
  '((t (:inherit italic)))
  "Emote message bodies."
  :group 'leman-room-faces)

(defface leman-room-quote
  '((t (:height 0.9 :inherit font-lock-comment-face)))
  "Quoted parts of messages.
Anything wrapped by HTML BLOCKQUOTE tag."
  :group 'leman-room-faces)

(defface leman-room-redacted
  '((t (:strike-through t)))
  "Redacted messages."
  :group 'leman-room-faces)

(defface leman-room-spoiler
  '((t (:inherit button)))
  "Spoiler labels and unrevealed spoiler placeholders."
  :group 'leman-room-faces)

(defface leman-room-self-message
  '((t (:inherit (font-lock-variable-name-face))))
  "Oneself's message bodies.
Note that this does not need to inherit
`leman-room-message-text', because that face is combined with
this one automatically."
  :group 'leman-room-faces)

(defface leman-room-timestamp-header
  '((t (:inherit header-line :weight bold :height 1.1)))
  "Timestamp headers."
  :group 'leman-room-faces)

(defface leman-room-mention
  ;; TODO(30.1): Remove when not supporting Emacs 27 anymore.
  (if (version< emacs-version "27.1")
      '((t (:inherit hl-line)))
    '((t (:inherit hl-line :extend t))))
  "Messages that mention the local user."
  :group 'leman-room-faces)

(defface leman-room-wrap-prefix
  `((t :inherit highlight))
  "Face applied to `leman-room-wrap-prefix', which see."
  :group 'leman-room-faces)

;;;;; Options

(defcustom leman-room-ellipsis "⋮"
  "String used when abbreviating certain strings."
  :type 'string)

(defcustom leman-room-avatars (display-images-p)
  "Show room avatars."
  :type 'boolean)

(defcustom leman-room-avatar-max-width 32
  "Maximum width in pixels of room avatars shown in header lines."
  :type 'integer)

(defcustom leman-room-avatar-max-height 32
  "Maximum height in pixels of room avatars shown in header lines."
  :type 'integer)

(defcustom leman-room-coalesce-events 100
  "Coalesce certain events in room buffers.
For example, membership events can be overwhelming in large
rooms, especially ones bridged to IRC.  This option groups them
together so they take less space.

The current, naïve implementation re-renders events as they are
coalesced, which can cause a performance problem in unusual
circumstances, so the number of events coalesced into a single,
rendered event may be limited."
  :type '(choice (integer :tag "Up to this many events")
                 (const :tag "An unlimited number of events"
                        ;; NOTE: As this docstring says, in most cases it should be fine,
                        ;; but since in those rare cases the problem can be unusually bad
                        ;; (e.g. taking 15 minutes to render a room's events), we default
                        ;; to a safer choice.
                        :doc "Note that this choice may cause performance problems in rooms with very large numbers of consecutive membership events, but in most cases it should be fine."
                        t)
                 (const :tag "Don't coalesce" nil)))

(defcustom leman-room-header-line-format
  ;; TODO: Show in new screenshots.
  '(:eval (concat (if leman-room-avatars
                      (or (leman-room-avatar leman-room)
                          "")
                    "")
                  " " (propertize (leman-room--escape-%
                                   (or (leman-room-display-name leman-room)
                                       "[no room name]"))
                                  'face 'leman-room-name)
                  ": " (propertize (leman-room--escape-%
                                    (or (leman-room-topic leman-room)
                                        "[no topic]"))
                                   ;; Also set help-echo in case the topic is too wide to fit.
                                   'help-echo (leman-room-topic leman-room))))
  "Header line format for room buffers.
See Info node `(elisp)Header lines'."
  :type 'sexp)
(put 'leman-room-header-line-format 'risky-local-variable t)

(defcustom leman-room-buffer-name-prefix "*Leman Room: "
  "Prefix for Leman room buffer names."
  :type 'string)

(defcustom leman-room-buffer-name-suffix "*"
  "Suffix for Leman room buffer names."
  :type 'string)

(defcustom leman-room-timestamp-format "%H:%M:%S"
  "Format string for event timestamps.
See function `format-time-string'."
  :type '(choice (const "%H:%M:%S")
                 (const "%Y-%m-%d %H:%M:%S")
                 string))

(defcustom leman-room-left-margin-width 0
  "Width of left margin in room buffers.
When using a non-graphical display, this should be set slightly
wider than when using a graphical display, to prevent sender
display names from colliding with event text."
  :type 'integer)

(defcustom leman-room-right-margin-width (length leman-room-timestamp-format)
  "Width of right margin in room buffers."
  :type 'integer)

(defcustom leman-room-sender-headers t
  "Show sender headers.
Automatically set by setting `leman-room-message-format-spec',
but may be overridden manually."
  :type 'boolean)

(defcustom leman-room-unread-only-counts-notifications t
  "Only use notification counts to mark rooms unread.
Notification counts are set by the server based on each room's
notification settings.  Otherwise, whether a room is marked
unread depends on the room's fully-read marker, read-receipt
marker, whether the local user sent the latest events, etc."
  :type 'boolean)

(defcustom leman-room-compose-method 'minibuffer
  "How to compose messages.

The value `minibuffer' means the minibuffer will be used to write
and edit messages.  You can use \
\\<leman-room-minibuffer-map>\\[leman-room-compose-from-minibuffer] \
to switch from the minibuffer
to a separate compose buffer, and \\[save-buffer] in the compose buffer
will then return you to the minibuffer to confirm the message
before sending.

The value `compose-buffer' means that the minibuffer is not used --
messages are written in a compose buffer by default, and \\[save-buffer]
sends the composed message directly."
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Compose buffer" compose-buffer)))

(defcustom leman-room-compose-buffer-display-action
  (cons 'display-buffer-below-selected
        '((window-height . 3)
          (inhibit-same-window . t)
          (reusable-frames . nil)))
  "`display-buffer' action for displaying compose buffers.

See also option `leman-room-compose-buffer-window-auto-height'
and `leman-room-compose-buffer-window-dedicated'."
  :type display-buffer--action-custom-type
  :risky t)

(defcustom leman-room-compose-buffer-window-dedicated 'created
  "Whether windows for compose buffers should be dedicated.

A dedicated compose buffer window will not be used to display any
other buffer, and will be deleted once the message has been sent
or aborted (see `leman-room-compose-buffer-quit-restore-window').

The values t and nil mean \"always\" and \"never\" respectively.

The value `created' means newly-created windows are dedicated.
\(The default `leman-room-compose-buffer-display-action' always
creates a new window.)

The value `auto-height' means that windows will be dedicated if
the option `leman-room-compose-buffer-window-auto-height' is
enabled (this option generally keeps the windows too small to
usefully display other buffers).

The value `delete' means that windows will not be dedicated, but
they will still be deleted once the message is sent or aborted
\(even when they have also been used to display other buffers).

See also `set-window-dedicated-p' and
`switch-to-buffer-in-dedicated-window'."
  :type '(radio (const :tag "Always" t)
                (const :tag "Never" nil)
                (const :tag "Never (but always delete window)" delete)
                (const :tag "Newly-created windows" created)
                (const :tag "When auto-height enabled" auto-height)))

(defcustom leman-room-compose-buffer-window-auto-height t
  "Dynamically match the compose buffer window height to its contents.
See also `leman-room-compose-buffer-window-auto-height-max' and
`leman-room-compose-buffer-window-auto-height-min'."
  :type 'boolean)

;; Experimental.  Disabled by default.  Set to 'height to use this.
(defvar leman-room-compose-buffer-window-auto-height-fixed nil
  "The buffer-local `window-size-fixed' value in compose buffers.")

(defvar leman-room-compose-buffer-window-auto-height-pixelwise t
  "Whether to adjust the window height for pixel-precise lines.")

;; This is a mutex to ensure that auto-height resizing cannot trigger itself
;; recursively.  This may prevent desirable resizing in certain cases, but we
;; get the correct result in the majority of situations, and it is simple.
(defvar leman-room-compose-buffer-window-auto-height-resizing-p)

(defcustom leman-room-compose-buffer-window-auto-height-min nil
  "If non-nil, limits the body height of the compose buffer window.

See also option `leman-room-compose-buffer-window-auto-height'
and `leman-room-compose-buffer-window-auto-height-max'."
  :type '(choice (const :tag "Default" nil)
                 (natnum :tag "Lines")))

(defcustom leman-room-compose-buffer-window-auto-height-max nil
  "If non-nil, limits the body height of the compose buffer window.

See also option `leman-room-compose-buffer-window-auto-height'
and `leman-room-compose-buffer-window-auto-height-min'."
  :type '(choice (const :tag "Default" nil)
                 (natnum :tag "Lines")))

(defcustom leman-room-mode-self-insert-keymap-update-hook nil
  "Hook run after rebuilding `leman-room-mode-self-insert-keymap'.

This happens at the time `leman-room-self-insert-mode' is
enabled, and also if user options `leman-room-self-insert-chars',
`leman-room-self-insert-commands', or
`leman-room-mode-map-prefix-key' are customized while the mode is
enabled.

You can use this hook to define any desired custom bindings which
are not accounted for by those user options."
  :type 'hook)

(defvar leman-room-self-insert-mode)
(defvar leman-room-self-insert-chars)
(defvar leman-room-self-insert-commands)
(defun leman-room-mode-self-insert-keymap-update ()
  "Rebuilds `leman-room-mode-self-insert-keymap'.
Also rebuilds `leman-room-mode--advertised-keymap'."
  ;; Must be defined ahead of `leman-room-self-insert-option-setter'.
  (let ((map (make-sparse-keymap)))
    ;; Ensure that `leman-room-self-insert-chars' start a message.
    (dolist (range leman-room-self-insert-chars)
      (if (consp range)
          ;; Process a range the same way that `global-map' does.
          (let ((vec1 (make-vector 1 nil))
                (from (car range))
                (to (cdr range)))
            (while (<= from to)
              (aset vec1 0 from)
              (define-key map vec1 #'leman-room-self-insert-new-message)
              (setq from (1+ from))))
        ;; Else `range' is a single character.
        (define-key map (vector range) #'leman-room-self-insert-new-message)))
    ;; Provide access to `leman-room-mode-map' via a prefix binding.
    (when (bound-and-true-p leman-room-mode-map-prefix-key)
      (define-key map leman-room-mode-map-prefix-key leman-room-mode-map))
    ;; This is now the basis for `leman-room-mode-self-insert-keymap' and also
    ;; `leman-room-mode--advertised-keymap' (when `leman-room-self-insert-mode'
    ;; is enabled), but we need to keep the remaining differences between them
    ;; separate.  (We do still need some identical `remap' bindings for both
    ;; keymaps, but we can't do that just yet.)
    (setq leman-room-mode-self-insert-keymap (copy-keymap map))
    ;; To `leman-room-mode-self-insert-keymap', add `leman-room-mode-map'
    ;; as the keymap parent.  (This is the keymap which is actually used.)
    (set-keymap-parent leman-room-mode-self-insert-keymap leman-room-mode-map)
    (if (not (bound-and-true-p leman-room-self-insert-mode))
        ;; Advertise the real `leman-room-mode-map'.
        (setq leman-room-mode--advertised-keymap leman-room-mode-map)
      ;; Otherwise we base `leman-room-mode--advertised-keymap' on the same base
      ;; map previously copied to `leman-room-mode-self-insert-keymap'.
      (setq leman-room-mode--advertised-keymap map)
      ;; To `leman-room-mode--advertised-keymap' (the keymap displayed when
      ;; `describe-mode' is called), rather than setting a parent we instead
      ;; copy the non-conflicting top-level bindings from `leman-room-mode-map'.
      ;; Not using a keymap parent means the advertised map doesn't see any
      ;; future changes to `leman-room-mode-map', but having a keymap parent
      ;; would make the `describe-mode' output very confusing on account of
      ;; Emacs bug#66792, so we accept potential inaccuracy as a trade-off for
      ;; showing more comprehensible help.
      ;;
      ;; The following will copy the `remap' keymap verbatim, clobbering any
      ;; pre-existing remappings; so we do this before we define other
      ;; remappings.
      (cl-labels ((copy-from (key definition)
		    (unless (lookup-key leman-room-mode--advertised-keymap
                                        (vector key))
		      (define-key leman-room-mode--advertised-keymap
                                  (vector key) definition))))
        ;; Copy from a copy of `leman-room-mode-map', otherwise the latter will
        ;; also acquire (share) the remap keybindings which are added below.
        (map-keymap #'copy-from (copy-keymap leman-room-mode-map))))
    ;; Now define our additional `remap' bindings in both keymaps.
    (let ((keymaps (if (bound-and-true-p leman-room-self-insert-mode)
                       (list leman-room-mode-self-insert-keymap
                             leman-room-mode--advertised-keymap)
                     (list leman-room-mode-self-insert-keymap))))
      (dolist (keymap keymaps)
        ;; Make `self-insert-command' (and friends) start a new message.
        (dolist (cmd leman-room-self-insert-commands)
          (define-key keymap (vector 'remap cmd)
                      #'leman-room-self-insert-new-message)))))
  (run-hooks 'leman-room-mode-self-insert-keymap-update-hook))

(defun leman-room-mode-effective-keymap-update ()
  "Sets the parent keymap for `leman-room-mode-effective-keymap'.

Either `leman-room-mode-self-insert-keymap' or `leman-room-mode-map',
depending on `leman-room-self-insert-mode'."
  ;; Must be defined ahead of `leman-room-self-insert-option-setter'.
  (set-keymap-parent leman-room-mode-effective-keymap
                     (if (bound-and-true-p leman-room-self-insert-mode)
                         leman-room-mode-self-insert-keymap
                       leman-room-mode-map)))

(defun leman-room-self-insert-option-setter (option value)
  "Setter for options affecting `leman-room-self-insert-mode'.

This is the setter function for `leman-room-self-insert-chars'
and `leman-room-self-insert-commands'.

Sets the value with (set-default-toplevel-value OPTION VALUE),
and then rebuilds `leman-room-mode-self-insert-keymap'."
  ;; Must be defined ahead of `leman-room-self-insert-chars' and
  ;; `leman-room-self-insert-commands'.
  ;;
  ;; Update the variable.
  (set-default-toplevel-value option value)
  ;; Update keymaps when necessary.
  (when (bound-and-true-p leman-room-self-insert-mode)
    (leman-room-mode-self-insert-keymap-update)
    (leman-room-mode-effective-keymap-update)))

(defcustom leman-room-self-insert-chars
  '((33 . 62) (64 . 126))
  "Characters handled by `leman-room-self-insert-mode'.

These are in addition to any `self-insert-command' key bindings
-- this list is to ensure that certain keys will be treated this
way even when they have `leman-room-mode-map' bindings.

Cons cell elements represent the range from the car to the cdr
\(inclusive).  The default value covers the common \"printable\"
ASCII characters excluding SPC (32), ? (63), and DEL (127).

Customizing this option updates `leman-room-mode-self-insert-keymap'
via the setter function `leman-room-self-insert-option-setter'.
To do the same in lisp code, set the option with `setopt'.

See also `leman-room-self-insert-commands'."
  :type '(repeat (choice (character :tag "Character")
                         (cons :tag "Character range"
                               (character :tag "From")
                               (character :tag "To"))))
  :set #'leman-room-self-insert-option-setter)

(defcustom leman-room-self-insert-commands
  '(self-insert-command yank)
  "Commands handled by `leman-room-self-insert-mode'.

When the mode is enabled, the listed commands are remapped to
`leman-room-self-insert-new-message' such that when one of those
commands is invoked in a room buffer, a new message will be
started and the event which triggered the command (typically a
`self-insert-command' key binding) will be re-issued in the
message buffer.

Customizing this option updates `leman-room-mode-self-insert-keymap'
via the setter function `leman-room-self-insert-option-setter'.
To do the same in lisp code, set the option with `setopt'.

See also `leman-room-self-insert-chars'."
  :type '(repeat (function :tag "Command"))
  :set #'leman-room-self-insert-option-setter)

(defcustom leman-room-mode-map-prefix-key (kbd "DEL")
  "A prefix key sequence to access `leman-room-mode-map'.
Active when `leman-room-self-insert-mode' is enabled.

The default key is DEL.

Customizing this option updates `leman-room-mode-self-insert-keymap'
via the setter function `leman-room-self-insert-option-setter'.
To do the same in lisp code, set the option with `setopt'."
  :type 'key-sequence
  :set #'leman-room-self-insert-option-setter)

(defcustom leman-room-reaction-picker (if (commandp 'emoji-search)
                                          'emoji-search
                                        #'insert-char)
  "Command used to select a reaction by `leman-room-send-reaction'.
Should be set to a command that somehow prompts the user for an
emoji and inserts it into the current buffer.  In Emacs 29
reasonable choices include `emoji-insert' which uses a transient
interface, and `emoji-search' which uses `completing-read'.  If
those are not available, one can use `insert-char'."
  :type `(choice
          (const :tag "Complete unicode character name" insert-char)
          ,@(when (commandp 'emoji-insert)
              '((const :tag "Categorized emoji menu" emoji-insert)))
          ,@(when (commandp 'emoji-search)
              '((const :tag "Complete emoji name" emoji-search)))
          ,@(when (assoc "emoji" input-method-alist)
              '((const :tag "Emoji input method"
                       leman-room-use-emoji-input-method)))
          (const :tag "Type an emoji without assistance" ignore)
          (function :tag "Use other command")))

(defvar leman-room-sender-in-left-margin nil
  "Whether sender is shown in left margin.
Set by `leman-room-message-format-spec-setter'.")

(defun leman-room-message-format-spec-setter (option value &optional local)
  "Set relevant options for `leman-room-message-format-spec', which see.
To be used as that option's setter.  OPTION and VALUE are
received from setting the customization option.  If LOCAL is
non-nil, set the variables buffer-locally (i.e. when called from
`leman-room-set-message-format'."
  (cl-macrolet ((set-vars (&rest pairs)
                  ;; Set variable-value pairs, locally if LOCAL is non-nil.
                  `(progn
                     ,@(cl-loop for (symbol value) on pairs by #'cddr
                                collect `(if local
                                             (set (make-local-variable ',symbol) ,value)
                                           (set ',symbol ,value))))))
    (if local
        (set (make-local-variable option) value)
      (set-default option value))
    (pcase value
      ;; Try to set the margin widths smartly.
      ("%B%r%T%R%t" ;; "Elemental"
        (set-vars leman-room-left-margin-width 0
                  leman-room-right-margin-width 8
                  leman-room-sender-headers t
                  leman-room-sender-in-headers t
                  leman-room-sender-in-left-margin nil))
      ("%S%L%B%r%T%R%t" ;; "IRC-style using margins"
        (set-vars leman-room-left-margin-width 12
                  leman-room-right-margin-width 8
                  leman-room-sender-headers nil
                  leman-room-sender-in-headers nil
                  leman-room-sender-in-left-margin t))
      ("[%t] %S> %B%r%T" ;; "IRC-style without margins"
       (set-vars leman-room-left-margin-width 0
                 leman-room-right-margin-width 0
                 leman-room-sender-headers nil
                 leman-room-sender-in-headers nil
                 leman-room-sender-in-left-margin nil))
      (_ (set-vars leman-room-left-margin-width
                   (if (string-match-p "%L" value)
                       12 0)
                   leman-room-right-margin-width
                   (if (string-match-p "%R" value)
                       8 0)
                   leman-room-sender-in-left-margin
                   (if (string-match-p (rx (1+ anything) (or "%S" "%s") (1+ anything) "%L") value)
                       t nil)
                   ;; NOTE: The following two variables may seem redundant, but one is an
                   ;; option that the user may override, while the other is set
                   ;; automatically.
                   leman-room-sender-headers
                   (if (string-match-p (or "%S" "%s") value)
                       ;; If "%S" or "%s" isn't found, assume it's to be shown in headers.
                       nil t)
                   leman-room-sender-in-headers
                   (if (string-match-p (rx (or "%S" "%s")) value)
                       ;; If "%S" or "%s" isn't found, assume it's to be shown in headers.
                       nil t))
         (message "Leman: When using custom message format, setting margin widths may be necessary")))
    (unless leman-room-sender-in-headers
      ;; HACK: Disable overline on sender face.
      (require 'face-remap)
      (if local
          (progn
            (face-remap-reset-base 'leman-room-user)
            (face-remap-add-relative 'leman-room-user '(:overline nil)))
        (set-face-attribute 'leman-room-user nil :overline nil)))
    (unless local
      (when (and (bound-and-true-p leman-sessions) (car leman-sessions))
        ;; Only display when a session is connected (not sure why `bound-and-true-p'
        ;; is required to avoid compilation warnings).
        (message "Leman: Kill and reopen room buffers to display in new format")))))

(defcustom leman-room-message-format-spec "%S%L%B%r%T%R%t"
  "Format messages according to this spec.
It may contain these specifiers:

  %L  End of left margin
  %R  Start of right margin
  %W  End of wrap-prefix

  %b  Message body (plain-text)
  %B  Message body (formatted if available)
  %i  Event ID
  %O  Room display name (used for mentions buffer)
  %r  Reactions
  %s  Sender ID
  %S  Sender display name
  %t  Event timestamp, formatted according to
      `leman-room-timestamp-format'
  %T  Thread summary (a chip linking to the thread view)

Note that margin sizes must be set manually with
`leman-room-left-margin-width' and
`leman-room-right-margin-width'."
  :type '(choice (const :tag "IRC-style using margins" "%S%L%B%r%T%R%t")
                 (const :tag "IRC-style without margins" "[%t] %S> %B%r%T")
                 (const :tag "IRC-style without margins, with wrap-prefix" "[%t] %S> %W%B%r%T")
                 (const :tag "IRC-style with right margin, with wrap-prefix" "%S> %W%B%r%T%R%t")
                 (const :tag "Elemental" "%B%r%T%R%t")
                 (string :tag "Custom format"))
  :set #'leman-room-message-format-spec-setter
  :set-after '(leman-room-left-margin-width leman-room-right-margin-width
                                            leman-room-sender-headers)
  ;; This file must be loaded before calling the setter to define the
  ;; `leman-room-user' face used in it.
  :require 'leman-room)

(defcustom leman-room-retro-messages-number 30
  "Number of messages to retrieve when loading earlier messages."
  :type 'integer)

(defcustom leman-room-timestamp-header-format " %H:%M "
  "Format string for timestamp headers where date is unchanged.
See function `format-time-string'.  If this string ends in a
newline, its background color will extend to the end of the
line."
  :type '(choice (const :tag "Time-only" " %H:%M ")
                 (const :tag "Always show date" " %Y-%m-%d %H:%M ")
                 string))

(defcustom leman-room-timestamp-header-with-date-format " %Y-%m-%d (%A)\n"
  ;; FIXME: In Emacs 27+, maybe use :extend t instead of adding a newline.
  "Format string for timestamp headers where date changes.
See function `format-time-string'.  If this string ends in a
newline, its background color will extend to the end of the
line."
  :type '(choice (const " %Y-%m-%d (%A)\n")
                 string))

(defcustom leman-room-replace-edited-messages t
  "Replace edited messages with their new content.
When nil, edited messages are displayed as new messages, leaving
the original messages visible."
  :type 'boolean)

(define-obsolete-variable-alias 'leman-room-shr-use-fonts
  'leman-room-use-variable-pitch "leman-0.14")

(defcustom leman-room-use-variable-pitch nil
  "Use proportional fonts for message bodies.
If non-nil, plain text message bodies are displayed in a
variable-pitch font, and `shr-use-fonts' is enabled for rendering
HTML-formatted message bodies (which includes most replies)."
  :type '(choice (const :tag "Disable variable-pitch fonts" nil)
                 (const :tag "Enable variable-pitch fonts" t)))

(defcustom leman-room-username-display-property '(raise -0.25)
  "Display property applied to username strings.
See Info node `(elisp)Other Display Specs'."
  :type '(choice (list :tag "Raise" (const :tag "Raise" raise) (number :tag "Factor"))
                 (list :tag "Height" (const height)
                       (choice (list :tag "Larger" (const :tag "Larger" +) (number :tag "Steps"))
                               (list :tag "Smaller" (const :tag "Smaller" -) (number :tag "Steps"))
                               (number :tag "Factor")
                               (function :tag "Function")
                               (sexp :tag "Form"))) ))

(defcustom leman-room-event-separator-display-property '(space :ascent 50)
  "Display property applied to invisible space string after events.
Allows visual separation between events without, e.g. inserting
newlines.

See Info node `(elisp)Specified Space'."
  :type 'sexp)

(defcustom leman-room-timestamp-header-delta 600
  "Show timestamp header where events are at least this many seconds apart."
  :type 'integer)

(defcustom leman-room-send-message-filter nil
  "Function through which to pass message content before sending.
Used to, e.g. send an Org-formatted message by exporting it to
HTML first."
  :type '(choice (const :tag "Send messages as-is" nil)
                 (const :tag "Send messages in Org format" leman-room-send-org-filter)
                 (function :tag "Custom filter function"))
  :set (lambda (option value)
         (set-default option value)
         (pcase value
           ('leman-room-send-org-filter
            ;; Activate in compose buffer by default.
            (add-hook 'leman-room-compose-hook #'leman-room-compose-org))
           (_ (remove-hook 'leman-room-compose-hook #'leman-room-compose-org)))))

(defcustom leman-room-mark-rooms-read t
  "Mark rooms as read automatically.
Moves read and fully-read markers in rooms on the server when
`leman-room-scroll-up-mark-read' is called at the end of a
buffer.  When `send', also marks room as read when sending a
message in it.  When disabled, rooms may still be marked as read
manually by calling `leman-room-mark-read'.  Note that this is
not strictly the same as read receipts."
  :type '(choice (const :tag "When scrolling past end of buffer" t)
                 (const :tag "Also when sending" send)
                 (const :tag "Never" nil)))

(defcustom leman-room-send-typing t
  "Send typing notifications to the server while typing a message."
  :type 'boolean)

(defcustom leman-room-join-view-buffer t
  "View room buffer when joining a room."
  :type 'boolean)

(defcustom leman-room-leave-kill-buffer t
  "Kill room buffer when leaving a room.
When disabled, the room's buffer will remain open, but
Matrix-related commands in it will fail."
  :type 'boolean)

(defcustom leman-room-warn-for-already-seen-messages nil
  "Warn when a sent message has already been seen.
Such a case could very rarely indicate a reused transaction ID,
which would prevent further messages from being sent (and would
be solved by logging in with a new session, generating a new
token), but most often it happens when the server echoes back a
sent message before acknowledging the sending of the
message (which is harmless and can be ignored)."
  :type 'boolean)

(defcustom leman-room-wrap-prefix
  (concat (propertize " "
                      'face 'leman-room-wrap-prefix)
          " ")
  "String prefixing certain events in room buffers.
Events include membership events, image attachments, etc.
Generally users should prefer to customize the face
`leman-room-wrap-prefix' rather than this option, because this
option's default value has that face applied to it where
appropriate; if users customize this option, they will need to
apply the face to the string themselves, if desired."
  :type 'string)

(defgroup leman-room-prism nil
  "Colorize usernames and messages in rooms."
  :group 'leman-room)

(defcustom leman-room-prism 'name
  "Display users' names and messages in unique colors."
  :type '(choice (const :tag "Name only" name)
                 (const :tag "Name and message" both)
                 (const :tag "Neither" nil)))

(defcustom leman-room-prism-addressee t
  "Show addressees' names in their respective colors.
Applies to room member names at the beginning of messages,
preceded by a colon or comma.

Note that a limitation applies to the current implementation: if
a message from the addressee is not yet visible in a room at the
time the addressed message is formatted, the color may not be
applied."
  ;; FIXME: When we keep a hash table of members in a room, make this
  ;; smarter.
  :type 'boolean)

(defcustom leman-room-prism-color-adjustment 0
  "Number used to tweak computed username colors.
This may be used to adjust your favorite users' colors if you
don't like the default ones.  (The only way to do it is by
experimentation--there is no direct mapping available, nor a
per-user setting.)

The number is added to the hashed user ID before converting it to
a color.  Note that, since user ID hashes are ratioed against
`most-positive-fixnum', this number must be very large in order
to have any effect; it should be at least 1e13.

After changing this option, a room's buffer must be killed and
recreated to see the effect."
  :type 'number
  :set (lambda (option value)
         (unless (or (= 0 value) (>= value 1e13))
           (user-error "This option must be a very large number, at least 1e13"))
         (set-default option value)))

(defcustom leman-room-prism-minimum-contrast 6
  "Attempt to enforce this minimum contrast ratio for user faces.
This should be a reasonable number from, e.g. 0-7 or so."
  ;; Prot would almost approve of this default.  :) I would go all the way
  ;; to 7, but 6 already significantly dilutes the colors in some cases.
  :type 'number)

(defcustom leman-room-prism-message-desaturation 25
  "Desaturate user colors by this percent for message bodies.
Makes message bodies a bit less intense."
  :type 'integer)

(defcustom leman-room-prism-message-lightening 10
  "Lighten user colors by this percent for message bodies.
Makes message bodies a bit less intense.

When using a light theme, it may be necessary to use a negative
number (to darken rather than lighten)."
  :type 'integer)

;;;; Macros

(defmacro leman-room-with-highlighted-event-at (position &rest body)
  "Highlight event at POSITION while evaluating BODY."
  ;; MAYBE: Accept a marker for POSITION.
  (declare (indent 1))
  `(let (leman-room-replying-to-overlay)
     (unwind-protect
         (progn
           (leman-room-highlight-event-at ,position)
           ,@body)
       (leman-room-unhighlight-event))))

(defmacro leman-room-with-typing (&rest body)
  "Send typing notifications around BODY.
When `leman-room-send-typing' is enabled, typing notifications
are sent while BODY is executing.  BODY is wrapped in an
`unwind-protect' form that cancels `leman-room-typing-timer' and
sends a not-typing notification."
  (declare (indent defun))
  `(unwind-protect
       (progn
         (when leman-room-send-typing
           (when leman-room-typing-timer
             ;; In case there are any stray ones (e.g. a user typing in
             ;; more than room at once, which is possible but unlikely).
             (cancel-timer leman-room-typing-timer))
           (setf leman-room-typing-timer (run-at-time nil 15 #'leman-room--send-typing leman-session leman-room)))
         ,@body)
     (when leman-room-send-typing
       (when leman-room-typing-timer
         (cancel-timer leman-room-typing-timer)
         (setf leman-room-typing-timer nil))
       ;; Cancel typing notifications after sending a message.  (The
       ;; spec doesn't say whether this is needed, but it seems to be.)
       (leman-room--send-typing leman-session leman-room :typing nil))))

(defmacro leman-room-wrap-prefix (string-form &rest properties)
  "Wrap STRING-FORM with `leman-room-wrap-prefix'.
Concats `leman-room-wrap-prefix' to STRING-FORM and applies it as
the `wrap-prefix' property.  Also applies any PROPERTIES."
  (declare (indent defun))
  `(concat leman-room-wrap-prefix
           (propertize ,string-form
                       'wrap-prefix leman-room-wrap-prefix
                       ,@properties)))

(defsubst leman-room--concat-property (string property value &optional append)
  "Return STRING having concatted VALUE with PROPERTY on it.
If APPEND, append it; otherwise prepend.  Assumes PROPERTY is
constant throughout STRING."
  (declare (indent defun))
  (let* ((old-value (get-text-property 0 property string))
         (new-value (if append
                        (concat old-value value)
                      (concat value old-value))))
    (propertize string property new-value)))

;;;;; Event highlighting

(defun leman-room-highlight-event-at (position)
  "Highlight event at POSITION using `leman-room-replying-to-overlay'.
See `leman-room-with-highlighted-event-at'."
  ;; MAYBE: Accept a marker for POSITION.
  (let* ((node (ewoc-locate leman-ewoc position))
         (event (ewoc-data node)))
    (unless (and (leman-event-p event)
                 (leman-event-id event))
      (error "No event at point"))
    (setf leman-room-replying-to-overlay
          (make-overlay (ewoc-location node)
                        ;; NOTE: It doesn't seem possible to get the end position of
                        ;; a node, so if there is no next node, we use point-max.
                        ;; But this might break if we were to use an EWOC footer.
                        (if (ewoc-next leman-ewoc node)
                            (ewoc-location (ewoc-next leman-ewoc node))
                          (point-max))))
    (overlay-put leman-room-replying-to-overlay 'face 'highlight)))

(defun leman-room-unhighlight-event ()
  "Delete overlay in `leman-room-replying-to-overlay'.
See `leman-room-with-highlighted-event-at'."
  (when (overlayp leman-room-replying-to-overlay)
    (delete-overlay leman-room-replying-to-overlay))
  (setf leman-room-replying-to-overlay nil))

(defun leman-room-compose-highlight (compose-buffer)
  "Make `leman-room-with-highlighted-event-at' persistent while COMPOSE-BUFFER exists."
  (when-let ((overlay leman-room-replying-to-overlay))
    ;; Prevent `leman-room-with-highlighted-event-at' from deleting the overlay:
    (setq leman-room-replying-to-overlay nil)
    ;; Instead, make it exist for the lifetime of the compose buffer:
    (cl-flet ((delete-overlay ()
                (when (overlayp overlay)
                  (delete-overlay overlay))))
      (with-current-buffer compose-buffer
        (add-hook 'kill-buffer-hook #'delete-overlay nil :local)))))

;;;;; Event formatting

;; NOTE: When adding specs, also add them to docstring
;; for `leman-room-message-format-spec'.

(defvar leman-room-event-formatters nil
  "Alist mapping characters to event-formatting functions.
Each function is called with three arguments: the event, the
room, and the session.  See macro
`leman-room-define-event-formatter'.")

(defvar leman-room--format-message-margin-p nil
  "Set by margin-related event formatters.")

(defvar leman-room--format-message-wrap-prefix nil
  "Set by margin-related event formatters.")

(defmacro leman-room-define-event-formatter (char docstring &rest body)
  "Define an event formatter for CHAR with DOCSTRING and BODY.
BODY is wrapped in a lambda form that binds `event', `room', and
`session', and the lambda is added to the variable
`leman-room-event-formatters', which see."
  (declare (indent defun)
           (debug (characterp stringp def-body)))
  `(setf (alist-get ,char leman-room-event-formatters nil nil #'equal)
         (lambda (event room session)
           ,docstring
           ,@body)))

(leman-room-define-event-formatter ?L
  "Text before this is shown in the left margin."
  (ignore event room session)
  (setf leman-room--format-message-margin-p t)
  (propertize " " 'left-margin-end t))

(leman-room-define-event-formatter ?R
  "Text after this is shown in the right margin."
  (ignore event room session)
  (setf leman-room--format-message-margin-p t)
  (propertize " " 'right-margin-start t))

(leman-room-define-event-formatter ?W
  "Text before this is the length of the event's wrap-prefix.
This emulates the effect of using the left margin (the \"%L\"
spec) without requiring all events to use the same margin width."
  (ignore event room session)
  (setf leman-room--format-message-wrap-prefix t)
  (propertize " " 'wrap-prefix-end t))

;; FIXME(v0.12): The quote-end may be detected in the wrong position when, e.g. a link is
;; in the middle of the quoted part.  We need to search backward from the end to find
;; where the quote face finally ends.

(leman-room-define-event-formatter ?b
  "Plain-text body content."
  ;; NOTE: `save-match-data' is required around calls to `leman-room--format-message-body'.
  (let* ((body (save-match-data
                 (leman-room--format-message-body event session :formatted-p nil)))
         (body-length (length body))
         (face (leman-room--event-body-face event room session))
         (quote-start (leman--text-property-search-forward 'face
                        (lambda (value)
                          (pcase value
                            ('leman-room-quote t)
                            ((pred listp) (member 'leman-room-quote value))))
                        body))
         (quote-end (when quote-start
                      (leman--text-property-search-backward 'face
                        (lambda (value)
                          (pcase value
                            ('leman-room-quote t)
                            ((pred listp) (member 'leman-room-quote value))))
                        body))))
    (add-face-text-property (or quote-end 0) body-length face 'append body)
    (when leman-room-prism-addressee
      (leman-room--add-member-face body room))
    body))

(leman-room-define-event-formatter ?B
  "Formatted body content (i.e. rendered HTML)."
  (let* ((body (save-match-data
                 (leman-room--format-message-body event session)))
         (body-length (length body))
         (face (leman-room--event-body-face event room session))
         (quote-start (leman--text-property-search-forward 'face
                        (lambda (value)
                          (pcase value
                            ('leman-room-quote t)
                            ((pred listp) (member 'leman-room-quote value))))
                        body))
         (quote-end (when quote-start
                      (leman--text-property-search-backward 'face
                        (lambda (value)
                          (pcase value
                            ('leman-room-quote t)
                            ((pred listp) (member 'leman-room-quote value))))
                        body :start (length body)))))
    (add-face-text-property (or quote-end 0) body-length face 'append body)
    (when leman-room-prism-addressee
      (leman-room--add-member-face body room))
    body))

(leman-room-define-event-formatter ?i
  "Event ID."
  ;; Probably only useful for debugging, so might remove later.
  (ignore room session)
  (leman-event-id event))

(leman-room-define-event-formatter ?o
  "Room avatar."
  (ignore event session)
  (or (alist-get 'room-list-avatar (leman-room-local room)) ""))

(leman-room-define-event-formatter ?O
  "Room display name."
  (ignore event session)
  (let ((room-name (propertize (or (leman-room-display-name room)
                                   (leman--room-display-name room))
                               'face 'leman-room-name
                               'help-echo (or (leman-room-canonical-alias room)
                                              (leman-room-id room)))))
    ;; HACK: This will probably only be used in the notifications buffers, anyway.
    (when leman-notify-limit-room-name-width
      (setf room-name (truncate-string-to-width room-name leman-notify-limit-room-name-width
                                                nil nil leman-room-ellipsis)))
    room-name))

;; NOTE: In ?s and ?S, we add nearly-invisible ASCII unit-separator characters ("​")
;; to prevent, e.g. `dabbrev-expand' from expanding display names with body text.

(leman-room-define-event-formatter ?s
  "Sender MXID."
  (ignore room session)
  (concat (propertize (leman-user-id (leman-event-sender event))
                      'face 'leman-room-user)
          "​"))

(leman-room-define-event-formatter ?S
  "Sender display name."
  (ignore session)
  (pcase-let ((sender (leman--format-user (leman-event-sender event) room))
              ((cl-struct leman-room (local (map buffer))) room))
    ;; NOTE: When called from an `leman-notify' function, ROOM may have no buffer.  In
    ;; that case, just use the current buffer (which should be a temp buffer used to
    ;; format the event).
    (with-current-buffer (or buffer (current-buffer))
      (when leman-room-sender-in-left-margin
        ;; Sender in left margin: truncate/pad appropriately.
        (setf sender
              (if (< (string-width sender) leman-room-left-margin-width)
                  ;; Using :align-to or :width space display properties doesn't
                  ;; seem to have any effect in the margin, so we make a string.
                  (concat (make-string (- leman-room-left-margin-width (string-width sender))
                                       ? )
                          sender)
                ;; String wider than margin: truncate it.
                (leman-room--concat-property
                  (truncate-string-to-width sender leman-room-left-margin-width nil nil "…")
                  'help-echo (concat sender " "))))))
    ;; NOTE: I'd like to add a help-echo function to display the sender ID, but the Emacs
    ;; manual says that there is currently no way to make text in the margins mouse-sensitive.
    ;; So `leman--format-user' returns a string propertized with `help-echo' as a string.
    (concat sender "​")))

(leman-room-define-event-formatter ?r
  "Reactions."
  (ignore session)
  (leman-room--format-reactions event room))

(leman-room-define-event-formatter ?T
  "Thread summary."
  (ignore session)
  (leman-room--format-thread-chip event room))

(leman-room-define-event-formatter ?t
  "Timestamp."
  (ignore room session)
  (propertize (format-time-string leman-room-timestamp-format ;; Timestamps are in milliseconds.
                                  (/ (leman-event-origin-server-ts event) 1000))
              'face 'leman-room-timestamp
              'help-echo (format-time-string "%Y-%m-%d %H:%M:%S"
                                             (/ (leman-event-origin-server-ts event) 1000))))

(defconst leman-room-variable-pitch-face (or (and (facep 'shr-text) 'shr-text)
                                             'variable-pitch)
  "May be used when formatting plain-text messages.

If user option `leman-room-use-variable-pitch' is non-nil, this
face is applied to plain-text messages for visual consistency
with HTML messages (which will be rendered by shr.el with
`shr-use-fonts' enabled).

The `shr-text' face was added in Emacs 29.1.  Prior to that,
shr.el used the `variable-pitch' face directly.")

(defun leman-room--event-body-face (event room session)
  "Return face definition for EVENT in ROOM on SESSION."
  (ignore room)  ;; Unused for now, but keeping for consistency.
  ;; This used to be a macro in --format-message, which is probably better for
  ;; performance, but using a function is clearer, and avoids premature optimization.
  (pcase-let* (((cl-struct leman-event sender
                           (content (map msgtype format ('m.new_content new-content)))
                           (unsigned (map ('redacted_by unsigned-redacted-by)))
                           (local (map ('redacted-by local-redacted-by))))
                event)
               ((cl-struct leman-user (id sender-id)) sender)
               ((cl-struct leman-session user) session)
               ((cl-struct leman-user (id user-id)) user)
               (self-message-p (equal sender-id user-id))
               (type-face (pcase msgtype
                            ("m.emote" 'leman-room-message-emote)
                            (_ 'leman-room-message-text)))
               (context-face (cond (self-message-p
                                    'leman-room-self-message)
                                   ((or (leman-room--event-mentions-user-p event user)
                                        (leman--event-mentions-room-p event))
                                    'leman-room-mention)))
               (prism-color (unless self-message-p
                              (when (eq 'both leman-room-prism)
                                (or (leman-user-message-color sender)
                                    (setf (leman-user-message-color sender)
                                          (let ((message-color (color-desaturate-name (leman--user-color sender)
                                                                                      leman-room-prism-message-desaturation)))
                                            (if (leman--color-dark-p (color-name-to-rgb (face-background 'default)))
                                                (color-lighten-name message-color leman-room-prism-message-lightening)
                                              (color-darken-name message-color leman-room-prism-message-lightening))))))))
               (redacted-face (when (or local-redacted-by unsigned-redacted-by)
                                'leman-room-redacted))
               ;; For visual consistency, apply the variable-pitch `shr-text' face to
               ;; non-HTML messages when `leman-room-use-variable-pitch' is non-nil.
               ;; (HTML messages are fontified by shr itself.)
               (shr-text-face (when (and leman-room-use-variable-pitch
                                         (not (equal (or format (alist-get 'format new-content))
                                                     "org.matrix.custom.html")))
                                leman-room-variable-pitch-face))
               (body-face (list :inherit (delq nil (list redacted-face context-face type-face shr-text-face)))))
    (if prism-color
        (plist-put body-face :foreground prism-color)
      body-face)))

(defun leman-room--user-named (member-name buffer room)
  "Return the user in ROOM whose displayname is MEMBER-NAME.
Searches the events visible in ROOM's BUFFER for a sender whose
displayname in ROOM matches MEMBER-NAME.  Returns nil if no such
sender is found."
  ;; HACK: Since we don't currently keep a list of all members in a
  ;; room, we look to see if this displayname has any mentions in the
  ;; room so far.
  (save-match-data
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (cl-labels ((found-sender-p (ewoc-data)
                      (when (leman-event-p ewoc-data)
                        (equal member-name
                               (gethash (leman-event-sender ewoc-data) (leman-room-displaynames room))))))
          (cl-loop with regexp = (regexp-quote member-name)
                   while (re-search-forward regexp nil t)
                   ;; NOTE: I don't know why, but sometimes the regexp
                   ;; search ends on a non-event line, like a timestamp
                   ;; header, so for now we just try to handle that case.
                   for maybe-event = (ewoc-data (ewoc-locate leman-ewoc))
                   when (found-sender-p maybe-event)
                   return (leman-event-sender maybe-event)))))))

(defun leman-room--add-member-face (string room)
  "Add member faces in ROOM to STRING.
If STRING begins with the name of a member in ROOM followed by a
colon or comma (as if STRING is a message addressing that
member), apply that member's displayname color face to that part
of the string.

Note that, if ROOM has no buffer, STRING is returned unchanged."
  ;; This only looks for a member name at the beginning of the string.  It would be neat to add
  ;; colors to every member mentioned in a message, but that would probably not perform well.

  ;; NOTE: This function may be called by `leman-notify' functions even when the room has
  ;; no buffer, and this function is designed to use events in a room buffer to more
  ;; quickly find the data it needs, so, for now, if the room has no buffer, we return
  ;; STRING unchanged.
  (pcase-let (((cl-struct leman-room (local (map buffer))) room))
    (if (buffer-live-p buffer)
        (save-match-data
          ;; This function may be called from a chain of others that use the match data, so
          ;; rather than depending on all of them to save the match data, we do it here.
          ;; FIXME: Member names containing spaces aren't matched.  Can this even be fixed reasonably?
          (when (string-match (rx bos (group (1+ (not blank))) (or ":" ",") (1+ blank)) string)
            (when-let* ((member-name (match-string 1 string))
                        (user (leman-room--user-named member-name buffer room))
                        (prism-color (or (leman-user-color user)
                                         (setf (leman-user-color user)
                                               (leman-room--user-color user)))))
              (add-face-text-property (match-beginning 1) (match-end 1)
                                      (list :foreground prism-color) nil string))))
      ;; Room has no buffer: return STRING as-is.
      string)))

;;;; Bookmark support

;; Especially useful with Burly: <https://github.com/alphapapa/burly.el>

(require 'bookmark)

(defun leman-room-bookmark-make-record ()
  "Return a bookmark record for the current `leman-room' buffer."
  (pcase-let* (((cl-struct leman-room (id room-id) canonical-alias display-name) leman-room)
               ((cl-struct leman-session user) leman-session)
               ((cl-struct leman-user (id session-id)) user))
    ;; MAYBE: Support bookmarking specific events in a room.
    (list (concat "Leman room: " display-name " (" canonical-alias ")")
          (cons 'session-id session-id)
          (cons 'room-id room-id)
          (cons 'handler #'leman-room-bookmark-handler))))

(defun leman-room-bookmark-handler (bookmark)
  "Show Leman room buffer for BOOKMARK."
  (pcase-let* ((`(,_name . ,(map session-id room-id)) bookmark)
               (session (leman-aprog1
                            (alist-get session-id leman-sessions nil nil #'equal)
                          (unless it
                            ;; MAYBE: Automatically connect.
                            (user-error "Session %s not connected: call `leman-connect' first" session-id))))
               (room (leman-aprog1
                         (leman-afirst (equal room-id (leman-room-id it))
                           (leman-session-rooms session))
                       (cl-assert it nil "Room %S not found on session %S" room-id session-id))))
    (leman-view-room room session)
    ;; HACK: Put point at the end of the room buffer.  This seems unusually difficult,
    ;; apparently because the bookmark library itself moves point after jumping to a
    ;; bookmark.  My attempts at setting the buffer's and window's points after calling
    ;; `leman-view-room' have had no effect.  `bookmark-after-jump-hook' sounds ideal, but
    ;; it does not seem to actually get run, so we use a timer that runs immediately after
    ;; `bookmark-jump' returns.
    (run-at-time nil nil (lambda ()
                           (goto-char (point-max))))))

;;;; Commands

(defun leman-room-override-name (name room session)
  "Set display NAME override for ROOM on SESSION.
Interactively, with prefix, prompt for room and session,
otherwise use current room.  If NAME is the empty string, remove
the override.

Sets account-data event of type
\"org.matrix.msc3015.m.room.name.override\".  This name is only
used by clients that respect this proposed override.  See
<https://github.com/matrix-org/matrix-spec-proposals/pull/3015#issuecomment-1451017296>."
  (interactive
   (leman-with-room-and-session
     (let* ((prompt (format "Set name override (%s): " (leman--format-room leman-room)))
            (name (read-string prompt nil nil (leman-room-display-name leman-room))))
       (list name leman-room leman-session))))
  (leman-put-account-data session "org.matrix.msc3015.m.room.name.override"
    (if (string-empty-p name)
        ;; `json-encode' wants an empty hash table to represent an empty map.  And
        ;; apparently there's no way to DELETE account-data events, so we have to re-PUT
        ;; it with empty content.
        (make-hash-table)
      (leman-alist "name" name))
    :room room))

(defun leman-room-flush-colors ()
  "Flush generated username/message colors.
Also, redisplay events in all open buffers.  The colors will be
regenerated according to the current background color.  Helpful
when switching themes or adjusting `leman-prism' options."
  (interactive)
  (cl-loop for user being the hash-values of leman-users
           do (setf (leman-user-color user) nil
                    (leman-user-message-color user) nil))
  (dolist (buffer (buffer-list))
    (when (eq 'leman-room-mode (buffer-local-value 'major-mode buffer))
      (with-current-buffer buffer
        (let ((window-start (when (get-buffer-window buffer)
                              (window-start (get-buffer-window buffer)))))
          (save-excursion
            (ewoc-refresh leman-ewoc))
          (when window-start
            (setf (window-start (get-buffer-window buffer)) window-start))))))
  ;; Flush notify-background-color colors.
  (cl-loop for (_id . session) in leman-sessions
           do (cl-loop for room in (leman-session-rooms session)
                       do (setf (alist-get 'notify-background-color (leman-room-local room)) nil)))
  ;; NOTE: The notifications buffer can't be refreshed because each event is from a
  ;; different room, and the `leman-room' variable is unset in the buffer.

  ;; (when-let (buffer (get-buffer "*Leman Notifications*"))
  ;;   (with-current-buffer buffer
  ;;     (ewoc-refresh leman-ewoc)))
  )

(defun leman-room--find-room-in-session (room-id room-alias session)
  "Return room in SESSION matching ROOM-ID or ROOM-ALIAS, if any."
  (or (and room-id (cl-find room-id (leman-session-rooms session)
                            :key #'leman-room-id :test #'equal))
      (and room-alias (cl-find room-alias (leman-session-rooms session)
                               :key #'leman-room-canonical-alias :test #'equal))))

(defun leman-room--browse-url-in-browser (url args)
  "Open URL in the web browser, bypassing Leman's matrix.to handler.
ARGS are passed to `browse-url'."
  (let ((handler (cons leman-room-matrix.to-url-regexp #'leman-room-browse-url)))
    ;; Note that `browse-url-handlers' was added in 28.1; prior to that
    ;; `browse-url-browser-function' served double-duty.
    ;; TODO: Remove compat code when requiring Emacs >=28.
    ;; (See also `leman-room-mode'.)
    (cond ((boundp 'browse-url-handlers)
           (let ((browse-url-handlers (remove handler browse-url-handlers)))
             (apply #'browse-url url args)))
          ((consp browse-url-browser-function)
           (let ((browse-url-browser-function (remove handler browse-url-browser-function)))
             (apply #'browse-url url args)))
          (t
           (apply #'browse-url url args)))))

(defun leman-room-browse-url (url &rest args)
  "Browse URL, using Leman for matrix.to URLs when possible.
Otherwise, fall back to `browse-url'.  When called outside of an
`leman-room' buffer, the variable `leman-session' must be bound
to the session in which to look for URL's room and event.  ARGS
are passed to `browse-url'."
  (interactive)
  (if (not (string-match leman-room-matrix.to-url-regexp url))
      ;; Not a matrix.to URL: leave it to the browser.
      (leman-room--browse-url-in-browser url args)
    ;; NOTE: Room IDs and event IDs in matrix.to URLs may be
    ;; percent-encoded (e.g. "!"/"#"/"$" as "%21"/"%23"/"%24").  All
    ;; groups must be extracted before unhexing, since that clobbers
    ;; the match data.
    (let* ((target-string (match-string 1 url))
           (event-string (match-string 2 url))
           (target (url-unhex-string target-string))
           (event-id (when event-string
                       (url-unhex-string event-string)))
           (room-id (when (string-prefix-p "!" target) target))
           (room-alias (when (string-prefix-p "#" target) target))
           ;; Prefer a joined room in the current session, then one in
           ;; any other session.  FOUND is a (ROOM . SESSION) cons.
           (found (or (when-let* ((session leman-session)
                                  (room (leman-room--find-room-in-session
                                         room-id room-alias session)))
                        (cons room session))
                      (cl-loop for (_id . session) in leman-sessions
                               for room = (leman-room--find-room-in-session
                                           room-id room-alias session)
                               when room
                               return (cons room session)))))
      (if found
          (progn
            ;; Found a joined room: view it and find the event.
            (pcase-let ((`(,room . ,session) found))
              (leman-view-room room session)
              (when event-id
                (leman-room-find-event event-id))))
        (pcase target
          ;; User links aren't handled yet; leave them to the browser.
          ((pred (string-prefix-p "@"))
           (leman-room--browse-url-in-browser url args))
          (_
           ;; Room not joined: offer to join it or load link in browser.
           (pcase-exhaustive
               (cadr (leman--read-multiple-choice
                      (format "Room <%s> not joined on current session.  Join it, or load link with browser?"
                              (or room-alias room-id))
                      '((?j "join" "Join room in Léman")
                        (?w "web browser" "Open URL in web browser"))
                      "\
You are not currently joined to that room.  You can either join the room
in Léman, or visit the link URL in your web browser."))
             ("join"
              (leman-join-room (or room-alias room-id) leman-session
                               :then (when event-id
                                       (lambda (room session)
                                         (leman-view-room room session)
                                         (leman-room-find-event event-id)))))
             ("web browser"
              (leman-room--browse-url-in-browser url args)))))))))

(defun leman-room-find-event (event-id)
  "Go to EVENT-ID in current buffer."
  (interactive)
  (cl-labels ((goto-event (event-id)
                (push-mark)
                (goto-char
                 (ewoc-location
                  (leman-room--ewoc-last-matching leman-ewoc
                    (lambda (data)
                      (and (leman-event-p data)
                           (equal event-id (leman-event-id data)))))))))
    (if (or (cl-find event-id (leman-room-timeline leman-room)
                     :key #'leman-event-id :test #'equal)
            (cl-find event-id (leman-room-state leman-room)
                     :key #'leman-event-id :test #'equal))
        ;; Found event in timeline: it should be in the EWOC, so go to it.
        (goto-event event-id)
      ;; Event not found in timeline: try to retro-load it.
      (message "Event %s not seen in current room.  Looking in history..." event-id)
      (let ((room leman-room))
        (leman-room-retro-to leman-room leman-session event-id
          ;; TODO: Add an ELSE argument to `leman-room-retro-to' and use it to give
          ;; a useful error here.
          :then (lambda ()
                  (with-current-buffer (alist-get 'buffer (leman-room-local room))
                    (goto-event event-id))))))))

(defun leman-room-set-composition-format (&optional localp)
  "Set message composition format.
If LOCALP (interactively, with prefix), set in current room's
buffer.  Sets `leman-room-send-message-filter'."
  (interactive (list current-prefix-arg))
  (let* ((formats (list (cons "Plain-text" nil)
                        (cons "Org-mode" #'leman-room-send-org-filter)))
         (selected-name (completing-read "Composition format: " formats nil 'require-match nil nil
                                         leman-room-send-message-filter))
         (selected-filter (alist-get selected-name formats nil nil #'equal)))
    (if localp
        (setq-local leman-room-send-message-filter selected-filter)
      (setq leman-room-send-message-filter selected-filter))))

(defun leman-room-set-message-format (format-spec)
  "Set `leman-room-message-format-spec' in current buffer to FORMAT-SPEC.
Interactively, prompts for the spec using suggested values of the
option."
  (interactive (list (let* ((choices (thread-last
                                       (get 'leman-room-message-format-spec 'custom-type)
                                       cdr
                                       (seq-filter (lambda (it)
                                                     (eq (car it) 'const)))
                                       (mapcar (lambda (it)
                                                 (cons (nth 2 it) (nth 3 it))))))
                            (choice (completing-read "Format: " (mapcar #'car choices))))
                       (or (alist-get choice choices nil nil #'equal)
                           choice))))
  (cl-assert leman-ewoc)
  (leman-room-message-format-spec-setter 'leman-room-message-format-spec format-spec 'local)
  (setf left-margin-width leman-room-left-margin-width
        right-margin-width leman-room-right-margin-width)
  (set-window-margins nil left-margin-width right-margin-width)
  (if leman-room-sender-in-headers
      (leman-room--insert-sender-headers leman-ewoc)
    (ewoc-filter leman-ewoc (lambda (node-data)
                              ;; Return non-nil for nodes that should stay.
                              (not (leman-user-p node-data)))))
  (ewoc-refresh leman-ewoc))

(defun leman-room-set-topic (session room topic)
  "Set ROOM's TOPIC on SESSION.
Interactively, with prefix, prompt for room and session,
otherwise use current room."
  (interactive
   (leman-with-room-and-session
     (list leman-session leman-room
           (read-string (format "New topic (%s): "
                                (leman-room-display-name leman-room))
                        (leman-room-topic leman-room) nil nil 'inherit-input-method))))
  (pcase-let* (((cl-struct leman-room (id room-id) display-name) room)
               (endpoint (format "rooms/%s/state/m.room.topic" (url-hexify-string room-id)))
               (data (leman-alist "topic" topic)))
    (leman-api session endpoint :method 'put :data (json-encode data)
      :then (lambda (_data)
              (message "Topic set (%s): %s" display-name topic)))))

(defun leman-room--encrypted-file-object (file-info content-uri)
  "Return the spec's EncryptedFile object for FILE-INFO and CONTENT-URI.
FILE-INFO is `leman-e2ee-encrypt-file''s response; CONTENT-URI is
the mxc:// URI of the uploaded ciphertext."
  (pcase-let* (((map ('key key) ('iv iv) ('sha256 sha256)) file-info))
    (leman-alist "url" content-uri
                 "v" "v2"
                 "key" (leman-alist "kty" "oct"
                                    "alg" "A256CTR"
                                    "k" key
                                    "ext" t
                                    "key_ops" (list "encrypt" "decrypt"))
                 "iv" iv
                 "hashes" (leman-alist "sha256" sha256))))

(cl-defun leman-room-send-file (file body room session &key (msgtype "m.file"))
  "Send FILE to ROOM on SESSION, using message BODY and MSGTYPE.
In an encrypted room, FILE is encrypted before upload (spec:
\"Sending encrypted attachments\"): the ciphertext is uploaded and
the event references it with a \"file\" object carrying the key,
IV, and hash, Megolm-encrypted like any other message content.
Interactively, with prefix, prompt for room and session,
otherwise use current room."
  ;; TODO: Support URLs to remote files.
  (interactive
   (leman-with-room-and-session
     (leman-room-with-typing
       (let* ((file (read-file-name (format "Send file (%s): " (leman-room-display-name leman-room))
                                    nil nil 'confirm))
              (body (leman-room-read-string
                     (format "Message body (%s): " (leman-room-display-name leman-room))
                     (file-name-nondirectory file) 'file-name-history nil 'inherit-input-method)))
         (list file body leman-room leman-session)))))
  ;; NOTE: The typing notification won't be quite right, because it'll be canceled while waiting
  ;; for the file to upload.  It would be awkward to handle that, so this will do for now.
  (when (yes-or-no-p (format "Upload file %S to room %S? "
                             file (leman-room-display-name room)))
    (pcase-let* ((filename (file-name-nondirectory file))
                 (extension (or (file-name-extension file) ""))
                 (mime-type (mailcap-extension-to-mime extension))
                 (size (file-attribute-size (file-attributes file)))
                 ;; E2EE: the file must be encrypted BEFORE upload
                 ;; (the homeserver must never see the plaintext), so
                 ;; the agent is required up front: failing here means
                 ;; nothing plaintext was uploaded.
                 (agent (and (leman-room--encrypted-p room)
                             leman-encrypt-send-content-function
                             (leman-session-e2ee session)))
                 (encrypted-file-info
                  (when (leman-room--encrypted-p room)
                    (unless agent
                      (signal 'leman-e2ee-error
                              (list "encrypt"
                                    (format "room %s is encrypted but no E2EE agent is running; see M-x leman-e2ee-status"
                                            (leman-room-id room)))))
                    (let ((encrypted (make-temp-file "leman-encrypt-")))
                      (prog1 (leman-e2ee-encrypt-file agent file encrypted)
                        ;; The temp file is deleted after the upload
                        ;; completes; if the upload fails it leaks
                        ;; into `temporary-file-directory', cleaned
                        ;; up by the system.
                        (setf file encrypted)))))
                 (data `(file ,file)))
      (leman-upload session :data data :filename filename
        :content-type (if encrypted-file-info "application/octet-stream" mime-type)
        :then (lambda (data)
                (message "Uploaded file %S.  Sending message..." file)
                (pcase-let* (((map ('content_uri content-uri)) data)
                             ((cl-struct leman-room (id room-id)) room)
                             ;; Per spec: "file" (an EncryptedFile)
                             ;; replaces "url" for encrypted
                             ;; attachments.
                             (media-entry (if encrypted-file-info
                                              (cons "file" (leman-room--encrypted-file-object
                                                            encrypted-file-info content-uri))
                                            (cons "url" content-uri)))
                             (content (cons media-entry
                                            (leman-alist "msgtype" msgtype
                                                         "body" body
                                                         "filename" filename
                                                         "info" (rassq-delete-all
                                                                 nil (leman-alist "mimetype" mime-type
                                                                                  "size" size)))))
                             (event-type "m.room.message"))
                  ;; E2EE: Megolm-encrypt the message content like any
                  ;; other message (the file key must not be visible
                  ;; to the homeserver).
                  (when leman-encrypt-send-content-function
                    (pcase-let ((`(,encrypted-content . ,encrypted-type)
                                 (funcall leman-encrypt-send-content-function session room content)))
                      (setf content encrypted-content
                            event-type encrypted-type)))
                  (leman-api session (format "rooms/%s/send/%s/%s"
                                             (url-hexify-string room-id) event-type
                                             (leman--update-transaction-id session))
                    :method 'put :data (json-encode content)
                    :then (apply-partially #'leman-room-send-event-callback
                                           :room room :session session :content content :data))))))))

(defun leman-room-send-image (file body room session)
  "Send image FILE to ROOM on SESSION, using message BODY.
Interactively, with prefix, prompt for room and session,
otherwise use current room."
  ;; TODO: Support URLs to remote files.
  (interactive
   (leman-with-room-and-session
     (leman-room-with-typing
       (let* ((file (read-file-name (format "Send image file (%s): " (leman-room-display-name leman-room))
                                    nil nil 'confirm))
              (body (leman-room-read-string
                     (format "Message body (%s): " (leman-room-display-name leman-room))
                     (file-name-nondirectory file) 'file-name-history nil 'inherit-input-method)))
         (list file body leman-room leman-session)))))
  (leman-room-send-file file body room session :msgtype "m.image"))

(defun leman-room-dnd-upload-file (uri _action)
  "Upload the file as specified by URI to the current room."
  (when-let ((file (dnd-get-local-file-name uri t)))
    (leman-room-send-file file (file-name-nondirectory file) leman-room leman-session
                          :msgtype (if (string-prefix-p "image/" (mailcap-file-name-to-mime-type file))
                                       "m.image"
                                     "m.file"))))

(cl-defun leman-room-join (id-or-alias session &key then)
  "Join room by ID-OR-ALIAS on SESSION.
THEN may be a function to call after joining the room (and when
`leman-room-join-view-buffer' is non-nil, after viewing the room
buffer).  It receives two arguments, the room and the session."
  (interactive (list (read-string "Join room (ID or alias): ")
                     (or leman-session
                         (leman-complete-session))))
  (cl-assert id-or-alias) (cl-assert session)
  (unless (string-match-p
           ;; According to tulir in #matrix-dev:matrix.org, ": is not
           ;; allowed in the localpart, all other valid unicode is
           ;; allowed.  (user ids and room ids are the same over
           ;; federation).  it's mostly a lack of validation in
           ;; synapse (arbitrary unicode isn't intentionally allowed,
           ;; but it's not disallowed either)".  See
           ;; <https://matrix.to/#/!jxlRxnrZCsjpjDubDX:matrix.org/$Cnb53UQdYnGFizM49Aje_Xs0BxVdt-be7Dnm7_k-0ho>.
           ;; Also, some homeservers (e.g. Conduit) generate room IDs
           ;; without a server name part, so the server part is
           ;; optional; the server still does the authoritative
           ;; validation.
           (rx bos (or "#" "!") (1+ (not (any ":")))
               (optional ":" (1+ (not (any ":")))))
           id-or-alias)
    (user-error "Invalid room ID or alias (use, e.g. \"#ROOM-ALIAS:SERVER\")"))
  (let ((endpoint (format "join/%s" (url-hexify-string id-or-alias))))
    (leman-api session endpoint :method 'post :data "{}"
      :then (lambda (data)
              ;; NOTE: This generates a symbol and sets its function value to a lambda
              ;; which removes the symbol from the hook, removing itself from the hook.
              ;; TODO: When requiring Emacs 27, use `letrec'.
              (pcase-let* (((map ('room_id room-id)) data)
                           (then-fns (delq nil
                                           (list (when leman-room-join-view-buffer
                                                   (lambda (room session)
                                                     (leman-view-room room session)))
                                                 then)))
                           (then-fn-symbol (gensym (format "leman-join-%s" id-or-alias)))
                           (then-fn (lambda (session)
                                      (when-let ((room (cl-loop for room in (leman-session-rooms session)
                                                                when (equal room-id (leman-room-id room))
                                                                return room)))
                                        ;; In case the join event is not in this next sync
                                        ;; response, make sure the room is found before removing
                                        ;; the function and joining the room.
                                        (remove-hook 'leman-sync-callback-hook then-fn-symbol)
                                        ;; FIXME: Probably need to unintern the symbol.
                                        (dolist (fn then-fns)
                                          (funcall fn room session))))))
                (setf (symbol-function then-fn-symbol) then-fn)
                (add-hook 'leman-sync-callback-hook then-fn-symbol)
                (message "Joined room: %s" room-id)))
      :else (lambda (plz-error)
              (pcase-let* (((cl-struct plz-error response) plz-error)
                           ((cl-struct plz-response status body) response)
                           ((map error) (json-read-from-string body)))
                (pcase status
                  ((or 403 429) (error "Unable to join room %s: %s" id-or-alias error))
                  (_ (error "Unable to join room %s: %s %S" id-or-alias status plz-error))))))))
(defalias 'leman-join-room #'leman-room-join)

(defun leman-room-goto-prev ()
  "Go to the previous message in buffer."
  (interactive)
  (if (>= (point) (- (point-max) 2))
      ;; Point is actually on the last event, but it doesn't appear to be: move point to
      ;; the beginning of that event.
      (ewoc-goto-node leman-ewoc (leman-room--ewoc-last-matching leman-ewoc #'leman-event-p))
    ;; Go to previous event.
    (leman-room-goto-next :next-fn #'ewoc-prev)))

(cl-defun leman-room-goto-next (&key (next-fn #'ewoc-next))
  "Go to the next message in buffer.
NEXT-FN is passed to `leman-room--ewoc-next-matching', which
see."
  (interactive)
  (if-let (node (leman-room--ewoc-next-matching leman-ewoc
                  (ewoc-locate leman-ewoc) #'leman-event-p next-fn))
      (ewoc-goto-node leman-ewoc node)
    (if (= (point) (point-max))
        ;; Already at end of buffer: signal error.
        (user-error "End of events")
      ;; Go to end-of-buffer so new messages will auto-scroll.
      (goto-char (point-max)))))

(defun leman-room-scroll-down-command ()
  "Scroll down, and load NUMBER earlier messages when at top."
  (interactive)
  (condition-case _err
      (scroll-down nil)
    (beginning-of-buffer
     (call-interactively #'leman-room-retro))))

(defun leman-room-mwheel-scroll (event)
  "Scroll according to EVENT, loading earlier messages when at top."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (mwheel-scroll event)
    (when (= (point-min) (window-start))
      (call-interactively #'leman-room-retro))))

;; TODO: Unify these retro-loading functions.

(cl-defun leman-room-retro
    (room session number &key buffer
          (then (apply-partially #'leman-room-retro-callback room session)))
  ;; FIXME: Naming things is hard.
  "Retrieve NUMBER older messages in ROOM on SESSION."
  (interactive (list leman-room leman-session
                     (cl-typecase current-prefix-arg
                       (null leman-room-retro-messages-number)
                       (list (read-number "Number of messages: "))
                       (number current-prefix-arg))
                     :buffer (current-buffer)))
  (unless leman-room-retro-loading
    (pcase-let* (((cl-struct leman-room id prev-batch) room)
                 (endpoint (format "rooms/%s/messages" (url-hexify-string id))))
      ;; We use a timeout of 30, because sometimes the server can take a while to
      ;; respond, especially if loading, e.g. hundreds or thousands of events.
      (leman-api session endpoint :timeout 30
        :params (remq nil
                      (list (when prev-batch
                              (list "from" prev-batch))
                            (list "dir" "b")
                            (list "limit" (number-to-string number))
                            (list "filter" (json-encode leman-room-messages-filter))))
        :then then
        :else (lambda (plz-error)
                (when buffer
                  (with-current-buffer buffer
                    (setf leman-room-retro-loading nil)))
                (signal 'leman-api-error (list (format "Loading %s earlier messages failed" number)
                                               plz-error))))
      (message "Loading %s earlier messages..." number)
      (setf leman-room-retro-loading t))))

(cl-defun leman-room-retro-to (room session event-id &key then (batch-size 100) (limit 1000))
  "Retrieve messages in ROOM on SESSION back to EVENT-ID.
When event is found, call function THEN.  Search in batches of
BATCH-SIZE events up to a total of LIMIT."
  (declare (indent defun))
  (cl-assert
   ;; Ensure the event hasn't already been retrieved.
   (not (gethash event-id (leman-session-events session))))
  (let* ((total-retrieved 0)
         ;; TODO: Use letrec someday.
         (callback-symbol (gensym "leman-room-retro-to-callback-"))
         (callback (lambda (data)
                     (leman-room-retro-callback room session data)
                     (if (gethash event-id (leman-session-events session))
                         (progn
                           (message "Found event %S" event-id)
                           ;; FIXME: Probably need to unintern the symbol.
                           (when then
                             (funcall then)))
                       ;; FIXME: What if it hits the beginning of the timeline?
                       (if (>= (cl-incf total-retrieved batch-size) limit)
                           (message "%s older events retrieved without finding event %S"
                                    limit event-id)
                         (message "Looking back for event %S (%s/%s events retrieved)"
                                  event-id total-retrieved limit)
                         (leman-room-retro room session  batch-size
                                           :buffer (alist-get 'buffer (leman-room-local room))
                                           :then callback-symbol))))))
    (fset callback-symbol callback)
    (leman-room-retro room session batch-size
                      :buffer (alist-get 'buffer (leman-room-local room))
                      :then callback-symbol)))

(cl-defun leman-room-retro-to-token (room session from to
                                          &key (batch-size 100) (limit 1000))
  "Retrieve messages in ROOM on SESSION back from FROM to TO.
Retrieve batches of BATCH-SIZE up to total LIMIT.  FROM and TO
are sync batch tokens.  Used for, e.g. filling gaps in
\"limited\" sync responses."
  ;; NOTE: We don't set `leman-room-retro-loading' since the room may
  ;; not have a buffer.  This could theoretically allow a user to
  ;; overlap manual scrollback-induced loading of old messages with
  ;; this gap-filling loading, but that shouldn't matter, and probably
  ;; would be very rare, anyway.
  (pcase-let* (((cl-struct leman-room id) room)
               (endpoint (format "rooms/%s/messages" (url-hexify-string id)))
               (then
                (lambda (data)
                  (leman-room-retro-callback room session data
                                             :set-prev-batch nil)
                  (pcase-let* (((map end chunk) data))
		    ;; HACK: Comparing the END and TO tokens ought to
		    ;; work for determining whether we are done
		    ;; filling, but it isn't (maybe the server isn't
		    ;; returning the TO token as END when there are no
		    ;; more events), so instead we'll check the length
		    ;; of the chunk.
                    (unless (< (length chunk) batch-size)
                      ;; More pages remain to be loaded.
                      (let ((remaining-limit (- limit batch-size)))
                        (if (not (> remaining-limit 0))
                            ;; FIXME: This leaves a gap if it's larger than 1,000 events.
                            ;; Probably, the limit should be configurable, but it would be good
                            ;; to find some way to remember the gap and fill it if the user
                            ;; scrolls to it later (although that might be very awkward to do).
                            (display-warning 'leman-room-retro-to-token
                                             (format "Loaded events in %S (%S) without filling gap; not filling further"
                                                     (leman-room-display-name room)
                                                     (or (leman-room-canonical-alias room)
                                                         (leman-room-id room))))
			  ;; FIXME: Remove this message after further testing.
                          (message "Leman: Continuing to fill gap in %S (%S) (remaining limit: %s)"
                                   (leman-room-display-name room)
                                   (or (leman-room-canonical-alias room)
                                       (leman-room-id room))
                                   remaining-limit)
                          (leman-room-retro-to-token
                           room session end to :limit remaining-limit))))))))
    ;; FIXME: Remove this message after further testing.
    (message "Leman: Filling gap in %S (%S)"
	     (leman-room-display-name room)
             (or (leman-room-canonical-alias room)
                 (leman-room-id room)))
    (leman-api session endpoint :timeout 30
      :params (list (list "from" from)
                    (list "to" to)
                    (list "dir" "b")
                    (list "limit" (number-to-string batch-size))
                    (list "filter" (json-encode leman-room-messages-filter)))
      :then then
      :else (lambda (plz-error)
              (signal 'leman-api-error
                      (list (format "Filling gap in %S (%S) failed"
                                    (leman-room-display-name room)
                                    (or (leman-room-canonical-alias room)
                                        (leman-room-id room)))
                            plz-error))))))

;; NOTE: `declare-function' doesn't recognize cl-defun forms, so this declaration doesn't work.
(declare-function leman--sync "leman" t t)
(defun leman-room-sync (session &optional force)
  "Sync SESSION (interactively, current buffer's).
If FORCE (interactively, with prefix), cancel any outstanding
sync requests.  Also, update any room list buffers."
  (interactive (list leman-session current-prefix-arg))
  (leman--sync session :force force)
  (cl-loop for buffer in (buffer-list)
           when (member (buffer-local-value 'major-mode buffer)
                        '(leman-room-list-mode leman-tabulated-room-list-mode))
           do (with-current-buffer buffer
                (revert-buffer))))

(defun leman-room-retry-decryption ()
  "Retry decryption of undecryptable events of the current session.
Events that failed to decrypt because their room keys had not
arrived yet (e.g. old messages fetched before importing keys with
`leman-e2ee-import-keys' or restoring a key backup) are decrypted
in place when this succeeds.  It also runs automatically after a
sync delivers room keys."
  (interactive)
  (unless (leman-session-e2ee leman-session)
    (user-error "Leman E2EE: no agent running (try reconnecting)"))
  (pcase-let* ((`(,decrypted . ,pending)
                (leman-e2ee--retry-decryption leman-session)))
    (leman-message "Leman E2EE: decrypted %s of %s undecryptable events (%s remaining)"
                   decrypted pending (- pending decrypted))))

(defun leman-room-view-event (event)
  "Pop up buffer showing details of EVENT (interactively, the one at point).
EVENT should be an `leman-event' or `leman-room-membership-events' struct."
  (interactive (list (ewoc-data (ewoc-locate leman-ewoc))))
  (require 'pp)
  (cl-labels ((event-alist (event)
                (leman-alist :id (leman-event-id event)
                             :sender (leman-user-id (leman-event-sender event))
                             :content (leman-event-content event)
                             :origin-server-ts (leman-event-origin-server-ts event)
                             :type (leman-event-type event)
                             :state-key (leman-event-state-key event)
                             :unsigned (leman-event-unsigned event)
                             :receipts (leman-event-receipts event)
                             :local (leman-event-local event))))
    (let* ((buffer-name (format "*Leman event: %s*"
                                (cl-typecase event
                                  (leman-room-membership-events "[multiple events]")
                                  (leman-event (leman-event-id event)))))
           (event (cl-typecase event
                    (leman-room-membership-events
                     (mapcar #'event-alist (leman-room-membership-events-events event)))
                    (leman-event (event-alist event))))
           (inhibit-read-only t))
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (pp event (current-buffer))
        (view-mode)
        (pop-to-buffer (current-buffer))))))

(defun leman-room-dispatch-new-message ()
  "Write a new message in accordance with `leman-room-compose-method'."
  (interactive)
  (call-interactively
   (cl-case leman-room-compose-method
     (compose-buffer 'leman-room-compose-message)
     (t 'leman-room-send-message))))

(defun leman-room-dispatch-new-message-alt ()
  "Inverse of `leman-room-dispatch-new-message'."
  (interactive)
  (call-interactively
   (cl-case leman-room-compose-method
     (compose-buffer 'leman-room-send-message)
     (t 'leman-room-compose-message))))

(defun leman-room-dispatch-edit-message ()
  "Edit a message in accordance with `leman-room-compose-method'."
  (interactive)
  (call-interactively
   (cl-case leman-room-compose-method
     (compose-buffer 'leman-room-compose-edit)
     (t 'leman-room-edit-message))))

(defun leman-room-dispatch-reply-to-message ()
  "Reply to a message in accordance with `leman-room-compose-method'."
  (interactive)
  (call-interactively
   (cl-case leman-room-compose-method
     (compose-buffer 'leman-room-compose-reply)
     (t 'leman-room-write-reply))))

(defun leman-room-dispatch-send-message ()
  "Send a message in accordance with `leman-room-compose-method'."
  (interactive)
  (call-interactively
   (cl-case leman-room-compose-method
     (compose-buffer #'leman-room-compose-send-direct)
     (t #'leman-room-compose-send))))

(cl-defun leman-room-send-message (room session &key body formatted-body replying-to-event)
  "Send message to ROOM on SESSION with BODY and FORMATTED-BODY.
Interactively, with prefix, prompt for room and session,
otherwise use current room.

REPLYING-TO-EVENT may be an event the message is in reply to; the
message will reference it appropriately.

If `leman-room-send-message-filter' is non-nil, the message's
content alist is passed through it before sending.  This may be
used to, e.g. process the BODY into another format and add it to
the content (e.g. see `leman-room-send-org-filter')."
  (interactive
   (leman-with-room-and-session
     (let* ((prompt (format "Send message (%s): " (leman-room-display-name leman-room)))
            (body (leman-room-with-typing
                    (leman-room-read-string prompt nil 'leman-room-message-history
                                            nil 'inherit-input-method))))
       (list leman-room leman-session :body body))))
  (leman-send-message room session :body body :formatted-body formatted-body
    :replying-to-event replying-to-event :filter leman-room-send-message-filter
    :then #'leman-room-send-event-callback)
  ;; NOTE: This assumes that the selected window is the buffer's window.  For now
  ;; this is almost surely the case, but in the future, we might let the function
  ;; send messages to other rooms more easily, so this assumption might not hold.
  (when-let* ((buffer (alist-get 'buffer (leman-room-local room)))
              (window (get-buffer-window buffer)))
    (with-selected-window window
      (when (>= (window-point) (ewoc-location (ewoc-nth leman-ewoc -1)))
        ;; Point is on last event: advance it to eob so that when the event is received
        ;; back, the window will scroll.  (This might not always be desirable, because
        ;; the user might have point on that event for a reason, but I think in most
        ;; cases, it will be what's expected and most helpful.)
        (setf (window-point) (point-max))))))

(cl-defun leman-room-send-emote (room session &key body)
  "Send emote to ROOM on SESSION with BODY.
Interactively, with prefix, prompt for room and session,
otherwise use current room.

If `leman-room-send-message-filter' is non-nil, the message's
content alist is passed through it before sending.  This may be
used to, e.g. process the BODY into another format and add it to
the content (e.g. see `leman-room-send-org-filter')."
  (interactive
   (leman-with-room-and-session
     (let* ((prompt (format "Send emote (%s): " (leman-room-display-name leman-room)))
            (body (leman-room-with-typing
                    (leman-room-read-string prompt nil 'leman-room-emote-history
                                            nil 'inherit-input-method))))
       (list leman-room leman-session :body body))))
  (cl-assert (not (string-empty-p body)))
  (pcase-let* (((cl-struct leman-room (local (map buffer))) room)
               (window (when buffer (get-buffer-window buffer))))
    (leman-send-message room session :body body :msgtype "m.emote"
      :filter leman-room-send-message-filter
      :then (apply-partially #'leman-room-send-event-callback :room room
                             :session session))
    ;; NOTE: This assumes that the selected window is the buffer's window.  For now
    ;; this is almost surely the case, but in the future, we might let the function
    ;; send messages to other rooms more easily, so this assumption might not hold.
    (when window
      (with-selected-window window
        (when (>= (window-point) (ewoc-location (ewoc-nth leman-ewoc -1)))
          ;; Point is on last event: advance it to eob so that when the event is received
          ;; back, the window will scroll.  (This might not always be desirable, because
          ;; the user might have point on that event for a reason, but I think in most
          ;; cases, it will be what's expected and most helpful.)
          (setf (window-point) (point-max)))))))

(cl-defun leman-room-send-event-callback (&key data room session content)
  "Callback for event-sending functions.
DATA is the parsed JSON object.  If DATA's event ID is already
present in SESSION's events table, show an appropriate warning
mentioning the ROOM and CONTENT."
  (pcase-let* (((map ('event_id event-id)) data))
    (when (and leman-room-warn-for-already-seen-messages
               (gethash event-id (leman-session-events session)))
      (let ((message (format "Event ID %S already seen in session %S.  This may indicate a reused transaction ID, which could mean that the event was not sent to the room (%S).  You may need to disconnect, delete the `leman-sessions-file', and connect again to start a new session.  Alternatively, this can happen if the event's sent-confirmation is received after the event itself is received in the next sync response, in which case no action is needed."
                             event-id (leman-user-id (leman-session-user session))
                             (leman-room-display-name room))))
        (when content
          (setf message (concat message (format " Event content: %S" content))))
        (display-warning 'leman-room-send-event-callback message)))
    (when (eq 'send leman-room-mark-rooms-read)
      ;; Move read markers.
      (when-let ((buffer (alist-get 'buffer (leman-room-local room))))
        (with-current-buffer buffer
          ;; NOTE: The new event may not exist in the buffer yet, so
          ;; we just have to use the last one.
          ;; FIXME: When we add local echo, this can be fixed.
          (save-excursion
            (goto-char (ewoc-location
                        (leman-room--ewoc-last-matching leman-ewoc #'leman-event-p)))
            (call-interactively #'leman-room-mark-read)))))))

(defun leman-room-edit-message-prepare ()
  "Bindings for `leman-room-edit-message' and `leman-room-compose-edit'."
  (cl-assert leman-ewoc) (cl-assert leman-session)
  (unless (leman-event-p (ewoc-data (ewoc-locate leman-ewoc)))
    (user-error "No event at point"))
  ;; Bindings for... `event' (from ewoc).
  (pcase-let* ((event (ewoc-data (ewoc-locate leman-ewoc)))
               ;; `user' (from leman-session).
               ((cl-struct leman-session user) leman-session)
               ;; `sender', `body' (from event).
               ((cl-struct leman-event sender (content (map body))) event))
    (unless (equal (leman-user-id sender) (leman-user-id user))
      (user-error "You may only edit your own messages"))
    ;; Remove any leading asterisk from the plain-text body.
    (setf body (replace-regexp-in-string (rx bos "*" (1+ space)) "" body t t))
    (list event body)))

(defun leman-room-edit-message (event room session body)
  "Edit EVENT in ROOM on SESSION to have new BODY.
The message must be one sent by the local user.  If EVENT is
itself an edit of another event, the original event is edited."
  ;; See also `leman-room-compose-edit'.
  (interactive (leman-room-with-highlighted-event-at (point)
                 (cl-destructuring-bind (leman-room-editing-event body)
                     (leman-room-edit-message-prepare)
                   (leman-room-with-typing
                     (let* ((prompt (format "Edit message (%s): "
                                            (leman-room-display-name leman-room)))
                            (body (leman-room-read-string prompt body 'leman-room-message-history
                                                          nil 'inherit-input-method)))
                       (when (string-empty-p body)
                         (user-error "To delete a message, use command `leman-room-delete-message'"))
                       (when (yes-or-no-p (format "Edit message to: %S? " body))
                         (list leman-room-editing-event leman-room leman-session body)))))))
  (let* ((new-content (leman-alist "body" body
                                   "msgtype" "m.text"))
         (_ (when leman-room-send-message-filter
              (setf new-content (funcall leman-room-send-message-filter new-content room))))
         (original-event (leman--original-event-for event session))
         (content (leman-alist "msgtype" "m.text"
                               "body" body
                               "m.new_content" new-content
                               "m.relates_to" (leman-alist
                                               "rel_type" "m.replace"
                                               "event_id" (leman-event-id original-event))))
         ;; E2EE: like `leman-send-message', encrypt the edit when the
         ;; room requires it (the function also determines the event
         ;; type to send).
         (event-type "m.room.message")
         (_ (when leman-encrypt-send-content-function
              (pcase-let ((`(,encrypted-content . ,encrypted-type)
                           (funcall leman-encrypt-send-content-function session room content)))
                (setf content encrypted-content
                      event-type encrypted-type))))
         (endpoint (format "rooms/%s/send/%s/%s" (url-hexify-string (leman-room-id room))
                           event-type (leman--update-transaction-id session))))
    (leman-api session endpoint :method 'put :data (json-encode content)
      :then (apply-partially #'leman-room-send-event-callback :room room :session session
                             :content content :data))))

(defun leman-room-delete-message (event room session &optional reason)
  "Delete EVENT in ROOM on SESSION, optionally with REASON."
  (interactive (leman-room-with-highlighted-event-at (point)
                 (unless (leman-event-p (ewoc-data (ewoc-locate leman-ewoc)))
                   (user-error "No event at point"))
                 (if (yes-or-no-p "Delete this event? ")
                     (list (ewoc-data (ewoc-locate leman-ewoc))
                           leman-room leman-session (read-string "Reason (optional): " nil nil nil 'inherit-input-method))
                   ;; HACK: This isn't really an error, but is there a cleaner way to cancel?
                   (user-error "Message not deleted"))))
  (leman-redact (leman--original-event-for event session) room session reason))

(defun leman-room-write-reply (event)
  "Write and send a reply to EVENT.
Interactively, to event at point."
  ;; See also `leman-room-compose-reply'.
  (interactive (progn (cl-assert leman-ewoc)
                      (list (ewoc-data (ewoc-locate leman-ewoc)))))
  (cl-assert leman-room) (cl-assert leman-session) (cl-assert (leman-event-p event))
  (let ((leman-room-replying-to-event event))
    (leman-room-with-highlighted-event-at (point)
      (pcase-let* ((room leman-room)
                   (session leman-session)
                   (prompt (format "Send reply (%s): " (leman-room-display-name room)))
                   (leman-room-read-string-setup-hook
                    (lambda ()
                      (setq-local leman-room-replying-to-event event)))
                   (body (leman-room-with-typing
                           (leman-room-read-string prompt nil 'leman-room-message-history
                                                   nil 'inherit-input-method))))
        ;; NOTE: `leman-room-send-message' looks up the original event, so we pass `event'
        ;; as :replying-to-event.
        (leman-room-send-message room session :body body :replying-to-event event)))))

(when (assoc "emoji" input-method-alist)
  (defun leman-room-use-emoji-input-method ()
    "Activate the emoji input method in the current buffer."
    (interactive)
    (set-input-method "emoji")))

(defun leman-room-send-reaction (key position &optional event)
  "Send reaction of KEY to event at POSITION.
KEY should be a reaction string, e.g. \"👍\".

Interactively, send reaction to event at point.  The user option
`leman-room-reaction-picker' controls how the reaction string
is selected, or rather controls the initial mechanism, since the
user can always cancel that command with \\[keyboard-quit] and
choose a different one using the key bindings in
`leman-room-reaction-map' (note that other than `insert-char',
these all require at least version 29 of Emacs):

\\{leman-room-reaction-map}"
  (interactive
   (let ((event (ewoc-data (ewoc-locate leman-ewoc))))
     (unless (leman-event-p event)
       (user-error "No event at point"))
     (list (minibuffer-with-setup-hook
               (lambda ()
                 (setq-local after-change-functions
                             (list (lambda (&rest _)
                                     (catch 'exit
                                       (exit-minibuffer))
                                     (throw 'selected (minibuffer-contents)))))
                 (use-local-map
                  (make-composed-keymap leman-room-reaction-map (current-local-map)))
                 (let ((enable-recursive-minibuffers t))
                   (call-interactively leman-room-reaction-picker)))
             (catch 'selected
               (read-string "Reaction: ")))
           (point))))
  ;; SPEC: MSC2677 <https://github.com/matrix-org/matrix-doc/pull/2677>
  ;; HACK: We could simplify this by storing the key in a text property...
  (leman-room-with-highlighted-event-at position
    (pcase-let* ((event (or event
                            (ewoc-data (ewoc-locate leman-ewoc position))
                            (user-error "No event at point")))
                 ;; NOTE: Sadly, `face-at-point' doesn't work here because, e.g. if
                 ;; hl-line-mode is enabled, it only returns the hl-line face.
                 ((cl-struct leman-event (id event-id)) event)
                 ((cl-struct leman-room (id room-id)) leman-room)
                 (endpoint (format "rooms/%s/send/m.reaction/%s" (url-hexify-string room-id)
                                   (leman--update-transaction-id leman-session)))
                 (content (leman-alist "m.relates_to"
                                       (leman-alist "rel_type" "m.annotation"
                                                    "event_id" event-id
                                                    "key" key))))
      (leman-api leman-session endpoint :method 'put :data (json-encode content)
        :then (apply-partially #'leman-room-send-event-callback
                               :room leman-room :session leman-session :content content
                               :data)))))

(defun leman-room-toggle-reaction (key event room session)
  "Toggle reaction of KEY to EVENT in ROOM on SESSION."
  (interactive
   (cl-labels
       ((face-at-point-p (face)
          (let ((face-at-point (get-text-property (point) 'face)))
            (or (eq face face-at-point)
                (and (listp face-at-point)
                     (member face face-at-point)))))
        (buffer-substring-while (beg pred &key (forward-fn #'forward-char))
          "Return substring of current buffer from BEG while PRED is true."
          (save-excursion
            (goto-char beg)
            (cl-loop while (funcall pred)
                     do (funcall forward-fn)
                     finally return (buffer-substring-no-properties beg (point)))))
        (key-at (pos)
          (or (get-text-property pos 'leman-reaction-key)
              (cond ((face-at-point-p 'leman-room-reactions-key)
                     (buffer-substring-while
                      pos (lambda () (face-at-point-p 'leman-room-reactions-key))))
                    ((face-at-point-p 'leman-room-reactions)
                     ;; Point is in a reaction button but after the key.
                     (buffer-substring-while
                      (button-start (button-at pos))
                      (lambda () (face-at-point-p 'leman-room-reactions-key))))))))
     (list (or (key-at (point))
               (char-to-string (read-char-by-name "Reaction (prepend \"*\" for substring search): ")))
           (ewoc-data (ewoc-locate leman-ewoc))
           leman-room leman-session)))
  (pcase-let* (((cl-struct leman-event (local (map reactions))) event)
               ((cl-struct leman-session user) session)
               ((cl-struct leman-user (id user-id)) user))
    (if-let (reaction-event (cl-find-if (lambda (event)
                                          (and (equal user-id (leman-user-id (leman-event-sender event)))
                                               (equal key (map-nested-elt (leman-event-content event) '(m.relates_to key)))))
                                        reactions))
        ;; Already sent this reaction: redact it.
        (leman-redact reaction-event room session)
      ;; Send reaction.
      (leman-room-send-reaction key (point)))))

(defun leman-room-reaction-button-action (button)
  "Push reaction BUTTON at point."
  ;; TODO: Toggle reactions off with redactions (not in spec yet, but Element does it).
  (save-excursion
    (goto-char (button-start button))
    (call-interactively #'leman-room-toggle-reaction)))

(defun leman-room-toggle-space (room space session)
  ;; Naming things is hard, but this seems the best balance between concision, ambiguity,
  ;; and consistency.  The docstring is always there.  (Or there's the sci-fi angle:
  ;; "spacing" a room...)
  "Toggle ROOM's membership in SPACE on SESSION."
  (interactive
   (leman-with-room-and-session
     :prompt-form (leman-complete-room :session leman-session
                    :predicate (lambda (room) (not (leman--space-p room))) )
     (pcase-let* ((prompt (format "Toggle room %S's membership in space: "
                                  (leman--format-room leman-room)))
                  ;; TODO: Use different face for spaces the room is already in.
                  (`(,space ,_session) (leman-complete-room :session leman-session :prompt prompt :suggest nil
                                         :predicate #'leman--space-p)))
       (list leman-room space leman-session))))
  (pcase-let* (((cl-struct leman-room (id child-id)) room)
               (routing-server (progn
                                 (string-match (rx (1+ (not (any ":"))) ":" (group (1+ anything))) child-id)
                                 (match-string 1 child-id)))
               (action (if (leman--room-in-space-p room space)
                           'remove 'add))
               (data (pcase action
                       ('add (leman-alist "via" (vector
                                                 ;; FIXME: Finish and use the routing function.
                                                 ;; (leman--room-routing room)
                                                 routing-server)))
                       ('remove (make-hash-table)))))
    (leman-put-state space "m.space.child" child-id data session
      :then (lambda (response-data)
              ;; It appears that the server doesn't send the new event in the next sync (at
              ;; least, not to the client that put the state), so we must simulate receiving it.
              (pcase-let* (((map event_id) response-data)
                           ((cl-struct leman-session user) session)
                           ((cl-struct leman-room (id child-id)) room)
                           (fake-event (make-leman-event :id event_id :type "m.space.child"
                                                         :sender user :state-key child-id
                                                         :content (json-read-from-string (json-encode data)))))
                (push fake-event (leman-room-timeline space))
                (run-hook-with-args 'leman-event-hook fake-event space session))
              (leman-message "Room %S %s space %S"
                             (leman--format-room room)
                             (pcase action
                               ('add "added to")
                               ('remove "removed from"))
                             (leman--format-room space))))))

;;;; Functions

(defun leman-room-view (room session)
  "Switch to a buffer showing ROOM on SESSION.
Uses action `leman-view-room-display-buffer-action', which see."
  (interactive (leman-complete-room :session (leman-complete-session) :suggest nil
                 :predicate (lambda (room)
                              (not (leman--space-p room)))))
  (pcase-let* (((cl-struct leman-room (local (map buffer))) room))
    (unless (buffer-live-p buffer)
      (setf buffer (leman-room--buffer session room (leman-room--buffer-name room))
            (alist-get 'buffer (leman-room-local room))  buffer))
    ;; FIXME: This doesn't seem to work as desired, e.g. when
    ;; `leman-view-room-display-buffer-action' is set to `display-buffer-no-window'; I
    ;; guess because `pop-to-buffer' selects a window.
    (pop-to-buffer buffer leman-view-room-display-buffer-action)
    (run-hook-with-args 'leman-room-view-hook room session)))
(defalias 'leman-view-room #'leman-room-view)

(defun leman-room-view-hook-room-list-auto-update (_room session)
  "Call `leman-room-list-auto-update' with SESSION.
To be used in `leman-room-view-hook', which see."
  ;; This function is necessary because the hook is called with the room argument, which
  ;; `leman-room-list-auto-update' doesn't need.
  (declare (function leman-room-list-auto-update "leman-room-list"))
  (leman-room-list-auto-update session))

(defun leman-room--buffer-name (room)
  "Return name for ROOM's buffer."
  (concat leman-room-buffer-name-prefix
          (or (leman-room-display-name room)
              (setf (leman-room-display-name room)
                    (leman--room-display-name room)))
          leman-room-buffer-name-suffix))

(defun leman-room-goto-event (event)
  "Go to EVENT in current buffer."
  (if-let ((node (leman-room--ewoc-last-matching leman-ewoc
                   (lambda (data)
                     (and (leman-event-p data)
                          (equal (leman-event-id event) (leman-event-id data)))))))
      (goto-char (ewoc-location node))
    (error "Event not found in buffer: %S" (leman-event-id event))))

(defun leman-room--event-at (pos)
  "Return event at POS or signal an error."
  ;; TODO: Use this where appropriate.
  (save-excursion
    (goto-char pos)
    (cl-assert leman-ewoc)
    (let ((data (ewoc-data (ewoc-locate leman-ewoc))))
      (cl-typecase data
        (leman-event data)
        (otherwise (user-error "No event at point"))))))

(defun leman-room--process-retro-chunk (chunk session)
  "Process \"dir=b\" CHUNK of events for SESSION.
Convert the events to `leman-event' structs, discarding events
already known to SESSION, and return the new events as a list."
  ;; NOTE: The events are converted in place in the vector, which is
  ;; regrettable, but it's how we discard already-seen events.
  (declare (function leman--make-event "leman")
           (function leman--put-event "leman"))
  (cl-loop for event across-ref chunk
           do (if (gethash (alist-get 'event_id event) (leman-session-events session))
                  ;; Duplicate event: set to nil to be ignored.
                  (setf event nil)
                 ;; New event.  Decrypt E2EE events, if possible (the
                 ;; /messages endpoint's events include the room ID).
                 (setf event (leman-e2ee--decrypt-event-struct
                              session (alist-get 'room_id event) event))
                ;; HACK: Put events on events table.  See FIXME in caller about using the event hook.
                (leman--put-event event nil session))
           (leman-progress-update)
           finally do (setf chunk (seq-remove #'null chunk))
           finally return chunk))

(defun leman-room--insert-retro-events (room buffer chunk set-prev-batch end)
  "Insert earlier CHUNK events into ROOM's BUFFER.
If SET-PREV-BATCH and END, set ROOM's prev-batch slot to END."
  (with-current-buffer buffer
    (save-window-excursion
      ;; NOTE: See note in `leman--update-room-buffers'.
      (when-let ((buffer-window (get-buffer-window buffer)))
        (select-window buffer-window))
      ;; Always reset the flag, even if processing an event signals:
      ;; a stuck flag would block all future retro loading for the
      ;; room's buffer.
      (unwind-protect
          (progn
            ;; FIXME: Use retro-loading in event handlers, or in --handle-events, anyway.
            (leman-room--process-events chunk)
            ;; Don't set the slot if the response doesn't include an "end" token (that
            ;; would cause subsequent retro requests to fetch events from the end of the
            ;; timeline, as if we had just joined).
            (when (and set-prev-batch end)
              ;; This feels a little hacky, but maybe not too bad.
              (setf (leman-room-prev-batch room) end)))
        (setf leman-room-retro-loading nil)))))

(cl-defun leman-room-retro-callback (room session data
                                           &key (set-prev-batch t))
  "Push new DATA to ROOM on SESSION and add events to room buffer.
If SET-PREV-BATCH is nil, don't set ROOM's prev-batch slot to the
\"prev_batch\" token in response DATA (this should be set,
e.g. when filling timeline gaps as opposed to retrieving messages
before the earliest-seen message)."
  (declare (function leman--make-event "leman")
           (function leman--put-event "leman"))
  (pcase-let* (((cl-struct leman-room local) room)
	       ((map _start end chunk state) data)
                ((map buffer) local)
                (num-events (length chunk))
                ;; We do 3 things for chunk events, so we count them 3 times when
                ;; reporting progress.  (We also may receive some state events for
                ;; these chunk events, but we don't bother to include them in the
                ;; count, and we don't report progress for them, because they are
                ;; likely very few compared to the number of timeline events, which is
                ;; what the user is interested in (e.g. when loading 1000 earlier
                ;; messages in #emacs:matrix.org, only 31 state events were received).
                (progress-max-value (* 3 num-events)))
    ;; NOTE: Put the newly retrieved events at the end of the slots, because they should be
    ;; older events.  But reverse them first, because we're using "dir=b", which the
    ;; spec says causes the events to be returned in reverse-chronological order, and we
    ;; want to process them oldest-first (important because a membership event having a
    ;; user's displayname should be older than a message event sent by the user).
    ;; NOTE: The events in `chunk' and `state' are vectors, so we
    ;; convert them to a list before appending.
    (leman-debug num-events progress-max-value)
    (setf chunk (nreverse chunk)
          state (nreverse state))
    ;; FIXME: Like `leman--push-joined-room-events', this should probably run the `leman-event-hook' on the newly seen events.
    ;; Append state events.
    (cl-loop for event across-ref state
             do (setf event (leman--make-event event))
             finally do (setf (leman-room-state room)
                              (append (leman-room-state room) (append state nil))))
    (leman-with-progress-reporter (:reporter ("Leman: Processing earlier events..." 0 progress-max-value))
      (setf chunk (leman-room--process-retro-chunk chunk session)
            (leman-room-timeline room) (append (leman-room-timeline room) chunk))
      (when buffer
        ;; Insert events into the room's buffer.
        (leman-room--insert-retro-events room buffer chunk set-prev-batch end)))
    (message "Leman: Loaded %s earlier events." num-events)))

(defun leman-room--insert-events (events &optional retro)
  "Insert EVENTS into current buffer.
Calls `leman-room--insert-event' for each event and inserts
timestamp headers into appropriate places while maintaining
point's position.  If RETRO is non-nil, assume EVENTS are earlier
than any existing events, and only insert timestamp headers up to
the previously oldest event."
  (let (buffer-window point-node orig-first-node point-max-p)
    (when (get-buffer-window (current-buffer))
      ;; HACK: See below.
      (setf buffer-window (get-buffer-window (current-buffer))
            point-max-p (= (point) (point-max))))
    (when (and buffer-window retro)
      (setf point-node (ewoc-locate leman-ewoc (window-start buffer-window))
            orig-first-node (ewoc-nth leman-ewoc 0)))
    (save-window-excursion
      ;; NOTE: When inserting some events, seemingly only replies, if a different buffer's
      ;; window is selected, and this buffer's window-point is at the bottom, the formatted
      ;; events may be inserted into the wrong place in the buffer, even though they are
      ;; inserted into the EWOC at the right place.  We work around this by selecting the
      ;; buffer's window while inserting events, if it has one.  (I don't know if this is a bug
      ;; in EWOC or in this file somewhere.  But this has been particularly nasty to debug.)
      (when buffer-window
        (select-window buffer-window))
      (cl-loop for event being the elements of events
               do (leman-room--process-event event)
               do (leman-progress-update)))
    ;; Since events can be received in any order, we have to check the whole buffer
    ;; for where to insert new timestamp headers.  (Avoiding that would require
    ;; getting a list of newly inserted nodes and checking each one instead of every
    ;; node in the buffer.  Doing that now would probably be premature optimization,
    ;; though it will likely be necessary if users keep buffers open for busy rooms
    ;; for a long time, as the time to do this in each buffer will increase with the
    ;; number of events.  At least we only do it once per batch of events.)
    (leman-room--insert-ts-headers nil (when retro orig-first-node))
    (when leman-room-sender-in-headers
      (leman-room--insert-sender-headers leman-ewoc))
    (when buffer-window
      (cond (retro (with-selected-window buffer-window
                     (set-window-start buffer-window (ewoc-location point-node))
                     ;; TODO: Experiment with this.
                     (forward-line -1)))
            (point-max-p (set-window-point buffer-window (point-max)))))))

(cl-defun leman-room--send-typing (session room &key (typing t))
  "Send a typing notification for ROOM on SESSION."
  (when (leman-session-revoked-p session)
    ;; The session is signed out: stop the repeating timer and send
    ;; nothing.
    (when leman-room-typing-timer
      (when (timerp leman-room-typing-timer)
        (cancel-timer leman-room-typing-timer))
      (setf leman-room-typing-timer nil))
    (cl-return-from leman-room--send-typing))
  (pcase-let* (((cl-struct leman-session user) session)
               ((cl-struct leman-user (id user-id)) user)
               ((cl-struct leman-room (id room-id)) room)
               (endpoint (format "rooms/%s/typing/%s"
                                 (url-hexify-string room-id) (url-hexify-string user-id)))
               ;; Encode a real JSON boolean: `nil' would encode as `null', which
               ;; some servers (e.g. Conduit) reject because the spec requires a
               ;; boolean for "typing".  Also, the timeout is meaningless when
               ;; sending a not-typing notification, so it is omitted.
               (data (if typing
                         (leman-alist "typing" t "timeout" 20000)
                       (leman-alist "typing" json-false))))
    (leman-api session endpoint :method 'put :data (json-encode data)
      ;; We don't really care about the response, I think.
      :then #'ignore)))

(defcustom leman-room-mode-hook nil
  ;; Due to Emacs bug#68600, define the mode hook separately to avoid the mode
  ;; line constructs in the `leman-room-mode' mode name being copied verbatim
  ;; into the auto-generated docstring.
  "Hook run after entering `leman-room-mode'."
  :options '(visual-line-mode)
  :type 'hook
  :group 'leman-room)

(define-derived-mode leman-room-mode fundamental-mode
  `("Leman-Room"
    (:eval (unless (map-elt leman-syncs leman-session)
             (propertize ":Not-syncing"
                         'face 'font-lock-warning-face
                         'help-echo "Automatic syncing was interrupted; press \"g\" to resume"))))
  "Major mode for Leman room buffers.
This mode initializes a buffer to be used for showing events in
an Leman room.  It kills all local variables, removes overlays,
and erases the buffer.

\\{leman-room-mode--advertised-keymap}"
  (use-local-map leman-room-mode-effective-keymap)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (setf buffer-read-only t
        left-margin-width leman-room-left-margin-width
        right-margin-width leman-room-right-margin-width
        imenu-create-index-function #'leman-room--imenu-create-index-function
        ;; TODO: Use EWOC header/footer for, e.g. typing messages.
        leman-ewoc (ewoc-create #'leman-room--pp-thing))
  ;; Prevent line/wrap-prefix formatting properties being included in copied text.
  (setq-local filter-buffer-substring-function #'leman-room--buffer-substring-filter)
  ;; Set the URL handler.  Note that `browse-url-handlers' was added in 28.1;
  ;; prior to that `browse-url-browser-function' served double-duty.
  ;; TODO: Remove compat code when requiring Emacs >=28.
  ;; (See also `leman-room-browse-url'.)
  (let ((handler (cons leman-room-matrix.to-url-regexp #'leman-room-browse-url)))
    (if (boundp 'browse-url-handlers)
        (setq-local browse-url-handlers (cons handler browse-url-handlers))
      (setq-local browse-url-browser-function
                  (cons handler
                        (if (consp browse-url-browser-function)
                            browse-url-browser-function
                          (and browse-url-browser-function
                               (list (cons "." browse-url-browser-function))))))))
  (setq-local completion-at-point-functions
              '(leman-room--complete-members-at-point leman-room--complete-rooms-at-point))
  (setq-local dnd-protocol-alist (append '(("^file:///" . leman-room-dnd-upload-file)
                                           ("^file:" . leman-room-dnd-upload-file))
                                         dnd-protocol-alist)))

(add-hook 'leman-room-mode-hook 'visual-line-mode)

;;;###autoload
(define-minor-mode leman-room-self-insert-mode
  "When enabled, `self-insert-command' keys begin a new message.

The user options `leman-room-self-insert-chars' and
`leman-room-self-insert-commands' determine the specific keys and
commands which will have this effect.

When this mode is enabled, `leman-room-mode-self-insert-keymap'
takes precedence over `leman-room-mode-map', with the shadowed
key bindings in `leman-room-mode-map' becoming accessible via
`leman-room-mode-map-prefix-key'.

If you define custom key bindings in `leman-room-mode-map', you
should call `leman-room-self-insert-mode' after defining those
keys (rather than before).  Your bindings will be functional in
either case, but they may not appear in the help for
`leman-room-mode' if you define them afterwards.

If you bind keys in `leman-room-mode-self-insert-keymap', do so
via `leman-room-mode-self-insert-keymap-update-hook' (see which)."
  :init-value nil
  :global t
  :keymap nil
  :group 'leman-room
  ;; Ensure the self-insert and advertised keymaps are up to date.
  (if leman-room-self-insert-mode
      (leman-room-mode-self-insert-keymap-update)
    (setq leman-room-mode--advertised-keymap leman-room-mode-map))
  ;; Make the local keymap used by `leman-room-mode' reflect the state
  ;; of `leman-room-self-insert-mode'.
  (leman-room-mode-effective-keymap-update))

(defun leman-room-self-insert-new-message ()
  "Compose a new message beginning with the just-typed character."
  (interactive)
  ;; Re-issue the event which triggered this command.
  ;; (Typically a `self-insert-command' key binding.)
  (seq-doseq (key (reverse (this-command-keys-vector)))
    (push key unread-command-events))
  (call-interactively #'leman-room-dispatch-new-message))

(defun leman-room-read-string (prompt &optional initial-input history default-value inherit-input-method)
  "Call `read-from-minibuffer', binding variables and keys for Leman.
Arguments PROMPT, INITIAL-INPUT, HISTORY, DEFAULT-VALUE, and
INHERIT-INPUT-METHOD are as those expected by `read-string',
which see.  Runs hook `leman-room-read-string-setup-hook', which
see."
  (let ((room leman-room)
        (session leman-session))
    (minibuffer-with-setup-hook
        (lambda ()
          "Bind keys and variables locally (to be called in minibuffer)."
          (setq-local leman-room room)
          (setq-local leman-session session)
          (setq-local completion-at-point-functions
                      '(leman-room--complete-members-at-point leman-room--complete-rooms-at-point))
          (visual-line-mode 1)
          (run-hooks 'leman-room-read-string-setup-hook))
      (read-from-minibuffer prompt initial-input leman-room-minibuffer-map
                            nil history default-value inherit-input-method))))

(defun leman-room--encrypted-p (room)
  "Return non-nil if ROOM's state or invite state indicates encryption."
  (cl-loop for state in (list (leman-room-state room)
                              (leman-room-invite-state room))
           thereis (cl-find "m.room.encryption" state
                            :test #'equal :key #'leman-event-type)))

(defun leman-room--button (label action)
  "Return LABEL propertized as a button which calls ACTION."
  (propertize label
              'button '(t)
              'category 'default-button
              'mouse-face 'highlight
              'follow-link t
              'action action))

(defun leman-room--initial-header (_room)
  "Return initial header string for ROOM's buffer."
  "")

(defun leman-room--initial-footer (room)
  "Return initial footer string for ROOM's buffer."
  (pcase (leman-room-status room)
    ;; Set header and footer for an invited room.
    ('invite
     (concat (propertize "You've been invited to this room.  "
                         'face 'font-lock-warning-face)
             (leman-room--button
              "[Join this room]"
              (lambda (_button)
                ;; Kill the room buffer so it can be recreated after joining
                ;; (which will cleanly update the room's name, footer, etc).
                (let ((room leman-room)
                      (session leman-session))
                  (kill-buffer)
                  (message "Joining room... (buffer will be reopened after joining)")
                  (leman-room-join (leman-room-id room) session))))))
    (_ (if (leman--space-p room)
           (concat (propertize "This room is a space.  It is not for messaging, but only a grouping of other rooms.  "
                               'face 'font-lock-type-face)
                   (leman-room--button
                    "[View rooms in this space]"
                    (lambda (_button)
                      ;; Kill the room buffer so it can be recreated after viewing
                      ;; (which will cleanly update the room's name, footer, etc).
                      (let ((room leman-room)
                            (session leman-session))
                        (kill-buffer)
                        (message "Viewing space...")
                        (leman-view-space room session)))))
         ""))))

(defun leman-room--buffer (session room name)
  "Return buffer named NAME showing ROOM's events on SESSION.
If ROOM has no buffer, one is made and stored in the room's local
data slot."
  (or (map-elt (leman-room-local room) 'buffer)
      (let ((new-buffer (generate-new-buffer name)))
        (with-current-buffer new-buffer
          (leman-room-mode)
          (setf header-line-format (when leman-room-header-line-format
                                     'leman-room-header-line-format)
                leman-session session
                leman-room room
                list-buffers-directory (or (leman-room-canonical-alias room)
                                           (leman-room-id room))
                ;; Track buffer in room's local slot.
                (map-elt (leman-room-local room) 'buffer) (current-buffer))
          (add-hook 'kill-buffer-hook
                    (lambda ()
                      (setf (map-elt (leman-room-local room) 'buffer) nil))
                    nil 'local)
          (setq-local bookmark-make-record-function #'leman-room-bookmark-make-record)
          ;; Set initial header and footer.  (Do this before processing events, which
          ;; might cause the header/footer to be changed (e.g. a tombstone event).
          (ewoc-set-hf leman-ewoc (leman-room--initial-header room)
                                   (leman-room--initial-footer room))
          ;; Clear new-events, because those only matter when a buffer is already open.
          (setf (alist-get 'new-events (leman-room-local room)) nil)
          ;; We don't use `leman-room--insert-events' to avoid extra
          ;; calls to `leman-room--insert-ts-headers'.
          ;; NOTE: We handle the events in chronological order (i.e. the reverse of the
          ;; stored order, which is latest-first), because some logic depends on this
          ;; (e.g. processing a message-edit event before the edited event would mean the
          ;; edited event would not yet be in the buffer).
          (setf (leman-room-state room) (leman-room--sanitize-events (leman-room-state room))
                (leman-room-timeline room) (leman-room--sanitize-events (leman-room-timeline room)))
          (leman-room--process-events (reverse (leman-room-state room)))
          (leman-room--process-events (reverse (leman-room-timeline room)))
          (leman-room--insert-ts-headers)
          (when leman-room-sender-in-headers
            (leman-room--insert-sender-headers leman-ewoc))
          (leman-room-move-read-markers room
            :read-event (when-let ((event (alist-get "m.read" (leman-room-account-data room) nil nil #'equal)))
                          (map-nested-elt event '(content event_id)))
            :fully-read-event (when-let ((event (alist-get "m.fully_read" (leman-room-account-data room) nil nil #'equal)))
                                (map-nested-elt event '(content event_id)))))
        ;; Return the buffer!
        new-buffer)))

(defun leman-room--event-data (id)
  "Return event struct for event ID in current buffer."
  ;; Search from bottom, most likely to be faster.
  (cl-loop with node = (ewoc-nth leman-ewoc -1)
           while node
           for data = (ewoc-data node)
           when (and (leman-event-p data)
                     (equal id (leman-event-id data)))
           return data
           do (setf node (ewoc-prev leman-ewoc node))))

(defun leman-room--escape-% (string)
  "Return STRING with \"%\" escaped.
Needed to display things in the header line."
  (replace-regexp-in-string (rx "%") "%%" string t t))

(defun leman-room--buffer-substring-filter (beg end &optional delete)
  "Value for `filter-buffer-substring-function' in Leman rooms.

Strips the `line-prefix' and `wrap-prefix' text properties which
are used when formatting certain Matrix events, but which should
not be copied into other buffers."
  (let ((string (funcall (default-value 'filter-buffer-substring-function)
                         beg end delete)))
    (remove-list-of-text-properties
     0 (length string) '(line-prefix wrap-prefix) string)
    string))

;;;;; Imenu

(defconst leman-room-timestamp-header-imenu-format "%Y-%m-%d (%A) %H:%M"
  "Format string for timestamps in Imenu indexes.")

(defun leman-room--imenu-create-index-function ()
  "Return Imenu index for the current buffer.
For use as `imenu-create-index-function'."
  (let ((timestamp-nodes (leman-room--ewoc-collect-nodes
                          leman-ewoc (lambda (node)
                                       (pcase (ewoc-data node)
                                         (`(ts . ,_) t))))))
    (cl-loop for node in timestamp-nodes
             collect (pcase-let*
                         ((`(ts ,timestamp) (ewoc-data node))
                          (formatted (format-time-string leman-room-timestamp-header-imenu-format timestamp)))
                       (cons formatted (ewoc-location node))))))

;;;;; Occur

(defvar-local leman-room-occur-pred nil
  "Predicate used to refresh `leman-room-occur' buffers.")

(define-derived-mode leman-room-occur-mode leman-room-mode "Leman-Room-Occur")

(progn
  (define-key leman-room-occur-mode-map [remap leman-room-send-message]  #'leman-room-occur-find-event)
  (define-key leman-room-occur-mode-map (kbd "g") #'revert-buffer)
  (define-key leman-room-occur-mode-map (kbd "n") #'leman-room-occur-next)
  (define-key leman-room-occur-mode-map (kbd "p") #'leman-room-occur-prev))

(cl-defun leman-room-occur (&key user-id regexp pred header)
  "Show known events in current buffer matching args in a new buffer.
If REGEXP, show events whose sender or body content match it.  Or
if USER-ID, show events from that user.  Or if PRED, show events
matching it.  HEADER is used if given, or set according to other
arguments."
  (interactive (let* ((regexp (read-regexp "Regexp (leave empty to select user instead)"))
                      (user-id (when (string-empty-p regexp)
                                 (leman-complete-user-id))))
                 (list :regexp regexp :user-id user-id)))
  (let* ((session leman-session)
         (room leman-room)
         (occur-buffer (get-buffer-create (format "*Leman Room Occur: %s*" (leman-room-display-name room))))
         (pred (cond (pred)
                     ((not (string-empty-p regexp))
                      (lambda (data)
                        (and (leman-event-p data)
                             (or (string-match regexp (leman-user-id (leman-event-sender data)))
                                 (when-let ((room-display-name
                                             (gethash (leman-event-sender data) (leman-room-displaynames room))))
                                   (string-match regexp room-display-name))
                                 (when-let ((body (alist-get 'body (leman-event-content data))))
                                   (string-match regexp body))))))
                     (user-id
                      (lambda (data)
                        (and (leman-event-p data)
                             (equal user-id (leman-user-id (leman-event-sender data))))))))
         (header (cond (header)
                       ((not (string-empty-p regexp))
                        (format "Events matching %S in %s" regexp (leman-room-display-name room)))
                       (user-id
                        (format "Events from %s in %s" user-id (leman-room-display-name room))))))
    (with-current-buffer occur-buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (leman-room-occur-mode)
      (setf header-line-format header
            leman-session session
            leman-room room)
      (setq-local revert-buffer-function (lambda (&rest _)
                                           (interactive)
                                           (let ((event-at-point (ewoc-data (ewoc-locate leman-ewoc))))
                                             (with-current-buffer (alist-get 'buffer (leman-room-local room))
                                               (leman-room-occur :pred pred :header header)
                                               (when-let ((node (leman-room--ewoc-last-matching leman-ewoc
                                                                  (lambda (data)
                                                                    (eq event-at-point data)))))
                                                 (ewoc-goto-node leman-ewoc node))))))
      (setf (leman-room-state room) (leman-room--sanitize-events (leman-room-state room))
            (leman-room-timeline room) (leman-room--sanitize-events (leman-room-timeline room)))
      (leman-room--process-events (reverse (leman-room-state room)))
      (leman-room--process-events (reverse (leman-room-timeline room)))
      (ewoc-filter leman-ewoc pred)
      ;; TODO: Insert date header before first event.
      (leman-room--insert-ts-headers))
    (pop-to-buffer occur-buffer)))

(defun leman-room-occur-find-event (event)
  "Find EVENT in room's main buffer."
  (interactive (list (ewoc-data (ewoc-locate leman-ewoc))))
  (pcase-let* (((cl-struct leman-room (local (map buffer))) leman-room)
               ((cl-struct leman-event id) event))
    (display-buffer buffer)
    (with-selected-window (get-buffer-window buffer)
      (leman-room-find-event id))))

(cl-defun leman-room-occur-next (&optional (n 1))
  "Go to Nth next event."
  (interactive)
  (let ((command (if (> n 0)
                     #'leman-room-goto-next
                   #'leman-room-goto-prev)))
    (cl-loop for i below (abs n)
             do (call-interactively command))
    (leman-room-occur-find-event (ewoc-data (ewoc-locate leman-ewoc)))))

(cl-defun leman-room-occur-prev (&optional (n 1))
  "Go to Nth previous event."
  (interactive)
  (leman-room-occur-next (- n)))

;;;;; Events

;; Functions to handle types of events.

;; NOTE: At the moment, this only handles "m.typing" ephemeral events.  Message
;; events are handled elsewhere.  A better framework should be designed...
;; TODO: Define other handlers this way.

;; MAYBE: Should we intern these functions?  That means every event
;; handled has to concat and intern.  Should we use lambdas in an
;; alist or hash-table instead?  For now let's use an alist.

(defvar leman-users)

(defvar leman-room-event-fns nil
  "Alist mapping event types to functions which process events in room buffers.")

;; NOTE: While transitioning to the defevent-based handler system, we
;; define both a handle-events and handle-event function that do the
;; same thing.

;; TODO: Tidy this up.

;; NOTE: --handle-events and --handle-event need to be called in the room
;; buffer's window, when it has one.  This is absolutely necessary,
;; otherwise the events may be inserted at the wrong place.  (I'm not
;; sure if this is a bug in EWOC or in my code, but doing this fixes it.)

(defun leman-room--sanitize-events (events)
  "Return EVENTS with any raw events converted to structs.
Raw (unconverted) events may end up in rooms' event slots, e.g.
from sync responses with unexpected formats; nils, e.g. from
`leman-room--process-retro-chunk'.  Since event slots are
iterable by many functions which expect structs (e.g.
`leman--user-displayname-in'), convert or discard such entries."
  (declare (function leman--make-event "leman"))
  (cl-loop for event in events
           when event
           collect (if (leman-event-p event)
                       event
                     (leman--make-event event))))

(defun leman-room--process-events (events)
  "Process EVENTS in current buffer.
Calls `leman-progress-update' for each event.  Calls
`leman-room--insert-ts-headers' when done.  Uses handlers defined
in `leman-room-event-fns'.  The current buffer should be a room's
buffer."
  ;; FIXME: Calling `leman-room--insert-ts-headers' is convenient, but it
  ;; may also be called in functions that call this function, which may
  ;; result in it being called multiple times for a single set of events.
  (cl-loop for entry being the elements of events ;; EVENTS may be a list or array.
           ;; NOTE: Raw events may be present (e.g. from unconverted sync
           ;; responses or, for already-seen events, nils, e.g. from
           ;; `leman-room--process-retro-chunk'); convert or discard them.
           for event = (if (leman-event-p entry)
                           entry
                         (when-let* ((raw entry)
                                     ((alist-get 'event_id raw)))
                           (leman--make-event raw)))
           for handler = (when event
                           (alist-get (leman-event-type event) leman-room-event-fns nil nil #'equal))
           when handler
           ;; A malformed event must not abort processing of the
           ;; remaining events in the batch.
           do (with-demoted-errors "Leman: Error processing event: %S"
                (funcall handler event))
           do (leman-progress-update))
  (leman-room--insert-ts-headers))

(defun leman-room--process-event (event)
  "Process EVENT in current buffer.
Uses handlers defined in `leman-room-event-fns'.  The current
buffer should be a room's buffer."
  (when (and event (not (leman-event-p event)))
    ;; Raw event: convert it (see `leman-room--process-events').
    (setf event (leman--make-event event)))
  (when-let ((handler (when event
                        (alist-get (leman-event-type event) leman-room-event-fns nil nil #'equal))))
    ;; We demote any errors that happen while processing events, because it's possible for
    ;; events to be malformed in unexpected ways, and that could cause an error, which
    ;; would stop processing of other events and prevent further syncing.
    (with-demoted-errors "Leman (leman-room--process-event): Error processing event: %S"
      (funcall handler event))))

;;;;;; Event handlers

(defmacro leman-room-defevent (type &rest body)
  "Define an event handling function for events of TYPE.
Around the BODY, the variable `event' is bound to the event being
processed.  The function is called in the room's buffer.  Adds
function to `leman-room-event-fns', which see."
  (declare (debug (stringp def-body))
           (indent defun))
  `(setf (alist-get ,type leman-room-event-fns nil nil #'string=)
         (lambda (event)
           ,(concat "`leman-room' handler function for " type " events.")
           ,@body)))

(leman-room-defevent "m.reaction"
  (pcase-let* (((cl-struct leman-event content) event)
               ((map ('m.relates_to relates-to)) content)
               ((map ('event_id related-id) ('rel_type rel-type) _key) relates-to))
    ;; TODO: Handle other rel_types?
    (pcase rel-type
      ("m.annotation"
       ;; Look for related event in timeline.
       (if-let ((related-event (cl-loop with fake-event = (make-leman-event :id related-id)
                                        for timeline-event in (leman-room-timeline leman-room)
                                        when (leman--events-equal-p fake-event timeline-event)
                                        return timeline-event)))
           ;; Found related event: add reaction to local slot and invalidate node.
           (progn
             ;; Every time a room buffer is made, these reaction events are processed again, so we use pushnew to
             ;; avoid duplicates.  (In the future, as event-processing is refactored, this may not be necessary.)
             (cl-pushnew event (map-elt (leman-event-local related-event) 'reactions))
             ;; The node may hold the related event itself or an
             ;; edit of it (edits replace the original in the
             ;; buffer), so match either.
             (when-let ((nodes (leman-room--ewoc-last-matching leman-ewoc
                                 (lambda (data)
                                   (and (leman-event-p data)
                                        (or (equal related-id (leman-event-id data))
                                            (equal related-id
                                                   (map-nested-elt (leman-event-content data)
                                                                   '(m.relates_to event_id)))))))))
               (ewoc-invalidate leman-ewoc nodes))))
         ;; No known related event in the timeline: maybe it's a thread
         ;; reply (thread replies are not shown in the timeline).
         (if-let ((thread-event (leman-room--thread-event-for-id related-id leman-room)))
             (cl-pushnew event (map-elt (leman-event-local thread-event) 'reactions)
                         :key #'leman-event-id :test #'equal)
           (leman-debug "No known related event for" event))))))

(leman-room-defevent "m.room.power_levels"
  (leman-room--insert-event event))

(defun leman-room--format-power-levels-event (event room _session)
  "Return power-levels EVENT in ROOM formatted as a string."
  (pcase-let (((cl-struct leman-event sender
                          (content (map ('users new-users)))
                          (unsigned (map ('prev_content (map ('users old-users))))))
               event))
    (when old-users
      (pcase-let* ((sender-id (leman-user-id sender))
                   (sender-displayname (leman--user-displayname-in room sender))
                   (`(,changed-user-id-symbol . ,new-level)
                    (cl-find-if (lambda (new-user)
                                  (let ((old-user (cl-find (car new-user) old-users
                                                           :key #'car)))
                                    (or (not old-user)
                                        (not (equal (cdr new-user) (cdr old-user))))))
                                new-users))
                   (changed-user-id (symbol-name changed-user-id-symbol))
                   (changed-user (when changed-user-id-symbol
                                   (gethash changed-user-id leman-users)))
                   (user-displayname (if changed-user
                                         (leman--user-displayname-in room changed-user)
                                       changed-user-id)))
        (leman-room-wrap-prefix
          (if (not changed-user)
              (format "%s sent a power-level event"
                      (propertize sender-displayname
                                  'help-echo sender-id))
            (format "%s set %s's power level to %s"
                    (propertize sender-displayname
                                'help-echo sender-id)
                    (propertize user-displayname 'help-echo changed-user-id)
                    new-level))
          'face 'leman-room-membership)))))

(leman-room-defevent "m.room.canonical_alias"
  (leman-room--insert-event event))

(defun leman-room--format-canonical-alias-event (event room _session)
  "Return canonical alias EVENT in ROOM formatted as a string."
  (pcase-let (((cl-struct leman-event sender
                          ;; TODO: Include alt_aliases, maybe.
                          ;; TODO: Include old alias when it is being replaced.
                          (content (map alias)))
               event))
    (leman-room-wrap-prefix
      (format "%s set the canonical alias to <%s>"
              (propertize (leman--user-displayname-in room sender)
                          'help-echo (leman-user-id sender))
              alias)
      'face 'leman-room-membership)))

(leman-room-defevent "m.room.redaction"
  ;; We handle redaction events here rather than an `leman-defevent' handler.  This way we
  ;; do less work for events in rooms that the user isn't looking at, at the cost of doing
  ;; a bit more work when a room's buffer is prepared.
  (pcase-let* (((cl-struct leman-event (local (map ('redacts redacted-id)))) event)
               ((cl-struct leman-room timeline) leman-room)
               (redacted-event (cl-find redacted-id timeline
                                        :key #'leman-event-id :test #'equal))
               (redacted-edit-events (cl-remove-if-not (lambda (timeline-event)
                                                         (pcase-let (((cl-struct leman-event
                                                                                 (content
                                                                                  (map ('m.relates_to
                                                                                        (map ('event_id related-id)
                                                                                             ('rel_type rel-type))))))
                                                                      timeline-event))
                                                           (and (equal redacted-id related-id)
                                                                (equal "m.replace" rel-type))))
                                                       timeline)))
    (leman-debug event redacted-event redacted-edit-events)
    (cl-loop for edit-event in redacted-edit-events
             do (cl-pushnew event (alist-get 'redacted-by (leman-event-local edit-event))))
    (when redacted-event
      (cl-pushnew event (alist-get 'redacted-by (leman-event-local redacted-event)))
      (pcase-let* (((cl-struct leman-event (content
                                            (map ('m.relates_to
                                                  (map ('event_id related-id)
                                                       ('rel_type rel-type))))))
                    redacted-event))
        (pcase rel-type
          ("m.annotation"
           ;; Redacted annotation/reaction.  NOTE: Since we link annotations in a -room
           ;; event handler (rather than in a non-room handler), we also unlink redacted
           ;; ones here.
           (when-let (annotated-event (cl-find related-id timeline
                                               :key #'leman-event-id :test #'equal))
             ;; Remove it from the related event's local slot.
             (setf (map-elt (leman-event-local annotated-event) 'reactions)
                   (cl-remove redacted-id (map-elt (leman-event-local annotated-event) 'reactions)
                              :key #'leman-event-id :test #'equal))
             ;; Invalidate the related event's node.
             (when-let (node (leman-room--ewoc-last-matching leman-ewoc
                               (lambda (data)
                                 (and (leman-event-p data)
                                      (equal related-id (leman-event-id data))))))
               (ewoc-invalidate leman-ewoc node)))))))
    ;; Invalidate the redacted event's node.
    (when-let ((node (leman-room--ewoc-last-matching leman-ewoc
                       (lambda (data)
                         (and (leman-event-p data)
                              (pcase-let (((cl-struct leman-event id
                                                      (content
                                                       (map ('m.relates_to
                                                             (map ('event_id related-id)
                                                                  ('rel_type rel-type))))))
                                           data))
                                (or (equal redacted-id id)
                                    (and (equal "m.replace" rel-type)
                                         (equal redacted-id related-id)))))))))
      (leman-debug node)
      (ewoc-invalidate leman-ewoc node))))

(leman-room-defevent "m.typing"
  (pcase-let* (((cl-struct leman-session user) leman-session)
               ((cl-struct leman-user (id local-user-id)) user)
               ((cl-struct leman-event content) event)
               ((map ('user_ids user-ids)) content)
               (usernames) (footer))
    (setf user-ids (delete local-user-id user-ids))
    (if (zerop (length user-ids))
        (setf footer "")
      (setf usernames (cl-loop for id across user-ids
                               for user = (gethash id leman-users)
                               if user
                               collect (leman--user-displayname-in leman-room user)
                               else collect id)
            footer (propertize (concat "Typing: " (string-join usernames ", "))
                               'face 'font-lock-comment-face)))
    (with-silent-modifications
      (ewoc-set-hf leman-ewoc "" footer))))

(leman-room-defevent "m.room.avatar"
  (leman-room--insert-event event))

(leman-room-defevent "org.matrix.msc3015.m.room.name.override"
  (ignore event)
  (setf (leman-room-display-name leman-room) (leman--room-display-name leman-room))
  (rename-buffer (leman-room--buffer-name leman-room)))

(leman-room-defevent "m.room.member"
  (with-silent-modifications
    (leman-room--insert-event event)))

(leman-room-defevent "m.room.encrypted"
  (leman-room--insert-event event))

(leman-room-defevent "m.room.message"
  (pcase-let* (((cl-struct leman-event content unsigned) event)
               ((map ('m.relates_to (map ('rel_type rel-type) ('event_id replaces-event-id)))) content)
               ((map ('m.relations (map ('m.replace (map ('event_id replaced-by-id)))))) unsigned))
    (cond ((and (equal "m.replace" rel-type) (leman-room--replace-thread-event event leman-room))
           ;; Event edits a thread reply: applied to the thread data; the reply is not shown in the main timeline.
           nil)
          ((equal "m.thread" rel-type)
           ;; Thread reply: aggregate into the thread's data; replies are not shown in the main timeline.
           (pcase-let* ((root-id (leman-room--add-thread-event event leman-room)))
             (when-let ((nodes (leman-room--ewoc-last-matching leman-ewoc
                                 (lambda (data)
                                   (and (leman-event-p data)
                                        (equal root-id (leman-event-id data)))))))
               (ewoc-invalidate leman-ewoc nodes))
             ;; Refresh an open thread view for this thread.
             (when-let* ((thread-buffer (leman-room--thread-buffer root-id leman-room))
                         (root-event (gethash root-id (leman-session-events leman-session))))
               (leman-room--render-thread thread-buffer root-event))))
          ((and leman-room-replace-edited-messages
                replaces-event-id (equal "m.replace" rel-type))
           ;; Event replaces existing event: find and replace it in buffer if possible, otherwise insert it.
           (or (leman-room--replace-event event)
               (progn
                 (leman-debug "Unable to replace event ID: inserting instead." replaces-event-id)
                 (leman-room--insert-event event))))
          ;; New event.
          (replaced-by-id
           (leman-debug "Event replaced: not inserting." replaced-by-id))
          ;; Not replaced: insert it.
          (t (leman-room--insert-event event)))))

(leman-room-defevent "m.room.tombstone"
  (pcase-let* (((cl-struct leman-event content) event)
               ((map body ('replacement_room new-room-id)) content)
               (session leman-session)
               (button (leman--button-buttonize
                        (propertize new-room-id 'help-echo "Join replacement room")
                        (lambda (_)
                          (leman-room-join new-room-id session))))
               (banner (format "This room has been replaced.  Explanation:%S  Replacement room: <%s>" body button)))
    (add-face-text-property 0 (length banner) 'font-lock-warning-face t banner)
    ;; NOTE: We assume that no more typing events will be received,
    ;; which would replace the footer.
    (leman-room--insert-event event)
    (ewoc-set-hf leman-ewoc banner banner)))

;;;;; Read markers

;; Marking rooms as read and showing lines where marks are.

(leman-room-defevent "m.read"
  (leman-room-move-read-markers leman-room
    :read-event (leman-event-id event)))

(leman-room-defevent "m.fully_read"
  (leman-room-move-read-markers leman-room
    :fully-read-event (leman-event-id event)))

(defvar-local leman-room-read-receipt-marker nil
  "EWOC node for the room's read-receipt marker.")

(defvar-local leman-room-fully-read-marker nil
  "EWOC node for the room's fully-read marker.")

(defface leman-room-read-receipt-marker
  '((t (:inherit show-paren-match)))
  "Read marker line in rooms."
  :group 'leman-room-faces)

(defface leman-room-fully-read-marker
  '((t (:inherit isearch)))
  "Fully read marker line in rooms."
  :group 'leman-room-faces)

(defcustom leman-room-send-read-receipts t
  "Whether to send read receipts.
Also controls whether the read-receipt marker in a room is moved
automatically."
  :type 'boolean
  :group 'leman-room)

(defun leman-room-read-receipt-idle-timer ()
  "Update read receipts in visible Leman room buffers.
To be called from timer stored in
`leman-read-receipt-idle-timer'."
  (when leman-room-send-read-receipts
    (dolist (window (window-list))
      (when (and (eq 'leman-room-mode (buffer-local-value 'major-mode (window-buffer window)))
                 (buffer-local-value 'leman-room (window-buffer window))
                 (not (leman-session-revoked-p
                       (buffer-local-value 'leman-session (window-buffer window)))))
        (leman-room-update-read-receipt window)))))

(defun leman-room-update-read-receipt (window)
  "Update read receipt for room displayed in WINDOW.
Also, mark room's buffer as unmodified."
  (with-selected-window window
    (let ((read-receipt-node (leman-room--ewoc-last-matching leman-ewoc
                               (lambda (node-data)
                                 (eq 'leman-room-read-receipt-marker node-data))))
          (window-end-node (or (ewoc-locate leman-ewoc (window-end nil t))
                               (ewoc-nth leman-ewoc -1))))
      (when (or
             ;; The window's end has been scrolled to or past the position of the
             ;; receipt marker.
             (and read-receipt-node
                  (>= (window-end nil t) (ewoc-location read-receipt-node)))
             ;; The read receipt is outside of retrieved events.
             (not read-receipt-node))
        (let* ((event-node (when window-end-node
                             ;; It seems like `window-end-node' shouldn't ever be nil,
                             ;; but just in case...
                             (cl-typecase (ewoc-data window-end-node)
                               (leman-event window-end-node)
                               (t (leman-room--ewoc-next-matching leman-ewoc window-end-node
                                    #'leman-event-p #'ewoc-prev)))))
               (node-after-event (ewoc-next leman-ewoc event-node))
               (event))
          (when event-node
            (unless (or (when node-after-event
                          (<= (ewoc-location node-after-event) (window-end nil t)))
                        (>= (window-end) (point-max)))
              ;; The entire event is not visible: use the previous event.  (NOTE: This
              ;; isn't quite perfect, because apparently `window-end' considers a position
              ;; visible if even one pixel of its line is visible.  This will have to be
              ;; good enough for now.)
              ;; FIXME: Workaround that an entire line's height need not be displayed for it to be considered so.
              (setf event-node (leman-room--ewoc-next-matching leman-ewoc event-node
                                 #'leman-event-p #'ewoc-prev)))
            (setf event (ewoc-data event-node))
            ;; Mark the buffer as not modified so that will not contribute to its being
            ;; considered unread.  NOTE: This will mean that any room buffer displayed in
            ;; a window will have its buffer marked unmodified when this function is
            ;; called.  This is probably for the best.
            (set-buffer-modified-p nil)
            (unless (alist-get event leman-room-read-receipt-request)
              ;; No existing request for this event: cancel any outstanding request and
              ;; send a new one.
              (when-let ((request-process (car (map-values leman-room-read-receipt-request))))
                (when (process-live-p request-process)
                  (interrupt-process request-process)))
              (setf leman-room-read-receipt-request nil)
              (setf (alist-get event leman-room-read-receipt-request)
                    (leman-room-mark-read leman-room leman-session
                      :read-event event)))))))))

(defun leman-room-goto-fully-read-marker ()
  "Move to the fully-read marker in the current room."
  (interactive)
  (if-let ((fully-read-pos (when leman-room-fully-read-marker
                             (ewoc-location leman-room-fully-read-marker))))
      (with-suppressed-warnings ((obsolete point))
        ;; I like using `point' as a GV, and I object to its being obsoleted (and said so
        ;; on emacs-devel).
        (setf (point) fully-read-pos (window-start) fully-read-pos))
    ;; Unlike the fully-read marker, there doesn't seem to be a
    ;; simple way to get the user's read-receipt marker.  So if
    ;; we haven't seen either marker in the retrieved events, we
    ;; go back to the fully-read marker.
    (if-let* ((fully-read-event (alist-get "m.fully_read" (leman-room-account-data leman-room) nil nil #'equal))
              (fully-read-event-id (map-nested-elt fully-read-event '(content event_id))))
        ;; Fully-read account-data event is known.
        (if (gethash fully-read-event-id (leman-session-events leman-session))
            ;; The fully-read event (i.e. the message event that was read, not the
            ;; account-data event) is already retrieved, but the marker is not present in
            ;; the buffer (this shouldn't happen, but somehow, it can): Reset the marker,
            ;; which should work around the problem.
            (leman-room-mark-read leman-room leman-session
              :fully-read-event (gethash fully-read-event-id (leman-session-events leman-session)))
          ;; Fully-read event not retrieved: search for it in room history.
          (let ((buffer (current-buffer)))
            (message "Searching for first unread event...")
            (leman-room-retro-to leman-room leman-session fully-read-event-id
              :then (lambda ()
                      (with-current-buffer buffer
                        ;; HACK: Should probably call this function elsewhere, in a hook or something.
                        (leman-room-move-read-markers leman-room)
                        (leman-room-goto-fully-read-marker))))))
      (error "Room has no fully-read event"))))

(cl-defun leman-room-mark-read (room session &key read-event fully-read-event)
  "Mark ROOM on SESSION as read on the server.
Set \"m.read\" to READ-EVENT and \"m.fully_read\" to
FULLY-READ-EVENT.  Return the API request.

Interactively, mark both types as read up to event at point."
  (declare (indent defun))
  (interactive
   (progn
     (cl-assert (equal 'leman-room-mode major-mode) nil
                "This command is to be used in `leman-room-mode' buffers")
     (let* ((node (ewoc-locate leman-ewoc))
            (event-at-point (cl-typecase (ewoc-data node)
                              (leman-event (ewoc-data node))
                              (t (when-let ((prev-event-node (leman-room--ewoc-next-matching leman-ewoc node
                                                               #'leman-event-p #'ewoc-prev)))
                                   (ewoc-data prev-event-node)))))
            (last-event (ewoc-data (leman-room--ewoc-last-matching leman-ewoc #'leman-event-p)))
            (event-to-mark-read (if (eq event-at-point last-event)
                                    ;; The node is at the end of the buffer: use the last event in the timeline
                                    ;; instead of the last node in the EWOC, because the last event in the timeline
                                    ;; might not be the last event in the EWOC (e.g. a reaction to an earlier event).
                                    (car (leman-room-timeline leman-room))
                                  event-at-point)))
       (list leman-room leman-session
             :read-event event-to-mark-read
             :fully-read-event event-to-mark-read))))
  (cl-assert room) (cl-assert session) (cl-assert (or read-event fully-read-event))
  (if (not fully-read-event)
      ;; Sending only a read receipt, which uses a different endpoint
      ;; than when setting the fully-read marker or both.
      (leman-room-send-receipt room session read-event)
    ;; Setting the fully-read marker, and maybe the "m.read" one too.
    (pcase-let* (((cl-struct leman-room (id room-id)) room)
                 (endpoint (format "rooms/%s/read_markers" (url-hexify-string room-id)))
                 (data (leman-alist "m.fully_read" (leman-event-id fully-read-event))))
      (when read-event
        (push (cons "m.read" (leman-event-id read-event)) data))
      ;; NOTE: See similar code in `leman-room-update-read-receipt'.
      (let ((request-process (leman-api session endpoint :method 'post :data (json-encode data)
                               :then (lambda (_data)
                                       (leman-room-move-read-markers room
                                         :read-event read-event :fully-read-event fully-read-event)
                                       (leman--update-unread-indicator))
                               :else (lambda (plz-error)
                                       (pcase (plz-error-message plz-error)
                                         ("curl process interrupted"
                                          ;; Ignore this, because it happens when we
                                          ;; update a read marker before the previous
                                          ;; update request is completed.
                                          nil)
                                         (_ (signal 'leman-api-error
                                                    (list (format "Leman: (leman-room-mark-read) Unexpected API error: %s"
                                                                  plz-error)
                                                          plz-error))))))))
        (when-let ((room-buffer (alist-get 'buffer (leman-room-local room))))
          ;; NOTE: Ideally we would do this before sending the new request, but to make
          ;; the code much simpler, we do it afterward.
          (with-current-buffer room-buffer
            (when-let ((request-process (car (map-values leman-room-read-receipt-request))))
              (when (process-live-p request-process)
                (interrupt-process request-process)))
            (setf leman-room-read-receipt-request nil
                  (alist-get read-event leman-room-read-receipt-request) request-process)))))))

(cl-defun leman-room-send-receipt (room session event &key (type "m.read"))
  "Send receipt of TYPE for EVENT to ROOM on SESSION."
  (pcase-let* (((cl-struct leman-room (id room-id)) room)
               ((cl-struct leman-event (id event-id)) event)
               (endpoint (format "rooms/%s/receipt/%s/%s"
                                 (url-hexify-string room-id) type
                                 (url-hexify-string event-id))))
    (leman-api session endpoint :method 'post :data "{}"
      :then (pcase type
              ("m.read" (lambda (_data)
                          (leman-room-move-read-markers room
                            :read-event event)))
              ;; No other type is yet specified.
              (_ #'ignore)))))

(cl-defun leman-room-move-read-markers
    (room &key
          (read-event (when-let ((event (alist-get "m.read" (leman-room-account-data room) nil nil #'equal)))
                        (map-nested-elt event '(content event_id))))
          (fully-read-event (when-let ((event (alist-get "m.fully_read" (leman-room-account-data room) nil nil #'equal)))
                              (map-nested-elt event '(content event_id)))))
  "Move read markers in ROOM to READ-EVENT and FULLY-READ-EVENT.
Each event may be an `leman-event' struct or an event ID.  This
updates the markers in ROOM's buffer, not on the server; see
`leman-room-mark-read' for that."
  (declare (indent defun))
  (cl-labels ((update-marker (symbol to-event)
                (let* ((old-node (symbol-value symbol))
                       (new-event-id (cl-etypecase to-event
                                       (leman-event (leman-event-id to-event))
                                       (string to-event)))
                       ;; FIXME: Some events, like reactions, are not inserted into the
                       ;; EWOC directly, and if a read marker refers to such an event, the
                       ;; place for the read marker will not be found.
                       (event-node (leman-room--ewoc-last-matching leman-ewoc
                                     (lambda (data)
                                       (and (leman-event-p data)
                                            (equal (leman-event-id data) new-event-id)))))
                       (inhibit-read-only t))
                  (with-silent-modifications
                    (when old-node
                      (ewoc-delete leman-ewoc old-node))
                    (set symbol (when event-node
                                  ;; If the event hasn't been inserted into the buffer yet,
                                  ;; this might be nil.  That shouldn't happen, but...
                                  (ewoc-enter-after leman-ewoc event-node symbol)))))))
    (when-let ((buffer (alist-get 'buffer (leman-room-local room))))
      ;; MAYBE: Error if no buffer?  Or does it matter?
      (with-current-buffer buffer
        (when read-event
          (update-marker 'leman-room-read-receipt-marker read-event))
        (when fully-read-event
          (update-marker 'leman-room-fully-read-marker fully-read-event))))
    ;; NOTE: Return nil so that, in the event this function is called manually with `eval-expression',
    ;; it does not cause an error due to the return value being an EWOC node, which is a structure too
    ;; big and/or circular to print.  (This was one of those bugs that only happens WHEN debugging.)
    nil))

(defun leman-room-scroll-up-mark-read ()
  "Scroll buffer contents up, move fully read marker, and bury when at end.
Moves fully read marker to the top of the window (when the
marker's position is within the range of received events).  At
end-of-buffer, moves fully read marker to after the last event,
buries the buffer and shows the next unread room, if any."
  (declare (function leman-tabulated-room-list-next-unread "leman-tabulated-room-list")
           (function leman-room-list-next-unread "leman-room-list"))
  (interactive)
  (if (= (window-point) (point-max))
      (progn
        ;; At the bottom of the buffer: mark read and show next unread room.
        (when leman-room-mark-rooms-read
          (leman-room-mark-read leman-room leman-session
            :read-event (ewoc-data (leman-room--ewoc-last-matching leman-ewoc
                                     (lambda (data) (leman-event-p data))))
            :fully-read-event (ewoc-data (leman-room--ewoc-last-matching leman-ewoc
                                           (lambda (data) (leman-event-p data))))))
        (set-buffer-modified-p nil)
        (if-let ((rooms-window (cl-find-if (lambda (window)
                                             (member (buffer-name (window-buffer window))
                                                     '("*Leman Taxy*" "*Leman Rooms*")))
                                           (window-list))))
            ;; Rooms buffer already displayed: select its window and move to next unread room.
            (progn
              (select-window rooms-window)
              (funcall (pcase-exhaustive major-mode
                         ('leman-tabulated-room-list-mode #'leman-tabulated-room-list-next-unread)
                         ('leman-room-list-mode #'leman-room-list-next-unread))))
          ;; Rooms buffer not displayed: bury this room buffer, which should usually
          ;; result in another room buffer or the rooms list buffer being displayed.
          (bury-buffer))
        (when (member major-mode '(leman-tabulated-room-list-mode leman-room-list-mode))
          ;; Back in the room-list buffer: revert it.
          (revert-buffer)))
    ;; Not at the bottom of the buffer: scroll.
    (condition-case _err
        (scroll-up-command)
      (end-of-buffer (set-window-point nil (point-max))))
    (when-let* ((node (ewoc-locate leman-ewoc (window-start)))
                (event-node (leman-room--ewoc-next-matching leman-ewoc node
                              #'leman-event-p #'ewoc-prev))
                (fully-read-pos (and leman-room-fully-read-marker
                                     (ewoc-location leman-room-fully-read-marker)))
                ((< fully-read-pos (ewoc-location event-node))))
      ;; Move fully-read marker to top of window.
      (leman-room-mark-read leman-room leman-session :fully-read-event (ewoc-data event-node)))))

;;;;; EWOC

(cl-defun leman-room--ewoc-next-matching (ewoc node pred &optional (move-fn #'ewoc-next))
  "Return the next node in EWOC after NODE that PRED is true of.
PRED is called with node's data.  Moves to next node by MOVE-FN."
  (declare (indent defun))
  (cl-loop do (setf node (funcall move-fn ewoc node))
           until (or (null node)
                     (funcall pred (ewoc-data node)))
           finally return node))

(defun leman-room--ewoc-last-matching (ewoc predicate)
  "Return the last node in EWOC matching PREDICATE.
PREDICATE is called with node's data.  Searches backward from
last node."
  (declare (indent defun))
  ;; Intended to be like `ewoc-collect', but returning as soon as a match is found.
  (cl-loop with node = (ewoc-nth ewoc -1)
           while node
           when (funcall predicate (ewoc-data node))
           return node
           do (setf node (ewoc-prev ewoc node))))

(defun leman-room--ewoc-collect-nodes (ewoc predicate)
  "Collect all nodes in EWOC matching PREDICATE.
PREDICATE is called with the full node."
  ;; Intended to be like `ewoc-collect', but working with the full node instead of just the node's data.
  (cl-loop with node = (ewoc-nth ewoc 0)
           do (setf node (ewoc-next ewoc node))
           while node
           when (funcall predicate node)
           collect node))

(defun leman-room--insert-ts-headers (&optional start-node end-node)
  "Insert timestamp headers into current buffer's `leman-ewoc'.
Inserts headers between START-NODE and END-NODE, which default to
the first and last nodes in the buffer, respectively."
  (let* ((type-predicate (lambda (node-data)
                           (and (leman-event-p node-data)
                                (not (equal "m.room.member" (leman-event-type node-data))))))
         (ewoc leman-ewoc)
         (end-node (or end-node
                       (ewoc-nth ewoc -1)))
         (end-pos (if end-node
                      (ewoc-location end-node)
                    ;; HACK: Trying to work around a bug in case the
                    ;; room doesn't seem to have any events yet.
                    (point-max)))
         (node-b (or start-node (ewoc-nth ewoc 0)))
         node-a)
    ;; On the first loop iteration, node-a is set to the first matching
    ;; node after node-b; then it's set to the first node after node-a.
    (while (and (setf node-a (leman-room--ewoc-next-matching ewoc (or node-a node-b) type-predicate)
                      node-b (when node-a
                               (leman-room--ewoc-next-matching ewoc node-a type-predicate)))
                (not (or (> (ewoc-location node-a) end-pos)
                         (when node-b
                           (> (ewoc-location node-b) end-pos)))))
      (cl-labels ((format-event (event)
                    (format "TS:%S (%s)  Sender:%s  Message:%S"
                            (/ (leman-event-origin-server-ts (ewoc-data event)) 1000)
                            (format-time-string "%Y-%m-%d %H:%M:%S"
                                                (/ (leman-event-origin-server-ts (ewoc-data event)) 1000))
                            (leman-user-id (leman-event-sender (ewoc-data event)))
                            (when (alist-get 'body (leman-event-content (ewoc-data event)))
                              (substring-no-properties
                               (truncate-string-to-width (alist-get 'body (leman-event-content (ewoc-data event))) 20))))))
        (leman-debug "Comparing event timestamps:"
                     (list 'A (format-event node-a))
                     (list 'B (format-event node-b))))
      ;; NOTE: Matrix timestamps are in milliseconds.
      (let* ((a-ts (/ (leman-event-origin-server-ts (ewoc-data node-a)) 1000))
             (b-ts (/ (leman-event-origin-server-ts (ewoc-data node-b)) 1000))
             (diff-seconds (- b-ts a-ts))
             (leman-room-timestamp-header-format leman-room-timestamp-header-format))
        (when (and (>= diff-seconds leman-room-timestamp-header-delta)
                   (not (when-let ((node-after-a (ewoc-next ewoc node-a)))
                          (pcase (ewoc-data node-after-a)
                            (`(ts . ,_) t)
                            ((or 'leman-room-read-receipt-marker 'leman-room-fully-read-marker) t)))))
          (unless (equal (time-to-days a-ts) (time-to-days b-ts))
            ;; Different date: bind format to print date.
            (let ((leman-room-timestamp-header-format leman-room-timestamp-header-with-date-format))
              ;; Insert the date-only header.
              (setf node-a (ewoc-enter-after ewoc node-a (list 'ts b-ts)))))
          (with-silent-modifications
            ;; Avoid marking a buffer as modified just because we inserted a ts
            ;; header (this function may be called after other events which shouldn't
            ;; cause it to be marked modified, like moving the read markers).
            (ewoc-enter-after ewoc node-a (list 'ts b-ts))))))))

(cl-defun leman-room--insert-sender-headers
    (ewoc &optional (start-node (ewoc-nth ewoc 0)) (end-node (ewoc-nth ewoc -1)))
  ;; TODO: Use this in appropriate places.
  "Insert sender headers into EWOC.
Inserts headers between START-NODE and END-NODE, which default to
the first and last nodes in the buffer, respectively."
  (cl-labels ((message-event-p (data)
                (and (leman-event-p data)
                     (equal "m.room.message" (leman-event-type data)))))
    (when (and start-node (not (message-event-p (ewoc-data start-node))))
      ;; Start node not a message event: forward to next message event (and if none are
      ;; found, there's nothing to do).
      (setf start-node (leman-room--ewoc-next-matching ewoc start-node #'message-event-p)))
    (when end-node
      ;; Set end node to first message event after it.  (This simplifies the loop by
      ;; continuing until finding `end-node' or the last node, and ensures we fix headers
      ;; after any inserted messages.)
      (setf end-node (leman-room--ewoc-next-matching ewoc end-node #'message-event-p)))
    (let ((event-node start-node) prev-node)
      (while (and event-node (not (eq event-node end-node)))
        (setf prev-node
              ;; Find previous message or user header.
              (leman-room--ewoc-next-matching ewoc event-node
                (lambda (data)
                  (or (leman-user-p data) (message-event-p data)))
                #'ewoc-prev))
        (let ((sender (leman-event-sender (ewoc-data event-node))))
          (cond ((not prev-node)
                 ;; No previous message/sender: insert sender.
                 (ewoc-enter-before ewoc event-node sender))
                ((leman-user-p (ewoc-data prev-node))
                 ;; Previous node is a sender.
                 (unless (equal sender (ewoc-data prev-node))
                   ;; Previous node is the wrong sender: fix it.
                   (ewoc-set-data prev-node sender)))
                ((and (message-event-p (ewoc-data prev-node))
                      (not (equal sender (leman-event-sender (ewoc-data prev-node)))))
                 ;; Previous node is a message from a different sender: insert header.
                 (ewoc-enter-before ewoc event-node sender))))
        (setf event-node (leman-room--ewoc-next-matching ewoc event-node #'message-event-p))))))

(defun leman-room--coalesce-nodes (a b ewoc)
  "Try to coalesce events in nodes A and B in EWOC.
Return absorbing node if coalesced."
  ;; NOTE: This does not coalesce two `leman-room-membership-events' nodes; it only
  ;; coalesces an individual membership event into another one or into an
  ;; `leman-room-membership-events' node.
  ;; TODO: Allow two `leman-room-membership-events' nodes to be coalesced.
  (cl-labels ((coalescable-p (node)
                (or (and (leman-event-p (ewoc-data node))
                         (member (leman-event-type (ewoc-data node)) '("m.room.member")))
                    (leman-room-membership-events-p (ewoc-data node)))))
    (when (and (coalescable-p a) (coalescable-p b))
      (let* ((absorbing-node (if (or (leman-room-membership-events-p (ewoc-data a))
                                     (not (leman-room-membership-events-p (ewoc-data b))))
                                 a b))
             (absorbed-node (if (eq absorbing-node a) b a)))
        (when (cl-etypecase (ewoc-data absorbing-node)
                (leman-room-membership-events
                 (pcase-exhaustive leman-room-coalesce-events
                   ((pred integerp)
                    (< (length (leman-room-membership-events-events (ewoc-data absorbing-node)))
                       leman-room-coalesce-events))
                   (`t t)))
                (leman-event
                 (setf (ewoc-data absorbing-node)
                       (leman-room-membership-events--update
                        (make-leman-room-membership-events
                         :events (list (ewoc-data absorbing-node)))))))
          (push (ewoc-data absorbed-node)
                (leman-room-membership-events-events (ewoc-data absorbing-node)))
          (leman-room-membership-events--update (ewoc-data absorbing-node))
          (ewoc-delete ewoc absorbed-node)
          (ewoc-invalidate ewoc absorbing-node)
          absorbing-node)))))

(defun leman-room--timestamped-node-p (data)
  "Return non-nil if ewoc node datum DATA has a timestamp."
  (pcase data
    ((pred leman-event-p) t)
    ((pred leman-room-membership-events-p) t)
    (`(ts . ,_) t)))

(defun leman-room--read-marker-node-p (data)
  "Return non-nil if ewoc node datum DATA is a read marker."
  (member data '(leman-room-fully-read-marker
                 leman-room-read-receipt-marker)))

(defun leman-room--node-ts (data)
  "Return the timestamp of ewoc node datum DATA in milliseconds."
  (pcase data
    ((pred leman-event-p) (leman-event-origin-server-ts data))
    ((pred leman-room-membership-events-p)
     ;; Not sure whether to use earliest or latest ts; let's try this for now.
     (leman-room-membership-events-earliest-ts data))
    (`(ts ,ts)
     ;; Matrix server timestamps are in ms, so we must convert back.
     (* 1000 ts))))

(defun leman-room--node-ts< (a b)
  "Return non-nil if ewoc node datum A's timestamp is before B's."
  (< (leman-room--node-ts a) (leman-room--node-ts b)))

(defun leman-room--insert-event (event)
  "Insert EVENT into current buffer."
  (cl-labels ((format-event (event)
                (format "TS:%S (%s)  Sender:%s  Message:%S"
                        (/ (leman-event-origin-server-ts event) 1000)
                        (format-time-string "%Y-%m-%d %H:%M:%S"
                                            (/ (leman-event-origin-server-ts event) 1000))
                        (leman-user-id (leman-event-sender event))
                        (when (alist-get 'body (leman-event-content event))
                          (substring-no-properties
                           (truncate-string-to-width (alist-get 'body (leman-event-content event)) 20))))))
    (leman-debug "INSERTING NEW EVENT: " (format-event event))
    (let* ((ewoc leman-ewoc)
           (event-node-before (leman-room--ewoc-node-before ewoc event #'leman-room--node-ts< :pred #'leman-room--timestamped-node-p))
           new-node)
      ;; HACK: Insert after any read markers.
      (cl-loop for node-after-node-before = (ewoc-next ewoc event-node-before)
               while node-after-node-before
               while (leman-room--read-marker-node-p (ewoc-data node-after-node-before))
               do (setf event-node-before node-after-node-before))
      (setf new-node (if (not event-node-before)
                         (let ((first-node (ewoc-nth ewoc 0)))
                           (cond ((not first-node)
                                  (leman-debug "No event before it: add first.")
                                  (leman-debug "EWOC empty: add first.")
                                  (ewoc-enter-first ewoc event))
                                 ((and (leman-user-p (ewoc-data first-node))
                                       (equal (leman-event-sender event)
                                              (ewoc-data first-node)))
                                  (leman-debug "No event before it: add first.")
                                  (leman-debug "EWOC not empty.")
                                  (leman-debug "First node is header for this sender: insert after it, instead.")
                                  (setf event-node-before first-node)
                                  (ewoc-enter-after ewoc first-node event))
                                 (t
                                  (leman-debug "No event before it: add first.")
                                  (leman-debug "EWOC not empty.")
                                  (leman-debug "First node is not header for this sender: insert first.")
                                  (ewoc-enter-first ewoc event))))
                       (leman-debug "Found event before new event: insert after it.")
                       (when-let ((next-node (ewoc-next ewoc event-node-before)))
                         (when (and (leman-user-p (ewoc-data next-node))
                                    (equal (leman-event-sender event)
                                           (ewoc-data next-node)))
                           (leman-debug "Next node is header for this sender: insert after it, instead.")
                           (setf event-node-before next-node)))
                       (leman-debug "Inserting after event"
                                    ;; NOTE: `format-event' is only for debugging, and it
                                    ;; doesn't handle user headers, so commenting it out or now.
                                    ;; (format-event (ewoc-data event-node-before))

                                    ;; NOTE: And it's *Very Bad* to pass the raw node data
                                    ;; to `leman-debug', because it makes event insertion
                                    ;; *Very Slow*.  So we just comment that out for now.
                                    ;; (ewoc-data event-node-before)
                                    )
                       (ewoc-enter-after ewoc event-node-before event)))
      (when leman-room-coalesce-events
        ;; Try to coalesce events.
        ;; TODO: Move this to a separate function and call it from where this function is called.
        (setf new-node (or (when event-node-before
                             (leman-room--coalesce-nodes event-node-before new-node ewoc))
                           (when (ewoc-next ewoc new-node)
                             (leman-room--coalesce-nodes new-node (ewoc-next ewoc new-node) ewoc))
                           new-node)))
      (when leman-room-sender-in-headers
        (leman-room--insert-sender-headers ewoc new-node new-node))
      ;; Return new node.
      new-node)))

(defun leman-room--replace-event (new-event)
  "Replace appropriate event with NEW-EVENT in current buffer.
If replaced event is not found, return nil, otherwise non-nil."
  (let* ((ewoc leman-ewoc)
         (old-event-node (leman-room--ewoc-last-matching ewoc
                           (lambda (data)
                             (cl-typecase data
                               (leman-event (leman--events-equal-p data new-event)))))))
    (when old-event-node
      ;; TODO: Record old events in new event's local data, and make it accessible when inspecting the new event.
      (let ((node-before (ewoc-prev ewoc old-event-node))
            (inhibit-read-only t)
            (old-event (ewoc-data old-event-node)))
        ;; The original event's client-side data (e.g. reactions) and
        ;; read receipts are attached to it: carry them over so they
        ;; aren't lost when the edit replaces it in the buffer.
        (when-let ((old-local (leman-event-local old-event)))
          (setf (leman-event-local new-event)
                (append (leman-event-local new-event) old-local)))
        (when-let ((old-receipts (leman-event-receipts old-event)))
          (setf (leman-event-receipts new-event) old-receipts))
        (ewoc-delete ewoc old-event-node)
        (if node-before
            (ewoc-enter-after ewoc node-before new-event)
          (ewoc-enter-first ewoc new-event))))))

(cl-defun leman-room--ewoc-node-before (ewoc data <-fn
                                             &key (from 'last) (pred #'identity))
  "Return node in EWOC that matches PRED and belongs before DATA by <-FN.
Search from FROM (either `first' or `last')."
  (cl-assert (member from '(first last)))
  (if (null (ewoc-nth ewoc 0))
      (leman-debug "EWOC is empty: returning nil.")
    (leman-debug "EWOC has data: add at appropriate place.")
    (cl-labels ((next-matching (ewoc node next-fn pred)
                  (cl-loop do (setf node (funcall next-fn ewoc node))
                           until (or (null node)
                                     (funcall pred (ewoc-data node)))
                           finally return node)))
      (let* ((next-fn (pcase from ('first #'ewoc-next) ('last #'ewoc-prev)))
             (start-node (ewoc-nth ewoc (pcase from ('first 0) ('last -1)))))
        (unless (funcall pred (ewoc-data start-node))
          (setf start-node (next-matching ewoc start-node next-fn pred)))
        (if (funcall <-fn (ewoc-data start-node) data)
            (progn
              (leman-debug "New data goes before start node.")
              start-node)
          (leman-debug "New data goes after start node: find node before new data.")
          (let ((compare-node start-node))
            (cl-loop while (setf compare-node (next-matching ewoc compare-node next-fn pred))
                     until (funcall <-fn (ewoc-data compare-node) data)
                     finally return (if compare-node
                                        (progn
                                          (leman-debug "Found place: enter there.")
                                          compare-node)
                                      (leman-debug "Reached end of collection: insert there.")
                                      (pcase from
                                        ('first (ewoc-nth ewoc -1))
                                        ('last nil))))))))))

;;;;; Formatting

(defun leman-room--pp-thing (thing)
  "Pretty-print THING.
To be used as the pretty-printer for `ewoc-create'.  THING may be
an `leman-event' or `leman-user' struct, or a list like `(ts
TIMESTAMP)', where TIMESTAMP is a Unix timestamp number of
seconds."
  ;; TODO: Use handlers to insert so e.g. membership events can be inserted silently.

  ;; TODO: Use `cl-defmethod' and define methods for each of these THING types.  (I've
  ;; benchmarked thoroughly and found no difference in performance between using
  ;; `cl-defmethod' and using a `defun' with `pcase', so as long as the `cl-defmethod'
  ;; specializer is sufficient, I see no reason not to use it.)
  (pcase-exhaustive thing
    ((pred leman-event-p)
     (let ((beg (point)))
       (insert "" (leman-room--format-event thing leman-room leman-session))
       ;; Animations can only be started once the images are in this
       ;; buffer (see `leman-room--animate-images').
       (leman-room--animate-images beg (point))))
    ((pred leman-user-p)
     (insert (propertize (leman--format-user thing)
                         'display leman-room-username-display-property)))
    (`(ts ,(and (pred numberp) ts)) ;; Insert a date header.
     (let* ((string (format-time-string leman-room-timestamp-header-format ts))
            (width (string-width string))
            (maybe-newline (if (equal leman-room-timestamp-header-format leman-room-timestamp-header-with-date-format)
                               ;; HACK: Rather than using another variable, compare the format strings to
                               ;; determine whether the date is changing: if so, add a newline before the header.
                               (progn
                                 (cl-incf width 3)
                                 "\n")
                             ""))
            (alignment-space (pcase leman-room-timestamp-header-align
                               ('right (propertize " "
                                                   'display `(space :align-to (- text ,(1+ width)))))
                               ('center (propertize " "
                                                    'display `(space :align-to (- center ,(/ (1+ width) 2)))))
                               (_ " "))))
       (insert maybe-newline
               alignment-space
               (propertize string
                           'face 'leman-room-timestamp-header))))
    ((or 'leman-room-read-receipt-marker 'leman-room-fully-read-marker)
     (insert (propertize " "
                         'display '(space :width text :height (1))
                         'face thing)))
    ((pred leman-room-membership-events-p)
     (let ((formatted-events (leman-room--format-membership-events thing leman-room)))
       (add-face-text-property 0 (length formatted-events)
                               'leman-room-membership 'append formatted-events)
       (insert (leman-room-wrap-prefix formatted-events))))))

;; (defun leman-room--format-event (event)
;;   "Format `leman-event' EVENT."
;;   (pcase-let* (((cl-struct leman-event sender type content origin-server-ts) event)
;;                ((map body format ('formatted_body formatted-body)) content)
;;                (ts (/ origin-server-ts 1000)) ; Matrix timestamps are in milliseconds.
;;                (body (if (not formatted-body)
;;                          body
;;                        (pcase format
;;                          ("org.matrix.custom.html"
;;                           (leman-room--render-html formatted-body))
;;                          (_ (format "[unknown formatted-body format: %s] %s" format body)))))
;;                (timestamp (propertize
;;                            " " 'display `((margin left-margin)
;;                                           ,(propertize (format-time-string leman-room-timestamp-format ts)
;;                                                        'face 'leman-room-timestamp))))
;;                (body-face (pcase type
;;                             ("m.room.member" 'leman-room-membership)
;;                             (_ (if (equal (leman-user-id sender)
;;                                           (leman-user-id (leman-session-user leman-session)))
;;                                 'leman-room-self-message 'default))))
;;                (string (pcase type
;;                          ("m.room.message" body)
;;                          ("m.room.member" "")
;;                          (_ (format "[unknown event-type: %s] %s" type body)))))
;;     (add-face-text-property 0 (length body) body-face 'append body)
;;     (prog1 (concat timestamp string)
;;       ;; Hacky or elegant?  We return the string, but for certain event
;;       ;; types, we also insert a widget (this function is called by
;;       ;; EWOC with point at the insertion position).  Seems to work...
;;       (pcase type
;;         ("m.room.member"
;;          (widget-create 'leman-room-membership
;;                      :button-face 'leman-room-membership
;;                         :value (list (alist-get 'membership content))))))))

(defun leman-room--format-shield (event)
  "Return the authenticity marker for EVENT.
Decrypted (encrypted) messages get a lock when the sending device
is verified, a warning shield otherwise, with the reason in the
tooltip (the sdk's shield state, stashed by the decrypt path).
Plaintext messages get an open lock."
  (pcase (alist-get 'shield (leman-event-local event))
    (`(none)
     (propertize " 🔒" 'help-echo "Encrypted message (the sending device is verified)"))
    (`(red ,_code ,message)
     (propertize " ⚠️" 'help-echo (format "Encrypted message: %s" message) 'face 'error))
    (`(grey ,_code ,message)
     (propertize " ⚠️" 'help-echo (format "Encrypted message: %s" message) 'face 'shadow))
    (_ (when (equal (leman-event-type event) "m.room.message")
         (propertize " 🔓" 'help-echo "Not encrypted")))))

(defun leman-room--format-event (event room session)
  "Return EVENT in ROOM on SESSION formatted.
Formats according to `leman-room-message-format-spec', which see."
  (concat (pcase (leman-event-type event)
            ;; TODO: Define these with a macro, like the defevent and format-spec ones.
            ("m.room.message" (leman-room--format-message event room session))
            ("m.room.member"
             (widget-create 'leman-room-membership
                            :button-face 'leman-room-membership
                            :value event)
             "")
            ("m.reaction"
             ;; Handled by defevent-based handler.
             "")
            ("m.room.avatar"
             (leman-room-wrap-prefix
               (format "%s changed the room's avatar."
                       (propertize (leman--user-displayname-in room (leman-event-sender event))
                                   'help-echo (leman-user-id (leman-event-sender event))))
               'face 'leman-room-membership))
            ("m.room.power_levels"
             (leman-room--format-power-levels-event event room session))
            ("m.room.canonical_alias"
             (leman-room--format-canonical-alias-event event room session))
            ;; NOTE: Only undecryptable events have this type in the
            ;; buffer: successfully decrypted ones are made into
            ;; events of their decrypted type before rendering.
            ("m.room.encrypted"
             (leman-room-wrap-prefix
               (format "%s sent an encrypted message (unable to decrypt)."
                       (propertize (leman--user-displayname-in room (leman-event-sender event))
                                   'help-echo (leman-user-id (leman-event-sender event))))
               'face 'leman-room-membership))
             (_ (leman-room-wrap-prefix
                  (format "[sender:%s type:%s]"
                          (leman-user-id (leman-event-sender event))
                          (leman-event-type event))
                  'help-echo (format "%S" (leman-event-content event)))))
           ;; Encrypted messages carry their authenticity marker.
           (leman-room--format-shield event)
           (propertize " "
                       'display leman-room-event-separator-display-property)))

(defun leman-room--format-reactions (event room)
  "Return formatted reactions to EVENT in ROOM."
  ;; TODO: Like other events, pop to a buffer showing the raw reaction events when a key is pressed.
  (cl-labels
       ((reaction-image (key)
          (when-let* ((leman-room-images)
                      ((string-prefix-p "mxc://" key))
                      (session leman-session)
                      (url (leman--mxc-to-authenticated-url key session))
                      (data (or (leman-room--shr-image-data url)
                                ;; Not in the URL cache: fetch it in the
                                ;; background; the event will be re-rendered
                                ;; (showing the image) when it arrives.
                                (progn
                                  (leman-room--fetch-html-image
                                   url event room (leman-session-token session))
                                  nil))))
            ;; NOTE: `shr-get-image-data' returns (DATA CONTENT-TYPE)
            ;; in Emacs 30+, and just DATA (a string) in older
            ;; versions; normalize to the data string.
            (condition-case err
                (create-image (if (consp data) (car data) data)
                              nil 'data-p :ascent 'center
                              :max-width leman-room-reaction-emoji-max-size
                              :max-height leman-room-reaction-emoji-max-size)
              ;; Don't let an unreadable image break message rendering.
              (error (leman-debug "Reaction emoji image error:" err) nil))))
       (format-reaction (ks)
          (pcase-let* ((`(,raw-key . ,senders) ks)
                       (key-image (reaction-image raw-key))
                       (key (if key-image
                                (propertize " " 'display key-image)
                              (propertize raw-key 'face 'leman-room-reactions-key)))
                       (count (propertize (format " (%s)" (length senders))
                                          'face 'leman-room-reactions))
                       (string
                        (propertize (concat key count)
                                    ;; NOTE: The raw key is stored in a text property so it can be
                                    ;; toggled when the displayed key is, e.g., an image for a
                                    ;; custom-emoji reaction (in which case the raw key does not
                                    ;; appear in the buffer text).
                                    'leman-reaction-key raw-key
                                    'button '(t)
                                    'category 'default-button
                                    'action #'leman-room-reaction-button-action
                                    'follow-link t
                                    'help-echo (lambda (_window buffer _pos)
                                                 ;; NOTE: If the reaction key string is a Unicode character composed
                                                 ;; with, e.g. "VARIATION SELECTOR-16", `string-to-char' ignores the
                                                 ;; composed modifier/variation-selector and just returns the first
                                                 ;; character of the string.  This should be fine, since it's just
                                                 ;; for the tooltip.
                                                 (concat
                                                  (unless key-image
                                                    (concat (get-char-code-property (string-to-char raw-key) 'name) ": "))
                                                  (senders-names senders (buffer-local-value 'leman-room buffer))))))
                      (local-user-p (cl-member (leman-user-id (leman-session-user leman-session)) senders
                                               :key #'leman-user-id :test #'equal)))
           (when local-user-p
             (add-face-text-property 0 (length string) '(:box (:style pressed-button) :inverse-video t)
                                     nil string))
           (leman--remove-face-property string 'button)
           string))
       (senders-names (senders room)
         (cl-loop for sender in senders
                  collect (leman--user-displayname-in room sender)
                  into names
                  finally return (string-join names ", "))))
     (if-let ((reactions (map-elt (leman-event-local event) 'reactions)))
         (cl-loop with keys-senders
                  for reaction in reactions
                  for key = (map-nested-elt (leman-event-content reaction) '(m.relates_to key))
                  for sender = (leman-event-sender reaction)
                  do (push sender (alist-get key keys-senders nil nil #'string=))
                  finally do (setf keys-senders (cl-sort keys-senders #'> :key (lambda (pair) (length (cdr pair)))))
                  finally return (concat "\n  " (mapconcat #'format-reaction keys-senders "  ")))
       "")))

;;;; Threads

;; Threads (MSC3440): replies relating to a thread root with a
;; "m.thread" relationship.  Thread replies are not shown in the main
;; timeline; the root shows a summary chip which opens a thread view
;; buffer (see `leman-room-view-thread').

(defun leman-room--threads-table (room)
  "Return ROOM's table of thread events, keyed by root event ID.
The table is created if necessary."
  (or (map-elt (leman-room-local room) 'threads)
      (let ((table (make-hash-table :test #'equal)))
        (setf (map-elt (leman-room-local room) 'threads) table)
        table)))

(defun leman-room--thread-events (room root-id)
  "Return known thread events for root ROOT-ID in ROOM."
  (gethash root-id (leman-room--threads-table room)))

(defun leman-room--add-thread-event (event room)
  "Store thread-reply EVENT in ROOM's thread data.
EVENT is a reply relating to a thread root with an \"m.thread\"
relationship.  Returns the root's event ID."
  (pcase-let* (((cl-struct leman-event content) event)
               ((map ('m.relates_to (map ('event_id root-id)))) content))
    (cl-pushnew event (gethash root-id (leman-room--threads-table room))
                :key #'leman-event-id :test #'equal)
    root-id))

(defun leman-room--thread-event-for-id (event-id room)
  "Return the thread event with ID in ROOM, if known."
  (cl-loop for events being the hash-values of (leman-room--threads-table room)
           thereis (cl-find event-id events :key #'leman-event-id :test #'equal)))

(defun leman-room--replace-thread-event (event room)
  "Replace the thread event in ROOM that EVENT edits.
Returns non-nil if the replaced event was found in a thread.
NOTE: The original event is replaced by a copy whose content is
merged with the edit's \"m.new_content\", so the event keeps its
original ID and relationships."
  (pcase-let* (((cl-struct leman-event content) event)
               ((map ('m.relates_to (map ('event_id replaced-id)))) content)
               (table (leman-room--threads-table room))
               (new-content (map-elt content 'm.new_content)))
    (cl-loop for events being the hash-values of table
             for pos = (cl-position replaced-id events :key #'leman-event-id :test #'equal)
             when (and pos new-content)
             do (let ((copy (copy-leman-event (nth pos events))))
                  ;; NOTE: New keys are prepended so they take
                  ;; precedence in `alist-get' lookups.
                  (setf (leman-event-content copy)
                        (append new-content (leman-event-content copy))
                        (nth pos events) copy))
             and return t)))

(defun leman-room--thread-buffer (root-id room)
  "Return the open thread buffer for thread rooted at ROOT-ID in ROOM, if any."
  (cl-find-if (lambda (buffer)
                (with-current-buffer buffer
                  (and (derived-mode-p 'leman-thread-mode)
                       (equal leman-thread-root-id root-id)
                       (equal leman-room room))))
              (buffer-list)))

(defun leman-room--thread-root-id (event)
  "Return the thread root ID for thread-reply EVENT, if any."
  (when (equal (map-nested-elt (leman-event-content event) '(m.relates_to rel_type))
               "m.thread")
    (map-nested-elt (leman-event-content event) '(m.relates_to event_id))))

(defun leman-room--rerender-thread-for-event (event room)
  "Re-render any open thread buffer for ROOM that shows EVENT.
EVENT may be the thread's root or a reply in it.  This is
necessary for events that are not in the room buffer's main
timeline (i.e. thread replies), whose fetched images can
otherwise never be shown."
  (let ((root-id (or (leman-room--thread-root-id event)
                     (leman-event-id event))))
    (when-let ((thread-buffer (leman-room--thread-buffer root-id room)))
      (with-current-buffer thread-buffer
        (leman-room--render-thread
         thread-buffer
         (or (gethash root-id (leman-session-events leman-session))
             (make-leman-event :id root-id)))))))

(defun leman-room--thread-summary (event)
  "Return EVENT's server-side thread summary, if any.
This is the \"m.thread\" aggregation in EVENT's unsigned data."
  (map-nested-elt (leman-event-unsigned event) '(m.relations m.thread)))

(defun leman-room--format-thread-chip (event room)
  "Return a summary chip for the thread rooted at EVENT in ROOM.
If EVENT is not a thread root with known replies, return an empty
string."
  (let* ((root-id (leman-event-id event))
         (local-events (leman-room--thread-events room root-id))
         ;; NOTE: The server-side summary may know about replies we
         ;; haven't seen; the local events may include ones the
         ;; summary doesn't (e.g. while the server aggregates).
         (summary (leman-room--thread-summary event))
         (count (max (length local-events)
                     (or (alist-get 'count summary) 0))))
    (if (> count 0)
        (let* ((latest-event (or (car (cl-sort (copy-sequence local-events) #'> :key #'leman-event-origin-server-ts))
                                 ;; NOTE: The summary's "latest_event" is
                                 ;; a raw event alist; make it into an
                                 ;; event struct for uniform access.
                                 (when-let ((raw (alist-get 'latest_event summary)))
                                   (leman--make-event raw))))
               (snippet (when latest-event
                          (truncate-string-to-width
                           (or (map-elt (leman-event-content latest-event) 'body) "")
                           40 nil nil "…")))
               (label (format "\U0001f9f5 %d" count)))
          (concat
           (leman--button-buttonize
            (propertize label 'face 'leman-room-reactions)
            (lambda (_button) (leman-room-view-thread)))
           (when snippet
             (propertize " " 'display (propertize snippet 'face 'leman-room-reactions)))))
      "")))

(defun leman-room-view-thread (&optional position)
  "View the thread at POSITION in the current room buffer.
The thread root is the event at POSITION, or the event that a
thread reply at POSITION relates to."
  (interactive "d")
  (pcase-let* ((event (ewoc-data (ewoc-locate leman-ewoc (or position (point))))))
    (unless (leman-event-p event)
      (user-error "No event at point"))
    (pcase-let* (((cl-struct leman-event content) event)
                 ((map ('m.relates_to (map ('rel_type rel-type) ('event_id related-id)))) content)
                 (root-id (if (equal rel-type "m.thread") related-id (leman-event-id event))))
      (leman-room--view-thread root-id))))

(cl-defun leman-room--view-thread (root-id)
  "Open a buffer showing the thread rooted at ROOT-ID.
The thread's known events are shown; the full thread is fetched
from the server and the buffer re-rendered when it arrives."
  (let* ((room leman-room)
         (session leman-session)
         (root-event (or (gethash root-id (leman-session-events session))
                         (make-leman-event :id root-id)))
         (buffer (get-buffer-create
                  (format "*Leman Thread: %s*"
                          (or (leman-room-display-name room)
                              (leman-room-id room))))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'leman-thread-mode)
        (leman-thread-mode))
      (setf leman-thread-root-id root-id
            leman-room room
            leman-session session)
      (leman-room--render-thread buffer root-event))
    (pop-to-buffer buffer)
    ;; Fetch the full thread from the server.
    (leman-api session
        (format "rooms/%s/relations/%s"
                (url-hexify-string (leman-room-id room))
                (url-hexify-string root-id))
      :version "v1"
      :params '(("dir" "f") ("limit" "200"))
      :then (lambda (data)
              (when-let ((buffer-live buffer))
                (with-current-buffer buffer
                  (cl-loop for raw-event across (alist-get 'chunk data)
                           for event = (leman--make-event raw-event)
                           do (leman-room--process-fetched-thread-event event room))
                  (leman-room--render-thread buffer root-event)))))))

(defun leman-room--process-fetched-thread-event (event room)
  "Add fetched thread EVENT to ROOM's thread data."
  (pcase-let* (((cl-struct leman-event content) event)
               ((map ('m.relates_to (map ('rel_type rel-type)))) content))
    (pcase rel-type
      ("m.thread" (leman-room--add-thread-event event room))
      ("m.replace" (leman-room--replace-thread-event event room)))))

(defun leman-room--render-thread (buffer root-event)
  "Render in BUFFER the thread rooted at ROOT-EVENT."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (events (leman-room--thread-events leman-room (leman-event-id root-event))))
      (erase-buffer)
      (insert (propertize "Thread:\n" 'face 'leman-room-name))
      ;; Root event.
      (leman-room--insert-thread-event root-event)
      ;; Replies, oldest first.
      (dolist (event (cl-sort (copy-sequence events) #'< :key #'leman-event-origin-server-ts))
        (leman-room--insert-thread-event event))
      (goto-char (point-max))
      (insert (propertize (format "\n%s" (concat (propertize "RET" 'face 'help-key-binding)
                                                 " Reply in thread  "
                                                 (propertize "g" 'face 'help-key-binding)
                                                 " Refresh  "
                                                 (propertize "q" 'face 'help-key-binding)
                                                 " Quit"))
                          'face 'leman-room-timestamp)))))

(defun leman-room--insert-thread-event (event)
  "Insert EVENT into the current thread buffer."
  (pcase-let* ((room leman-room)
               (sender (leman--user-displayname-in room (leman-event-sender event)))
               (ts (format-time-string leman-room-timestamp-format
                                       (/ (leman-event-origin-server-ts event) 1000)))
               (body (or (leman-room--format-message-body event leman-session)
                         "")))
    (insert "\n"
            (propertize (format "%s  %s\n" sender ts)
                        'face 'leman-room-user)
            body "\n")
    (when-let ((reactions (leman-room--format-reactions event room)))
      (unless (string-empty-p reactions)
        (insert reactions)))))

(defvar leman-thread-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'leman-room-reply-in-thread)
    (define-key map (kbd "S-<return>") #'leman-room-reply-in-thread)
    (define-key map (kbd "r") #'leman-room-reply-in-thread)
    (define-key map (kbd "g") #'leman-room-refresh-thread)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for Leman thread buffers.")

(defvar-local leman-thread-root-id nil
  "Event ID of the thread root shown in the current thread buffer.")

(define-derived-mode leman-thread-mode special-mode "Leman-Thread"
  "Major mode for buffers showing a Leman thread.")

(defun leman-room-refresh-thread ()
  "Refresh the current thread buffer."
  (interactive)
  (when-let* ((root-id leman-thread-root-id)
              (root-event (gethash root-id (leman-session-events leman-session))))
    (let ((buffer (current-buffer)))
      (leman-api leman-session
          (format "rooms/%s/relations/%s"
                  (url-hexify-string (leman-room-id leman-room))
                  (url-hexify-string root-id))
        :version "v1"
        :params '(("dir" "f") ("limit" "200"))
        :then (lambda (data)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (cl-loop for raw-event across (alist-get 'chunk data)
                             for event = (leman--make-event raw-event)
                             do (leman-room--process-fetched-thread-event event leman-room))
                    (leman-room--render-thread buffer root-event))))))))

(defun leman-room-reply-in-thread ()
  "Reply in the thread shown in the current buffer."
  (interactive)
  (unless (derived-mode-p 'leman-thread-mode)
    (user-error "Not in a Leman thread buffer"))
  (when-let* ((root-id leman-thread-root-id)
              (text (read-string "Reply in thread: ")))
    (unless (string-empty-p text)
      (let* ((room leman-room)
             (session leman-session)
             ;; Latest known event in the thread: used as the
             ;; in-reply-to fallback target for unthreaded clients.
             (latest-event (car (cl-sort (copy-sequence
                                          (leman-room--thread-events room root-id))
                                         #'> :key #'leman-event-origin-server-ts)))
             (latest-id (or (and latest-event (leman-event-id latest-event))
                            (when-let* ((summary (leman-room--thread-summary
                                                 (gethash root-id (leman-session-events session)))))
                              (map-nested-elt summary '(latest_event event_id)))
                            root-id)))
        (leman-send-message room session
          :body text
          :filter (lambda (content _room)
                    (setf (alist-get "m.relates_to" content nil nil #'string=)
                          (leman-alist "rel_type" "m.thread"
                                       "event_id" root-id
                                       "m.in_reply_to" (leman-alist "event_id" latest-id)
                                       "is_falling_back" t))
                    content))))))

(defun leman-room--propertize-margins ()
  "Propertize margin text in current buffer."
  (when leman-room--format-message-wrap-prefix
    (when-let ((wrap-prefix-end (next-single-property-change (point-min) 'wrap-prefix-end)))
      (goto-char wrap-prefix-end)
      (delete-char 1)
      (let* ((prefix-width (string-width (buffer-substring-no-properties
                                          (line-beginning-position) (point))))
             (prefix (propertize " " 'display `((space :width ,prefix-width)))))
        ;; We apply the prefix to the entire event as `wrap-prefix', and to just the
        ;; body as `line-prefix'.
        (put-text-property (point-min) (point-max) 'wrap-prefix prefix)
        (put-text-property (point) (point-max) 'line-prefix prefix))))
  (when leman-room--format-message-margin-p
    (when-let ((left-margin-end (next-single-property-change (point-min) 'left-margin-end)))
      (goto-char left-margin-end)
      (delete-char 1)
      (let ((left-margin-text-width (string-width (buffer-substring-no-properties (point-min) (point)))))
        ;; It would be preferable to not have to allocate a string to
        ;; calculate the display width, but I don't know of another way.
        (put-text-property (point-min) (point)
                           'display `((margin left-margin)
                                      ,(buffer-substring (point-min) (point))))
        (save-excursion
          (goto-char (point-min))
          ;; Insert a string with a display specification that causes it to be displayed in the
          ;; left margin as a space that displays with the width of the difference between the
          ;; left margin's width and the display width of the text in the left margin (whew).
          ;; This is complicated, but it seems to work (minus a possible Emacs/Gtk bug that
          ;; sometimes causes the space to have a little "junk" displayed in it at times, but
          ;; that's not our fault).  (And this is another example of how well-documented Emacs
          ;; is: this was only possible by carefully reading the Elisp manual.)
          (insert (propertize " " 'display `((margin left-margin)
                                             (space :width (- left-margin ,left-margin-text-width))))))))
    (when-let ((right-margin-start (next-single-property-change (point-min) 'right-margin-start)))
      (goto-char right-margin-start)
      (delete-char 1)
      (let ((string (buffer-substring (point) (point-max))))
        ;; Relocate its text to the beginning so it won't be
        ;; displayed at the last line of wrapped messages.
        (delete-region (point) (point-max))
        (goto-char (point-min))
        (insert-and-inherit
         (propertize " "
                     'display `((margin right-margin) ,string)))))))

(defun leman-room--format-spec-value (formatter event room session spec)
  "Return the value that formatter FORMATTER gives for format spec SPEC.
EVENT, ROOM, and SESSION are passed to FORMATTER.  If FORMATTER
returns nil, return a placeholder string."
  (or (funcall formatter event room session)
      (let ((print-level 1))
        (propertize (format "[Event has no value for spec \"?%s\"]" (char-to-string spec))
                    'face 'font-lock-comment-face
                    'help-echo (format "%S" event)))))

(cl-defun leman-room--format-message (event room session &optional (format leman-room-message-format-spec))
  "Return EVENT in ROOM on SESSION formatted according to FORMAT.
Format defaults to `leman-room-message-format-spec', which see."
  ;; Bind this locally so formatters can modify it for this call.
  (let ((leman-room--format-message-margin-p)
        (left-margin-width leman-room-left-margin-width)
        (right-margin-width leman-room-right-margin-width))
    (cl-labels ((expand-format-spec ()
                  ;; Copied from `format-spec'.
                  (insert format)
                  (goto-char (point-min))
                  (while (search-forward "%" nil t)
                    (cond
                     ((eq (char-after) ?%)
                      ;; Quoted percent sign.
                      (delete-char 1))
                     ((looking-at "\\([-0-9.]*\\)\\([a-zA-Z]\\)")
                      ;; Valid format spec.
                      (let* ((num (match-string 1))
                             (spec (string-to-char (match-string 2)))
                             (_
                              ;; We delete the specifier now, because the formatter may change the
                              ;; match data, and we already have what we need.
                              (delete-region (1- (match-beginning 0)) (match-end 0)))
                             (formatter (or (alist-get spec leman-room-event-formatters)
                                            (error "Invalid format character: `%%%c'" spec)))
                             (val (leman-room--format-spec-value formatter event room session spec))
                             ;; Pad result to desired length.
                             (text (format (concat "%" num "s") val)))
                        (insert text)))
                     (t
                      ;; Signal an error on bogus format strings.
                      (error "leman-room--format-message: Invalid format string: %S" format))))))
      ;; Copied from `format-spec'.
      (with-current-buffer
          (or (get-buffer " *leman-room--format-message*")
              ;; TODO: Kill this buffer when disconnecting from all sessions.
              (with-current-buffer (get-buffer-create " *leman-room--format-message*")
                (setq buffer-undo-list t)
                (current-buffer)))
        (erase-buffer)
        ;; Pretend this is a room buffer.
        (setf leman-session session
              leman-room room)
        ;; HACK: Setting these buffer-locally in a temp buffer is ugly.
        (setq-local leman-room-left-margin-width left-margin-width)
        (setq-local leman-room-right-margin-width right-margin-width)
        (expand-format-spec)
        ;; Propertize margin text.
        (leman-room--propertize-margins)
        (buffer-string)))))

(cl-defun leman-room--format-message-body (event session &key (formatted-p t))
  "Return formatted body of \"m.room.message\" EVENT on SESSION.
If FORMATTED-P, return the formatted body content, when available."
  (pcase-let* (((cl-struct leman-event content
                           (unsigned (map ('redacted_by unsigned-redacted-by)))
                           (local (map ('redacted-by local-redacted-by))))
                event)
               ((map ('body main-body) msgtype ('format content-format) ('formatted_body formatted-body)
                     ('m.relates_to (map ('rel_type rel-type)))
                     ('m.new_content (map ('body new-body) ('formatted_body new-formatted-body)
                                          ('format new-content-format))))
                content)
               (body (or new-body main-body))
               (formatted-body (or new-formatted-body formatted-body))
               (body (if (or (not formatted-p) (not formatted-body))
                         ;; Copy the string so as not to add face properties to the one in the struct.
                         (copy-sequence body)
                       (pcase (or new-content-format content-format)
                          ("org.matrix.custom.html"
                           (save-match-data
                             (let ((rendered (leman-room--render-html formatted-body session)))
                               ;; Fetch any mxc:// images in the body
                               ;; asynchronously; the event is
                               ;; re-rendered when they arrive.
                               ;; NOTE: The loop's match position
                               ;; must be captured before calling
                               ;; 'leman--mxc-to-authenticated-url',
                               ;; which clobbers the match data.
                               (let ((start 0))
                                 (while (string-match "mxc://[^\"' >]+" formatted-body start)
                                   (let ((mxc (match-string 0 formatted-body))
                                         (next (match-end 0)))
                                     (leman-room--fetch-html-image
                                      (leman--mxc-to-authenticated-url mxc session)
                                      event leman-room
                                      (leman-session-token session))
                                     (setf start next))))
                               rendered)))
                         (_ (format "[unknown body format: %s] %s"
                                    (or new-content-format content-format) body)))))
               (appendix (pcase msgtype
                           ;; TODO: Face for m.notices.
                           ((or "m.text" "m.emote" "m.notice") nil)
                           ("m.image" (leman-room--format-m.image event session))
                           ("m.file" (leman-room--format-m.file event))
                           ("m.video" (leman-room--format-m.video event))
                           ("m.audio" (leman-room--format-m.audio event))
                           (_ (if (or local-redacted-by unsigned-redacted-by)
                                  nil
                                (format "[unsupported msgtype: %s]" msgtype ))))))
    (when body
      ;; HACK: Once I got an error when body was nil, so let's avoid that.
      (setf body (leman-room--linkify-urls body)))
    ;; HACK: Ensure body isn't nil (e.g. redacted messages can have empty bodies).
    (unless body
      (setf body (copy-sequence
                  ;; Yes, copying this string is necessary here too, otherwise a single
                  ;; string will be used across every call to this function, whose face
                  ;; properties will be added to every time in other functions, which will
                  ;; make a very big mess of face properties if a room's buffer is opened
                  ;; and closed a few times.
                  (if (or local-redacted-by unsigned-redacted-by)
                      "[redacted]"
                    "[message has no body content]"))))
    (when appendix
      (setf body (concat body " " appendix)))
    (when (equal "m.replace" rel-type)
      ;; Message is an edit.
      (setf body (concat body " " (propertize "[edited]" 'face 'font-lock-comment-face))))
    (when (and (or local-redacted-by unsigned-redacted-by)
               leman-room-hide-redacted-message-content)
      ;; Message is redacted and hiding is enabled: override the body to hide the content.
      ;; (This is a bit of a hack, since we've already prepared the body at this point,
      ;; but retrofitting this into the existing logic is more than I want to do right
      ;; now.  There are probably 3 or 4 different ways and places we could handle
      ;; redaction of content, and this seems like the simplest.)
      (setf body "[redacted]"))
    body))

(defun leman-room--store-image-in-url-cache (url data)
  "Store image DATA in the URL cache for URL.
The file mimics a raw HTTP response, the format that
`shr-get-image-data' expects to find in the URL cache."
  (let ((filename (url-cache-create-filename url)))
    (make-directory (file-name-directory filename) t)
    (with-temp-file filename
      (set-buffer-multibyte nil)
      (insert (format "HTTP/1.1 200 OK\r\nContent-Type: application/octet-stream\r\nContent-Length: %d\r\n\r\n"
                      (length data)))
      (insert data))))

(defun leman-room--invalidate-event-node (event room)
  "Invalidate EVENT's node in ROOM's buffer, if any.
This re-renders the event, displaying any images fetched since
its last rendering.  The node is found by event ID, so the event
is also re-rendered when its node now holds another struct for
the same event.  If EVENT has no node in the main timeline (e.g.
a thread reply), or is also shown in an open thread view (e.g. a
thread root), the thread view is re-rendered too."
  (let ((id (leman-event-id event)))
    (when-let* ((buffer (map-elt (leman-room-local room) 'buffer))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (when-let ((node (leman-room--ewoc-last-matching leman-ewoc
                           (lambda (node-data)
                             (and (leman-event-p node-data)
                                  (equal (leman-event-id node-data) id))))))
          (ewoc-invalidate leman-ewoc node))))
    (leman-room--rerender-thread-for-event event room)))

(cl-defun leman-room--fetch-html-image (url event room &optional token)
  "Fetch image URL for EVENT in ROOM, asynchronously.
TOKEN is the bearer token, for authenticated media URLs.  When
the data arrives, it is stored in the URL cache -- where the
renderer picks it up -- and every event waiting for the URL (at
least EVENT) is invalidated, re-rendering its message with the
image displayed.

While a fetch for URL is already in flight, EVENT is registered
as waiting and no new request is made: re-renders (e.g. caused by
receipts or typing notifications) must not amplify the fetch into
a duplicate request each time."
  (declare (indent defun))
  (unless (url-is-cached url)
    (if-let ((fetching (assoc url leman-room--fetching-html-images)))
        ;; Already fetching: register EVENT to be re-rendered when
        ;; the image arrives.
        (cl-pushnew (cons event room) (cdr fetching) :test #'equal)
      (cl-pushnew (list url (cons event room)) leman-room--fetching-html-images
                  :test #'equal)
      (plz-run
       (plz-queue leman-images-queue
         'get url :as 'binary :noquery t
         :headers (when token
                    (list (cons "Authorization" (concat "Bearer " token))))
         :then (lambda (data)
                 (leman-room--store-image-in-url-cache url data)
                 ;; Re-render every event waiting for this URL, and
                 ;; stop tracking the fetch.
                 (when-let ((entry (assoc url leman-room--fetching-html-images)))
                   (setf leman-room--fetching-html-images
                         (delq entry leman-room--fetching-html-images))
                   (pcase-dolist (`(,waiting-event . ,waiting-room) (cdr entry))
                     (leman-room--invalidate-event-node waiting-event waiting-room))))
         :else (lambda (plz-error)
                 ;; Stop tracking the failed fetch: waiting events
                 ;; keep their current rendering (as before).
                 (setf leman-room--fetching-html-images
                       (delq (assoc url leman-room--fetching-html-images)
                             leman-room--fetching-html-images))
                 (leman-debug "HTML image fetch failed:" url plz-error)))))))

(defun leman-room--shr-image-data (url)
  "Return image spec for URL from the URL cache, if present.
Unlike shr's own image handling, this never fetches: rendering
never blocks on the network."
  (and (url-is-cached url)
       (shr-get-image-data url)))

(defun leman-room--render-html (string session)
  "Return rendered version of HTML STRING from SESSION.
HTML is rendered to Emacs text using `shr-insert-document'."
  ;; NOTE: libxml's parser drops valueless attributes, so normalize
  ;; the valueless spoiler form (e.g. as sent by Element's /spoiler
  ;; command) to the empty-valued form, which is preserved.
  (setq string (replace-regexp-in-string
                "data-mx-spoiler\\([ \t\r\n>]\\)" "data-mx-spoiler=\"\"\\1" string))
  (with-current-buffer
      (or (get-buffer " *leman-room--render-html*")
          ;; TODO: Kill this buffer when disconnecting from all sessions.
          (with-current-buffer (get-buffer-create " *leman-room--render-html*")
            (setq buffer-undo-list t)
            (current-buffer)))
    (erase-buffer)
    (insert string)
    (save-excursion
      ;; NOTE: We workaround `shr`'s not indenting the blockquote properly (it
      ;; doesn't seem to compensate for the margin).  I don't know exactly how
      ;; `shr-tag-blockquote' and `shr-mark-fill' and `shr-fill-line' and
      ;; `shr-indentation' work together, but through trial-and-error, this
      ;; seems to work.  It even seems to work properly when a window is
      ;; resized (i.e. the wrapping is adjusted automatically by redisplay
      ;; rather than requiring the message to be re-rendered to HTML).
      (let ((shr-use-fonts leman-room-use-variable-pitch)
            ;; NOTE: Don't let shr start animations here: it anchors
            ;; them to this temporary buffer, which the rendered text
            ;; is immediately copied out of, killing them (or making
            ;; them janky).  They are started in the room buffer by
            ;; `leman-room--animate-images' instead.
            (shr-image-animate nil)
            (old-fn (symbol-function 'shr-tag-blockquote)) ;; Bind to a var to avoid unknown-function linting errors.
            (old-span-fn (symbol-function 'shr-tag-span)))
        (cl-letf (((symbol-function 'shr-fill-line) #'ignore)
                  ;; NOTE: Replace `shr-tag-img' to fetch images
                  ;; synchronously: shr's normal behavior inserts a
                  ;; placeholder and downloads asynchronously, but the
                  ;; render result is copied into the room buffer
                  ;; before that could ever complete.  Also, images
                  ;; with mxc:// sources are downloaded through the
                  ;; session's authenticated media endpoint.
                  ((symbol-function 'shr-tag-img)
                   (lambda (dom &optional _url)
                     (let* ((alt (or (dom-attr dom 'alt) ""))
                            (src (and dom (or (dom-attr dom 'src)
                                              (dom-attr dom 'srcset))))
                            (url (and src (not (string-empty-p src))
                                      (if (string-prefix-p "mxc://" src)
                                          src (shr-expand-url src))))
                            (image (cond
                                    ((and url (string-prefix-p "mxc://" url)
                                          session)
                                     (leman-room--shr-image-data
                                      (leman--mxc-to-authenticated-url url session)))
                                    ((and url (not shr-inhibit-images)
                                          (not (shr-image-blocked-p url)))
                                     (if (string-prefix-p "data:" url)
                                         (shr-image-from-data
                                          (substring url (length "data:")))
                                       (leman-room--shr-image-data url)))
                                    (t nil)))
                            (put-image (when image
                                         (funcall shr-put-image-function
                                                  image alt
                                                  (list :width (shr-string-number (dom-attr dom 'width))
                                                        :height (shr-string-number (dom-attr dom 'height)))))))
                       (if (imagep put-image)
                           (progn
                             ;; Cap the displayed size: shr's own
                             ;; rescaling does not run here, because
                             ;; the render buffer is never displayed.
                             (setf (image-property put-image :max-width)
                                   (round (* shr-max-image-proportion (frame-pixel-width)))
                                   (image-property put-image :max-height)
                                   (round (* shr-max-image-proportion (frame-pixel-height))))
                             (when-let ((height (shr-string-number (dom-attr dom 'height))))
                               (setf (image-property put-image :max-height) height))
                             (when-let ((width (shr-string-number (dom-attr dom 'width))))
                               (setf (image-property put-image :max-width) width)))
                         (when (display-graphic-p)
                           ;; Non-graphic displays: `shr-put-image'
                           ;; already inserted the alt text.
                           (shr-insert (if (string-empty-p (string-trim alt))
                                           "*" (string-trim alt))))))))
                  ((symbol-function 'shr-tag-blockquote)
                   (lambda (dom)
                     (let ((beg (point-marker)))
                       (funcall old-fn dom)
                       (add-text-properties beg (point-max)
                                            '( wrap-prefix "    "
                                               line-prefix "    "))
                       ;; NOTE: We use our own gv, `leman-text-property'; very convenient.
                       (add-face-text-property beg (point-max) 'leman-room-quote 'append))))
                  ;; Matrix spoilers (MSC2014), e.g. as sent by
                  ;; Element's /spoiler command.
                  ((symbol-function 'shr-tag-span)
                   (lambda (dom)
                     (if-let ((reason (assq 'data-mx-spoiler (dom-attributes dom))))
                         (let ((reason (cdr reason)))
                           (leman-room--shr-tag-spoiler
                            dom (and (stringp reason) (not (string-empty-p reason)) reason)))
                       (funcall old-span-fn dom)))))
          (shr-insert-document
           (libxml-parse-html-region (point-min) (point-max))))))
    (string-trim (buffer-substring (point) (point-max)))))

(defvar leman-room-spoiler-keymap
  (let ((map (make-sparse-keymap)))
    ;; NOTE: Bind `mouse-2', not `mouse-1': spoilers have a
    ;; `follow-link' property, so a quick mouse-1 click is translated
    ;; to a mouse-2 click before key lookup; a mouse-1 binding would
    ;; never be invoked, and the click would fall through to the
    ;; global mouse-2 binding (yanking into the read-only buffer).
    (define-key map [mouse-2] #'leman-room-toggle-spoiler)
    (define-key map (kbd "RET") #'leman-room-toggle-spoiler)
    map)
  "Keymap on spoiler labels and revealed spoiler content.")

(defun leman-room-toggle-spoiler (&optional event)
  "Toggle visibility of the spoiler at point or where EVENT occurred.
EVENT should be a mouse event, or nil to toggle at point.
NOTE: The interactive spec must accept keyboard invocation (e.g.
RET), so it cannot use the \"e\" code, which requires a mouse
event; see also `leman-room-spoiler-keymap'."
  (interactive
   (list (and (mouse-event-p last-command-event) last-command-event)))
  (let* ((start (and event (mouse-event-p event) (event-start event)))
         (pos (if start (posn-point start) (point)))
         (buffer (if start (window-buffer (posn-window start)) (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (goto-char pos)
          (leman-room--toggle-spoiler-at-point))))))

(defun leman-room--toggle-spoiler-at-point ()
  "Toggle visibility of the spoiler content containing or following point."
  (let* ((content-beg (cond ((get-text-property (point) 'leman-spoiler-content)
                             ;; Point is in the content: its run is its extent.
                             (or (previous-single-property-change (1+ (point)) 'leman-spoiler-content)
                                 (point-min)))
                            ;; Point is on the label: content follows it.
                            (t (next-single-property-change (point) 'leman-spoiler-content))))
         (content-end (when content-beg
                        ;; Nil when the content extends to the end of
                        ;; the buffer.
                        (or (next-single-property-change content-beg 'leman-spoiler-content)
                            (point-max)))))
    (when (and content-beg content-end)
      (let ((inhibit-read-only t)
            (buffer-undo-list t))
        (if (get-text-property content-beg 'invisible)
            (remove-text-properties content-beg content-end '(invisible nil))
          (add-text-properties content-beg content-end
                               '(invisible leman-spoiler)))))))

(defun leman-room--shr-tag-spoiler (dom reason)
  "Insert DOM rendered by shr as a spoiler labeled with REASON.
The content is hidden; clicking the label or pressing RET reveals
it (see `leman-room-toggle-spoiler')."
  (let ((label (if (and reason (not (string-empty-p reason)))
                    (format "[spoiler: %s] " reason)
                  "[spoiler] "))
        (props (list 'leman-spoiler-content t
                     'invisible 'leman-spoiler
                     'keymap leman-room-spoiler-keymap
                     'mouse-face 'highlight
                     'follow-link t
                     'help-echo "RET/click to reveal or hide this spoiler")))
    (insert (propertize label
                        'face 'leman-room-spoiler
                        'keymap leman-room-spoiler-keymap
                        'mouse-face 'highlight
                        'follow-link t
                        'help-echo "RET/click to reveal or hide this spoiler"))
    (let ((content-beg (point))
          (inhibit-read-only t))
      (funcall (symbol-function 'shr-generic) dom)
      (add-text-properties content-beg (point) props))))

(defun leman-room--animate-images (beg end)
  "Start animating multi-frame images between BEG and END.
Images are rendered by shr in a temporary buffer, where an
animation cannot be anchored (the rendering is copied out of that
buffer, and animations must be anchored to where an image is
actually displayed to keep running and be updated), so instead
the animations are started here, after the rendering has been
inserted into the room buffer."
  (when leman-room-animate-message-images
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (when-let* ((display (get-text-property (point) 'display))
                    (image (cond ((imagep display) display)
                                 ((and (consp display) (imagep (car display)))
                                  (car display))))
                    ((cdr (image-multi-frame-p image))))
          ;; Anchoring the animation to this buffer and position
          ;; makes it stop by itself when the image is no longer
          ;; displayed here (e.g. its node was re-rendered).
          (image-animate image nil t (point)))
        (goto-char (or (next-single-property-change (point) 'display nil end)
                       end))))))

(cl-defun leman-room--event-mentions-user-p (event user &optional (room leman-room))
  "Return non-nil if EVENT in ROOM mentions USER."
  (pcase-let* (((cl-struct leman-event content) event)
               ((map body formatted_body) content)
               (body (or formatted_body body)))
    ;; FIXME: `leman--user-displayname-in' may not be returning the right result for the
    ;; local user, so test the displayname slot too.  (But even that may be nil sometimes?
    ;; Something needs to be fixed...)
    ;; HACK: So we use the username slot, which was created just for this, for now.
    (when body
      (cl-macrolet ((matches-body-p
                      (form) `(when-let ((string ,form))
                                (string-match-p (regexp-quote string) body))))
        (or (matches-body-p (leman-user-username user))
            (matches-body-p (leman--user-displayname-in room user))
            (matches-body-p (leman-user-id user)))))))

(defun leman-room--linkify-urls (string)
  "Return STRING with URLs in it made clickable."
  ;; Is there an existing Emacs function to do this?  I couldn't find one.
  ;; Yes, maybe: `goto-address-mode'.  TODO: Try goto-address-mode.
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (cl-loop while (re-search-forward (rx bow "http" (optional "s") "://" (1+ (not space)))
                                      nil 'noerror)
             do (make-text-button (match-beginning 0) (match-end 0)
                                  'mouse-face 'highlight
                                  'face 'link
                                  'help-echo (match-string 0)
                                  'action #'browse-url-at-mouse
                                  'follow-link t))
    (buffer-string)))

;; NOTE: This function is not useful when displaynames are shown in the margin, because
;; margins are not mouse-interactive in Emacs, therefore the help-echo function is called
;; with the string and the position in the string, which leaves the buffer position
;; unknown.  So we have to set the help-echo to a string rather than a function.  But the
;; function may be useful in the future, so leaving it commented for now.

;; (defun leman-room--user-help-echo (window _object pos)
;;   "Return user ID string for POS in WINDOW.
;; For use as a `help-echo' function on `leman-user' headings."
;;   (let ((data (with-selected-window window
;;                 (ewoc-data (ewoc-locate leman-ewoc pos)))))
;;     (cl-typecase data
;;       (leman-event (leman-user-id (leman-event-sender data)))
;;       (leman-user (leman-user-id data)))))

(defun leman-room--user-color (user)
  "Return a color in which to display USER's messages."
  (let* ((id (leman-user-id user))
         (id-hash (float (+ (abs (sxhash id)) leman-room-prism-color-adjustment)))
         ;; TODO: Wrap-around the value to get the color I want.
         (ratio (/ id-hash (float most-positive-fixnum)))
         (color-num (round (* (* 255 255 255) ratio)))
         (color-rgb (list (/ (float (logand color-num 255)) 255)
                          (/ (float (ash (logand color-num 65280) -8)) 255)
                          (/ (float (ash (logand color-num 16711680) -16)) 255)))
         (background-rgb (color-name-to-rgb (face-background 'default))))
    (when (< (leman--contrast-ratio color-rgb background-rgb) leman-room-prism-minimum-contrast)
      (setf color-rgb (leman--increase-contrast color-rgb background-rgb leman-room-prism-minimum-contrast
                                                (color-name-to-rgb (face-foreground 'default)))))
    (apply #'color-rgb-to-hex (append color-rgb (list 2)))))

;;;;; Compose buffer

;; Compose messages in a separate buffer, like `org-edit-special'.

(defvar-local leman-room-compose-buffer nil
  "Non-nil in buffers that are composing a message to a room.")

(cl-defun leman-room-compose-message (room session &key body)
  "Compose a message to ROOM on SESSION.
Interactively, with prefix, prompt for room and session,
otherwise use current room.  With BODY, use it as the initial
message contents."
  (interactive
   (leman-with-room-and-session
     (list leman-room leman-session)))
  (let* ((compose-buffer (generate-new-buffer (format "*Leman compose: %s*" (leman--room-display-name leman-room))))
         (send-message-filter leman-room-send-message-filter))
    (leman-room-compose-highlight compose-buffer)
    (with-current-buffer compose-buffer
      (leman-room-init-compose-buffer room session)
      (setf leman-room-send-message-filter send-message-filter)
      ;; TODO: Make mode configurable.
      (when body
        (insert body))

      ;; FIXME: Inexplicably, this doesn't do anything, so we comment it out for now.
      ;; (add-function :override (local 'org-mode)
      ;;               ;; HACK: Since `org-mode' kills buffer-local variables we need, we add
      ;;               ;; buffer-local advice to prevent that from happening in case a user enables it.
      ;;               (lambda (&rest _ignore)
      ;;                 (message "Use `leman-room-compose-org' to activate Org in this buffer")))

      ;; NOTE: Surprisingly, we don't run this hook in `leman-room-init-compose-buffer',
      ;; because if a function in that hook calls the init function (like
      ;; `leman-room-compose-org' does), it makes `run-hooks' recursive.  As long as this
      ;; is the only function that makes the compose buffer, and as long as none of the
      ;; hooks do anything that activating `org-mode' nullifies, this should be okay...
      (run-hooks 'leman-room-compose-hook))
    ;; Display the compose buffer.  This might obscure the room buffer's window
    ;; point, so minimise the amount of scrolling which occurs to restore that
    ;; to a visible position.
    (pop-to-buffer compose-buffer leman-room-compose-buffer-display-action)
    (unless leman-room-compose-buffer-window-auto-height
      (let ((scroll-conservatively 101))
        (redisplay)))))

(defun leman-room-compose-edit (event room session body)
  "Edit EVENT in ROOM on SESSION to have new BODY, using a compose buffer.
The message must be one sent by the local user."
  ;; See also `leman-room-edit-message'.
  (interactive (cl-destructuring-bind (event body)
                   (leman-room-edit-message-prepare)
                 (list event leman-room leman-session body)))
  (cl-assert (leman-event-p event)) (cl-assert room) (cl-assert session)
  (let ((leman-room-editing-event event))
    (leman-room-with-highlighted-event-at (point)
      (leman-room-compose-message room session :body body))))

(defun leman-room-compose-reply (event)
  "Write and send a reply to EVENT, using a compose buffer.
Interactively, to event at point."
  ;; See also `leman-room-write-reply'.
  (interactive (progn (cl-assert leman-ewoc)
                      (list (ewoc-data (ewoc-locate leman-ewoc)))))
  (cl-assert leman-room) (cl-assert leman-session) (cl-assert (leman-event-p event))
  (let ((leman-room-replying-to-event event))
    (leman-room-with-highlighted-event-at (point)
      (leman-room-compose-message leman-room leman-session))))

(defun leman-room-compose-from-minibuffer ()
  "Edit the current message in a compose buffer.
To be called from a minibuffer opened from
`leman-room-read-string'."
  (interactive)
  (cl-assert (minibufferp)) (cl-assert leman-room) (cl-assert leman-session)
  ;; TODO: When requiring Emacs 27, use `letrec'.
  ;; HACK: I can't seem to find a better way to do this, to exit the minibuffer without exiting this command too.
  (let* ((body (minibuffer-contents))
         (compose-fn-symbol (gensym (format "leman-compose-%s" (or (leman-room-canonical-alias leman-room)
                                                                   (leman-room-id leman-room)))))
         (input-method current-input-method) ; Capture this value from the minibuffer.
         (send-message-filter leman-room-send-message-filter)
         (replying-to-event leman-room-replying-to-event)
         (editing-event leman-room-editing-event)
         (compose-fn (lambda ()
                       ;; HACK: Since exiting the minibuffer restores the previous window configuration,
                       ;; we have to do some magic to get the new compose buffer to appear.
                       ;; TODO: Use letrec with Emacs 27.
                       (remove-hook 'minibuffer-exit-hook compose-fn-symbol)
                       ;; FIXME: Probably need to unintern the symbol.
                       (leman-room-compose-message leman-room leman-session :body body)
		       ;; FIXME: This doesn't propagate the send-message-filter to the minibuffer.
                       (setf leman-room-send-message-filter send-message-filter)
                       (setq-local leman-room-replying-to-event replying-to-event
                                   leman-room-editing-event editing-event)
                       (cond (replying-to-event
                              (setq-local header-line-format
                                          (concat header-line-format
                                                  (format " (Replying to message from %s)"
                                                          (leman--user-displayname-in
                                                           leman-room (leman-event-sender replying-to-event))))))
                             (editing-event
                              (setq-local header-line-format (concat header-line-format " (Editing message)"))))
                       (let* ((compose-buffer (current-buffer))
                              (show-buffer-fn-symbol (gensym "leman-show-compose-buffer"))
                              (show-buffer-fn (lambda ()
                                                (remove-hook 'window-configuration-change-hook show-buffer-fn-symbol)
                                                ;; FIXME: Probably need to unintern the symbol.
                                                (pop-to-buffer compose-buffer leman-room-compose-buffer-display-action)
                                                (set-input-method input-method))))
                         (fset show-buffer-fn-symbol show-buffer-fn)
                         (add-hook 'window-configuration-change-hook show-buffer-fn-symbol)))))
    (fset compose-fn-symbol compose-fn)
    (add-hook 'minibuffer-exit-hook compose-fn-symbol)
    ;; Deactivate minibuffer's input method, otherwise subsequent
    ;; minibuffers will have it, too.
    (deactivate-input-method)
    (abort-recursive-edit)))

(defun leman-room-compose-buffer-string-trimmed ()
  "Like `buffer-string' trimmed with `string-trim'."
  (buffer-substring-no-properties (progn (goto-char (point-min))
                                         (skip-chars-forward " \t\r\n")
                                         (point))
                                  (progn (goto-char (point-max))
                                         (skip-chars-backward " \t\r\n")
                                         (point))))

(defun leman-room-compose-send-prepare ()
  "Bindings for `leman-room-compose-send' and `leman-room-compose-send-direct'."
  (cl-assert leman-room-compose-buffer)
  (cl-assert leman-room) (cl-assert leman-session)
  ;; Capture the necessary values from the compose buffer before killing it and
  ;; switching back to the room buffer.  Return the values as a list.
  (let ((body (leman-room-compose-buffer-string-trimmed))
        (input-method current-input-method)
        (send-message-filter leman-room-send-message-filter)
        (replying-to-event leman-room-replying-to-event)
        (editing-event leman-room-editing-event)
        (room leman-room)
        (session leman-session))
    (leman-room-compose-buffer-quit-restore-window)
    (leman-view-room room session)
    (add-to-history 'leman-room-message-history body)
    (list body input-method send-message-filter replying-to-event editing-event room session)))

(defun leman-room-compose-send ()
  "Prompt to send the current compose buffer's contents.
To be called from an `leman-room-compose' buffer.
See also `leman-room-compose-send-direct'."
  (interactive)
  (cl-destructuring-bind (body input-method send-message-filter
                               replying-to-event editing-event room session)
      (leman-room-compose-send-prepare)
    (let* ((prompt (format "Send message (%s): " (leman-room-display-name room)))
           (current-input-method input-method) ; Bind around read-string call.
           (leman-room-send-message-filter send-message-filter)
           (body (if (or editing-event replying-to-event)
                     (let ((pos (ewoc-location (leman-room--ewoc-last-matching leman-ewoc
                                                 (lambda (data)
                                                   (eq data (or editing-event
                                                                replying-to-event)))))))
                       (leman-room-with-highlighted-event-at pos
                         (leman-room-read-string prompt body 'leman-room-message-history
                                                 nil 'inherit-input-method)))
                   (leman-room-read-string prompt body 'leman-room-message-history
                                           nil 'inherit-input-method))))
      (if editing-event
          (leman-room-edit-message (leman--original-event-for editing-event session)
                                   room session body)
        (leman-room-send-message room session
                                 :body body
                                 :replying-to-event (and replying-to-event
                                                         (leman--original-event-for
                                                          replying-to-event session)))))))

(defun leman-room-compose-send-direct ()
  "Directly send the current compose buffer's contents.
To be called from an `leman-room-compose' buffer.
See also `leman-room-compose-send'."
  (interactive)
  (cl-destructuring-bind (body _input-method send-message-filter
                               replying-to-event editing-event room session)
      (leman-room-compose-send-prepare)
    (let ((leman-room-send-message-filter send-message-filter))
      (if editing-event
          (leman-room-edit-message (leman--original-event-for editing-event session)
                                   room session body)
        (leman-room-send-message room session
                                 :body body
                                 :replying-to-event (and replying-to-event
                                                         (leman--original-event-for
                                                          replying-to-event session)))))))

(defun leman-room-compose-abort (&optional no-history)
  "Kill the compose buffer and window.
With prefix arg NO-HISTORY, do not add to `leman-room-message-history'."
  (interactive "P")
  (let ((body (leman-room-compose-buffer-string-trimmed))
        (room leman-room))
    (unless no-history
      (add-to-history 'leman-room-message-history body))
    (leman-room-compose-buffer-quit-restore-window)
    ;; Make sure we end up with the associated room buffer selected.
    (when-let ((win (catch 'room-win
                      (walk-windows
                       (lambda (win)
                         (with-selected-window win
                           (and (derived-mode-p 'leman-room-mode)
                                (bound-and-true-p leman-room)
                                (eq leman-room room)
                                (throw 'room-win win))))))))
      (select-window win))))

(defun leman-room-compose-abort-no-history ()
  "Kill the compose buffer and window without adding to the history."
  (interactive)
  (leman-room-compose-abort t))

(defun leman-room-init-compose-buffer (room session)
  "Set up the current buffer as a compose buffer.
Sets ROOM and SESSION buffer-locally, binds `save-buffer' in
a copy of the local keymap, and sets `header-line-format'."
  ;; Using a macro for this seems awkward but necessary.
  (setq-local leman-room room)
  (setq-local leman-session session)
  (setq-local leman-room-replying-to-event leman-room-replying-to-event)
  (setq-local leman-room-editing-event leman-room-editing-event)
  (setf leman-room-compose-buffer t)
  (setq-local completion-at-point-functions
              (append '(leman-room--complete-members-at-point leman-room--complete-rooms-at-point)
                      completion-at-point-functions))
  (setq-local dabbrev-select-buffers-function #'leman-compose-dabbrev-select-buffers
              dabbrev-friend-buffer-function #'leman-room-mode-p)
  (setq-local yank-excluded-properties
              (append '(line-prefix wrap-prefix)
                      (default-value 'yank-excluded-properties)))
  (add-hook 'isearch-mode-hook 'leman-room-compose-history-isearch-setup nil t)
  ;; FIXME: Compose with local map?
  (use-local-map (if (current-local-map)
                     (copy-keymap (current-local-map))
                   (make-sparse-keymap)))
  ;; When `leman-room-self-insert-mode' is enabled, deleting the final character of the
  ;; message aborts and kills the compose buffer.
  (local-set-key [remap delete-backward-char]
                 `(menu-item "" leman-room-compose-abort-no-history
                             :filter ,(lambda (cmd)
                                        (and leman-room-self-insert-mode
                                             (<= (buffer-size) 1)
                                             (save-restriction (widen) (eobp))
                                             cmd))))
  (local-set-key [remap save-buffer] #'leman-room-dispatch-send-message)
  (local-set-key (kbd "C-c C-k") #'leman-room-compose-abort)
  (local-set-key (kbd "M-p") #'leman-room-compose-history-prev-message)
  (local-set-key (kbd "M-n") #'leman-room-compose-history-next-message)
  (local-set-key (kbd "M-r") #'leman-room-compose-history-isearch-backward)
  (local-set-key (kbd "C-M-r") #'leman-room-compose-history-isearch-backward-regexp)
  (setq header-line-format
        (concat (substitute-command-keys
                 (format " Press \\[save-buffer] to send message to room (%s), or \\[leman-room-compose-abort] to cancel."
                         (leman-room-display-name room)))
                (cond (leman-room-replying-to-event
                       (format " (Replying to message from %s)"
                               (leman--user-displayname-in
                                leman-room (leman-event-sender
                                            leman-room-replying-to-event))))
                      (leman-room-editing-event
                       " (Editing message)"))))
  ;; Adjust the window height automatically.
  (when leman-room-compose-buffer-window-auto-height
    (add-hook 'post-command-hook
              #'leman-room-compose-buffer-window-auto-height nil :local)
    ;; Our `window-min-height' comprises header & mode line + body lines.
    (setq-local window-min-height
                (+ 2 (if leman-room-compose-buffer-window-auto-height-min
                         (max 1 leman-room-compose-buffer-window-auto-height-min)
                       1)))
    (when leman-room-compose-buffer-window-auto-height-fixed
      (setq-local window-size-fixed
                  leman-room-compose-buffer-window-auto-height-fixed))
    ;; The following helps when `window--sanitize-window-sizes' adjusts all
    ;; windows in a frame (e.g. when splitting windows), as otherwise any
    ;; existing compose buffer windows are liable to be resized line-wise,
    ;; resulting in excess padding being introduced.
    (when leman-room-compose-buffer-window-auto-height-pixelwise
      (setq-local window-resize-pixelwise t)))
  ;; Other compose buffer window behaviours.
  (add-hook 'window-state-change-functions
            #'leman-room-compose-buffer-window-state-change-handler nil :local)
  (add-hook 'window-buffer-change-functions
            #'leman-room-compose-buffer-window-buffer-change-handler nil :local))

(defun leman-room-compose-buffer-window-auto-height ()
  "Ensure that the compose buffer displays the whole message.

Called via `post-command-hook' if option
`leman-room-compose-buffer-window-auto-height' is non-nil."
  ;; We use `post-command-hook' (rather than, say, `after-change-functions'),
  ;; because the required window height might change for reasons other than text
  ;; editing (e.g. changes to the window's width or the font size).
  ;;
  ;; Note that changes to the default face size (e.g. via `text-scale-adjust')
  ;; affect `default-line-height', invalidating the cache even when the text
  ;; itself didn't change.
  ;;
  ;; The following may also clear the cache in order to force a recalculation:
  ;; - `leman-room-compose-buffer-window-state-change-handler'
  ;; - `leman-room-compose-buffer-window-buffer-change-handler'
  ;;
  ;; Global mutex `leman-room-compose-buffer-window-auto-height-resizing-p'
  ;; ensures that we cannot run recursively.  We also resize only the selected
  ;; window, even if there are compose buffers displayed in other windows which
  ;; might also be affected.  This conservative approach can prevent desirable
  ;; resizing in some cases, but restricting our behaviour this way keeps things
  ;; simple so that we needn't consider potential issues such as endless cycles
  ;; of conflicting resizes.
  ;;
  ;; Perfection would in any case be non-trivial -- consider two compose windows
  ;; side-by-side in a horizontal split, each showing a different compose buffer
  ;; with a different desired height.  We cannot have the "correct" size for
  ;; both simultaneously.  The best thing to do would be to maintain the tallest
  ;; height amongst all conflicting windows at all times -- but that is, again,
  ;; considerably more complex.
  ;;
  ;; Most of the time the window arrangements are expected to be very simple and
  ;; so a more comprehensive solution, while possible, is not worth the added
  ;; complexity -- our relatively simplistic approach is good enough for the
  ;; vast majority of situations.

  ;; Skip resizing if we are being called recursively...
  (unless (or (bound-and-true-p leman-room-compose-buffer-window-auto-height-resizing-p)
              ;; ...or there are no other windows to resize...
              (window-full-height-p)
              ;; ...or we have just switched to this buffer from another buffer
              ;; (we may be cycling window buffers, and about to switch again).
              (and (window-old-buffer)
                   (not (eq (window-old-buffer) (current-buffer)))))
    ;; Manipulate the window body height.
    (let* ((pixelwise (and leman-room-compose-buffer-window-auto-height-pixelwise
                           (display-graphic-p)))
           (lineheight (and pixelwise (default-line-height)))
           (buflines (max 1 (count-screen-lines nil nil t)))
           (cache (if pixelwise
                      (* buflines lineheight)
                    buflines))
           (wcache (window-parameter
                    nil 'leman-room-compose-buffer-window-auto-height-cache)))
      ;; Do nothing if the desired height has not changed.
      (unless (and wcache (eql cache wcache))
        ;; Otherwise resize the window...
        (set-window-parameter
         nil 'leman-room-compose-buffer-window-auto-height-cache cache)
        (let* ((leman-room-compose-buffer-window-auto-height-resizing-p t)
               (minheight (if leman-room-compose-buffer-window-auto-height-min
                              (max 1 leman-room-compose-buffer-window-auto-height-min)
                            1))
               (maxheight leman-room-compose-buffer-window-auto-height-max)
               (maxlines (or (and maxheight (min buflines maxheight))
                             buflines))
               (reqlines (max maxlines minheight)))
          (if pixelwise
              ;; In GUI frames we should do this in pixels, as the line-based
              ;; `window-resize' DELTA is based on the default frame character
              ;; height, rather than the buffer's `default-line-height', which
              ;; doesn't take face remapping (e.g. `text-scale-adjust') into
              ;; account and would therefore enlarge the window by the wrong
              ;; value.  Pixel-based resizing also lets us eliminate vertical
              ;; padding resulting from the body lines being a different height
              ;; to the mode- and/or header-line height (which can easily happen
              ;; in GUI frames and is distractingly obvious in a small window
              ;; which is supposed to fit its content).
              (let* ((window-resize-pixelwise t)
                     (pixheight (* lineheight reqlines))
                     (pixels (- pixheight (window-body-height nil t))))
                (when-let ((pixels (window-resizable nil pixels nil t t)))
                  (window-resize nil pixels nil t t)))
            ;; In terminal frames we deal in lines rather than pixels.
            (let ((delta (- reqlines (window-body-height))))
              (when-let ((delta (window-resizable nil delta nil t)))
                (window-resize nil delta nil t))))
          ;; Ask Emacs to "preserve" the new height.  So long as the window
          ;; maintains this height and is displaying this specific buffer, Emacs
          ;; will avoid unnecessary height changes from side-effects of commands
          ;; such as `balance-windows'.  Explicit height changes are allowed.
          ;; We must update this parameter every time we change the height so
          ;; that the "preserved" height value is always correct.
          (window-preserve-size nil nil t)
          ;; In most cases we can fit the whole buffer in the resized window.
          (set-window-start nil (point-min) :noforce)
          ;; The resizing might have obscured the room buffer's window point, so
          ;; minimise the amount of scrolling which occurs to restore that to a
          ;; visible position.
          (let ((scroll-conservatively 101))
            (redisplay)))))))

(defun leman-room-compose-buffer-window-state-change-handler (win)
  "Called via buffer-local `window-state-change-functions' in compose buffers.

Called for any window WIN showing a compose buffer if that window
has been added or assigned another buffer, changed size, or been
selected or deselected.

This prevents a compose buffer window being stuck at the wrong
height (until the number of lines changes again) if something
other than the auto-height feature resizes the window.  We simply
flush the auto-height cache, thus ensuring the required height is
recalculated on the next cycle).

See also `leman-room-compose-buffer-window-buffer-change-handler'."
  ;; Ignore the window state changes triggered by our auto-height resizing.
  ;;
  ;; Also do nothing if the state change is for the selected window, as
  ;; the buffer-local `post-command-hook' is already dealing with that
  ;; case.  We only care about window state changes which are triggered
  ;; from elsewhere.  This means we skip the case whereby the selected
  ;; window has just switched to the compose buffer, and so we use
  ;; `window-buffer-change-functions' as well to capture that case.
  ;; (See `leman-room-compose-buffer-window-buffer-change-handler'.)
  (when leman-room-compose-buffer-window-auto-height
    (unless (or (bound-and-true-p leman-room-compose-buffer-window-auto-height-resizing-p)
                (eq win (selected-window)))
      ;; Clear the auto-height cache for this window.
      (set-window-parameter
       win 'leman-room-compose-buffer-window-auto-height-cache nil))))

(defun leman-room-compose-buffer-window-buffer-change-handler (win)
  "Called via buffer-local `window-buffer-change-functions' in compose buffers.

Called for any window WIN showing a compose buffer if that window
has just been created or assigned that buffer.

Flush the auto-height cache for any window which switches to
displaying a compose buffer, to ensure the required height is
recalculated on the next cycle.

Also detect whether a composer buffer's window was created for
that purpose, as this information affects the behaviour of
`leman-room-compose-buffer-quit-restore-window'.

See also `leman-room-compose-buffer-window-state-change-handler'."
  (with-selected-window win
    (when leman-room-compose-buffer-window-auto-height
      ;; Clear the auto-height cache for this window.
      (set-window-parameter
       win 'leman-room-compose-buffer-window-auto-height-cache nil))
    ;; Establish whether we've processed this window before, and whether it was
    ;; created to display a compose buffer.  We set a window property the first
    ;; time that we see the window, so if it's set at all, we've seen it before.
    (unless (assq 'leman-room-compose-buffer-window-created-p (window-parameters win))
      ;; If the window has never shown any other buffer, then it was created
      ;; specifically to display a compose buffer.
      (let ((created-for-compose-p (set-window-parameter
                                    win 'leman-room-compose-buffer-window-created-p
                                    (not (window-prev-buffers win)))))
        ;; Process `leman-room-compose-buffer-window-dedicated' when the compose
        ;; buffer is first displayed in this window, to decide whether the
        ;; window should be dedicated to the buffer.
        (when (cl-case leman-room-compose-buffer-window-dedicated
                (created created-for-compose-p)
                (auto-height leman-room-compose-buffer-window-auto-height)
                (delete nil)
                (t leman-room-compose-buffer-window-dedicated))
          (set-window-dedicated-p win t))))))

(defun leman-room-compose-buffer-quit-restore-window ()
  "Kill the current compose buffer and deal appropriately with its window.

The default `leman-room-compose-buffer-window-dedicated' value
ensures that the window is dedicated and therefore that it will
be deleted.

A non-dedicated window which has displayed another buffer at any
point will not be deleted."
  ;; N.b. This function exists primarily for documentation purposes,
  ;; to clarify the side-effect of using a dedicated window.
  (when (eq leman-room-compose-buffer-window-dedicated 'delete)
    ;; `quit-restore-window' always deletes a dedicated window.
    (set-window-dedicated-p nil t))
  (quit-restore-window nil 'kill))

(declare-function dabbrev--select-buffers "dabbrev")

(defun leman-compose-dabbrev-select-buffers ()
  "Used as `dabbrev-select-buffers-function' in compose buffers."
  (let ((buflist (dabbrev--select-buffers))
        (roombuf (map-elt (leman-room-local leman-room) 'buffer)))
    (if (and roombuf (buffer-live-p roombuf))
        (cons roombuf (delq roombuf buflist))
      buflist)))

(defun leman-room-mode-p (buffer)
  "Non-nil if BUFFER has `leman-room-mode' as its major mode.
Used with `dabbrev-friend-buffer-function'."
  (with-current-buffer buffer
    (derived-mode-p 'leman-room-mode)))

;;; Message history for compose buffers.  Isearch code is derived from comint.el.

(defvar-local leman-room--compose-message-history-index -1)
(defvar-local leman-room--compose-message-history-initial "")
(defvar-local leman-room--compose-history-isearch nil)

(defun leman-room-compose-message-history-insert (hist-pos &optional with-message)
  "Insert text of the absolute history position HIST-POS."
  ;; Store the not-from-history buffer message.
  (when (< leman-room--compose-message-history-index 0)
    (setq leman-room--compose-message-history-initial
          (leman-room-compose-buffer-string-trimmed)))
  ;; Update the index.
  (setq leman-room--compose-message-history-index (or hist-pos -1))
  (when (and with-message hist-pos (>= hist-pos 0))
    (let ((message-log-max nil))
      (message "History item %d" hist-pos)))
  ;; Update the buffer.
  (erase-buffer)
  (insert (if (< leman-room--compose-message-history-index 0)
              leman-room--compose-message-history-initial
            (or (nth leman-room--compose-message-history-index
                     leman-room-message-history)
                (format "[invalid leman message history element %d]"
                        leman-room--compose-message-history-index)))))

(defun leman-room-compose-history-prev-message (arg)
  "Cycle backward through message history, after saving current message.
With a numeric prefix ARG, go back ARG messages."
  (interactive "*p")
  (let ((len (length leman-room-message-history)))
    ;; Valid index values: -1 <= idx < len.
    (cond ((<= len 0)
           (user-error "Empty message history"))
          ((eql arg 0)) ;; No-op.
          ((and (> arg 0) (>= leman-room--compose-message-history-index (1- len)))
           (user-error "Beginning of history; no preceding item"))
          ((and (< arg 0) (< leman-room--compose-message-history-index 0))
           (user-error "End of history; no next item"))
          (t
           ;; It's still possible to move in the specified direction.
           (leman-room-compose-message-history-insert
            (let ((hist-pos (+ arg leman-room--compose-message-history-index)))
              (cond ((>= hist-pos len) (1- len))
                    ((< hist-pos -1) -1)
                    (t hist-pos)))
            :with-message)))))

(defun leman-room-compose-history-next-message (arg)
  "Cycle forward through message history, after saving current message.
With a numeric prefix ARG, go forward ARG messages."
  (interactive "*p")
  (leman-room-compose-history-prev-message (- arg)))

(defun leman-room-compose-history-isearch-backward ()
  "Search for a string in the message history using Isearch.
Use \\[isearch-backward] and \\[isearch-forward] to continue searching."
  (interactive)
  (setq leman-room--compose-history-isearch t)
  (isearch-backward nil t))

(defun leman-room-compose-history-isearch-backward-regexp ()
  "Search for a regular expression in the message history using Isearch.
Use \\[isearch-backward] and \\[isearch-forward] to continue searching."
  (interactive)
  (setq leman-room--compose-history-isearch t)
  (isearch-backward-regexp nil t))

(defun leman-room-compose-history-isearch-setup ()
  "Set up Isearch to search `leman-room-message-history'.
Intended to be added to `isearch-mode-hook' in an leman compose buffer."
  (when (eq leman-room--compose-history-isearch t)
    (setq isearch-message-prefix-add "history ")
    (setq-local isearch-search-fun-function
                #'leman-room-compose-history-isearch-search)
    (setq-local isearch-message-function
                #'leman-room-compose-history-isearch-message)
    (setq-local isearch-wrap-function
                #'leman-room-compose-history-isearch-wrap)
    (setq-local isearch-push-state-function
                #'leman-room-compose-history-isearch-push-state)
    (setq-local isearch-lazy-count nil)
    (add-hook 'isearch-mode-end-hook 'leman-room-compose-history-isearch-end nil t)))

(defun leman-room-compose-history-isearch-end ()
  "Clean up the buffer after terminating Isearch.
Called via `isearch-mode-end-hook'."
  (setq isearch-message-prefix-add nil)
  (setq isearch-search-fun-function 'isearch-search-fun-default)
  (setq isearch-wrap-function nil)
  (setq isearch-push-state-function nil)
  ;; Force isearch to not change mark.
  (setq isearch-opoint (point))
  (kill-local-variable 'isearch-lazy-count)
  (remove-hook 'isearch-mode-end-hook 'leman-room-compose-history-isearch-end t)
  (unless isearch-suspended
    (setq leman-room--compose-history-isearch nil)))

(defun leman-room-compose-history-isearch-search ()
  "Return the search function for Isearch in message history.
This function is used as the value of `isearch-search-fun-function'."
  #'leman-room-compose-history-isearch-function)

(defun leman-room-compose-history-isearch-function (string bound noerror)
  "Isearch in message history."
  (let ((search-fun
	 ;; Use standard functions to search within message text
	 (isearch-search-fun-default))
	found)
    (or
     ;; 1. First try searching in the initial message
     (funcall search-fun string nil noerror)
     ;; 2. If the above search fails, start putting next/prev history elements in the
     ;; buffer successively, and search the string in them.  Do this only when bound is
     ;; nil (i.e. not while lazy-highlighting search strings in the current message).
     (unless bound
       (condition-case nil
	   (progn
	     (while (not found)
	       (if isearch-forward
		   (leman-room-compose-history-next-message 1)
		 (leman-room-compose-history-prev-message 1))
               (goto-char (if isearch-forward (point-min) (point-max)))
	       (setq isearch-barrier (point)
                     isearch-opoint (point))
	       ;; After putting the next/prev history element, search the string in
               ;; them again, until `leman-room-compose-history-next-message' or
	       ;; `leman-room-compose-history-prev-message' raises an error at the
	       ;; beginning/end of history.
	       (setq found (funcall search-fun string nil noerror)))
	     ;; Return point of the new search result.
	     (point))
	 ;; Return nil on any isearch errors, including the "no next/preceding item"
         ;; user-errors signalled from `leman-room-compose-history-prev-message'.
         (error nil))))))

(defun leman-room-compose-history-isearch-message (&optional c-q-hack ellipsis)
  "Display the isearch message.
This function is used as the value of `isearch-message-function'."
  (setq isearch-message-prefix-add
        (if (and isearch-success
                 (not isearch-error)
                 (>= leman-room--compose-message-history-index 0))
            (format "history item %d: "
                    leman-room--compose-message-history-index)
          "history "))
  (isearch-message c-q-hack ellipsis))

(defun leman-room-compose-history-isearch-wrap ()
  "Wrap the history search when search fails.
Move point to the first history element for a forward search,
or to the last history element for a backward search.
This function is used as the value of `isearch-wrap-function'."
  ;; When `leman-room-compose-history-isearch-search' fails on reaching the
  ;; beginning/end of the history, wrap the search to the first/last
  ;; input history element.
  (leman-room-compose-message-history-insert
   (if isearch-forward
       (1- (length leman-room-message-history))
     -1))
  (goto-char (if isearch-forward (point-min) (point-max))))

(defun leman-room-compose-history-isearch-push-state ()
  "Save a function restoring the state of input history search.
Save `leman-room--compose-message-history-index' to the additional state parameter
in the search status stack.
This function is used as the value of `isearch-push-state-function'."
  (let ((index leman-room--compose-message-history-index))
    (lambda (cmd)
      (leman-room-compose-history-isearch-pop-state cmd index))))

(defun leman-room-compose-history-isearch-pop-state (_cmd hist-pos)
  "Restore the input history search state.
Go to the history element by the absolute history position HIST-POS.
See `leman-room-compose-history-isearch-push-state'."
  (leman-room-compose-message-history-insert hist-pos))

;;;;; Widgets

(require 'widget)

(define-widget 'leman-room-membership 'item
  "Widget for membership events."
  ;; FIXME: This makes it hard to add a timestamp according to the buffer's message format spec.
  ;; NOTE: The widget needs something before and after "%v" to correctly apply the
  ;; `leman-room-membership' face. We could use a zero-width space, but that won't work on
  ;; a TTY. So we use a regular space but replace it with nothing with a display spec.
  :format (let ((zws (propertize " " 'display "")))
            (concat "%{" zws "%v" zws "%}"))
  :sample-face 'leman-room-membership
  :value-create (lambda (widget)
                  (pcase-let* ((event (widget-value widget)))
                    (insert (leman-room-wrap-prefix
                              (leman-room--format-member-event event leman-room))))))

(defun leman-room--format-member-event (event room)
  "Return formatted string for \"m.room.member\" EVENT in ROOM."
  ;; SPEC: Section 9.3.4: "m.room.member".
  (pcase-let* (((cl-struct leman-event sender state-key
                           (content (map reason ('avatar_url new-avatar-url)
                                         ('membership new-membership) ('displayname new-displayname)))
                           (unsigned (map ('prev_content (map ('avatar_url old-avatar-url)
                                                              ('membership prev-membership)
                                                              ('displayname prev-displayname))))))
                event)
               (sender-name (leman--user-displayname-in leman-room sender))
               (reason-suffix (if reason
                                  (format " (%S)" reason)
                                "")))
    (cl-macrolet ((nes (var)
                    ;; For "non-empty-string".  Needed because the displayname can be
                    ;; an empty string, but apparently is never null.  (Note that the
                    ;; argument should be a variable, never any other form, to avoid
                    ;; multiple evaluation.)
                    `(when (and ,var (not (string-empty-p ,var)))
                       ,var))
                  (sender-name-id-string ()
                    `(propertize sender-name
                                 'help-echo (leman-user-id sender)))
                  (new-displayname-sender-name-state-key-string ()
                    `(propertize (or (nes new-displayname) (nes sender-name) (nes state-key))
                                 'help-echo state-key))
                  (sender-name-state-key-string ()
                    `(propertize sender-name
                                 'help-echo state-key))
                  (prev-displayname-id-string ()
                    `(propertize (or prev-displayname sender-name)
                                 'help-echo (leman-user-id sender)))
                  (prev-displayname-state-key-string ()
                    ;; The event's previous displayname, or the state key.
                    `(propertize (or prev-displayname state-key)
                                 'help-echo state-key)))
      ;; Not `pcase-exhaustive': valid rooms may have "knock" and
      ;; "knock_restricted" memberships (rooms v8+), and a malformed
      ;; event could have a nil or garbage membership; render those
      ;; instead of crashing the buffer's rendering.
      (pcase new-membership
        ("invite"
         (pcase prev-membership
           ((or "leave" '())
            (format "%s invited %s"
                    (sender-name-id-string)
                    (new-displayname-sender-name-state-key-string)))
           (_ (format "%s sent unrecognized invite event for %s"
                      (sender-name-id-string)
                      (new-displayname-sender-name-state-key-string)))))
        ("join"
         (pcase prev-membership
           ("invite"
            (format "%s accepted invitation to join"
                    (sender-name-state-key-string)))
           ("join"
            (cond ((not (equal new-displayname prev-displayname))
                   (propertize (format "%s changed name to %s"
                                       prev-displayname (or new-displayname (leman--user-displayname-in room sender)))
                               'help-echo state-key))
                  ((not (equal new-avatar-url old-avatar-url))
                   (format "%s changed avatar"
                           (new-displayname-sender-name-state-key-string)))
                  (t (format "Unrecognized membership event for %s"
                             (sender-name-state-key-string)))))
           ("leave"
            (format "%s rejoined"
                    (sender-name-state-key-string)))
           (`nil
            (format "%s joined"
                    (new-displayname-sender-name-state-key-string)))
           (_ (format "%s sent unrecognized join event for %s"
                      (sender-name-id-string)
                      (new-displayname-sender-name-state-key-string)))))
        ("leave"
         (pcase prev-membership
           ("invite"
            (pcase state-key
              ((pred (equal (leman-user-id sender)))
               (format "%s rejected invitation"
                       (sender-name-id-string)))
              (_ (format "%s revoked %s's invitation"
                         (sender-name-id-string)
                         (new-displayname-sender-name-state-key-string)))))
           ("join"
            (pcase state-key
              ((pred (equal (leman-user-id sender)))
               (format "%s left%s"
                       (prev-displayname-id-string)
                       reason-suffix))
              (_ (format "%s kicked %s%s"
                         (sender-name-id-string)
                         (prev-displayname-state-key-string)
                         reason-suffix))))
           ("ban"
            (format "%s unbanned %s"
                    (sender-name-id-string)
                    state-key))
           (_ (format "%s left%s"
                      (prev-displayname-id-string)
                      reason-suffix))))
        ("ban"
         (pcase prev-membership
           ((or "invite" "leave")
            (format "%s banned %s%s"
                    (sender-name-id-string)
                    (prev-displayname-state-key-string)
                    reason-suffix))
           ("join"
            (format "%s kicked and banned %s%s"
                    (sender-name-id-string)
                    (prev-displayname-state-key-string)
                    reason-suffix))
           (_ (format "%s sent unrecognized ban event for %s"
                      (sender-name-id-string)
                      (prev-displayname-state-key-string)))))
        (_
         ;; "knock" and "knock_restricted" (rooms v8+), or garbage:
         ;; render, don't crash.
         (format "%s sent unrecognized membership event for %s"
                 (sender-name-id-string)
                 (new-displayname-sender-name-state-key-string)))))))
;; NOTE: Widgets are only currently used for single membership events, not grouped ones.

(defun leman-room--pair-events (events others)
  "Pair each event in EVENTS with one in OTHERS by state-key.
Return a list of three lists: the paired OTHERS events, the
EVENTS that had no pair, and the OTHERS that had no pair.  Each
OTHERS event is paired at most once; once an OTHERS event has
been paired, its state-key is consumed, so other OTHERS and
EVENTS events having that state-key are dropped."
  (let ((paired-others nil)
        (remaining-events nil)
        (remaining-others (copy-sequence others))
        (paired-state-keys nil))
    (dolist (event events)
      (let ((state-key (leman-event-state-key event))
            (other (cl-find (leman-event-state-key event) remaining-others
                            :test #'equal :key #'leman-event-state-key)))
        (cond (other
               (push other paired-others)
               (push state-key paired-state-keys)
               (setf remaining-others (cl-delete state-key remaining-others
                                                 :test #'equal :key #'leman-event-state-key)))
              ((not (member state-key paired-state-keys))
               (push event remaining-events)))))
    (list (nreverse paired-others) (nreverse remaining-events) remaining-others)))

(defun leman-room--membership-summary-type (event)
  "Return the summary type for \"m.room.member\" EVENT's membership change.
Return nil when the event should not be shown in a summary."
  (let ((old (map-nested-elt (leman-event-unsigned event) '(prev_content membership)))
        (new (alist-get 'membership (leman-event-content event))))
    (cond ((equal new "join")
           (cond ((equal old "join")
                  (if (not (equal (alist-get 'avatar_url (leman-event-content event))
                                  (map-nested-elt (leman-event-unsigned event)
                                                  '(prev_content avatar_url))))
                      "changed avatar"
                    "changed name"))
                 ((equal old "leave") "rejoined")
                 (t "joined")))
          ((equal new "leave")
           (cond ((equal old "ban") "unbanned")
                 ((equal old "invite") "rejected invitation")
                 (t "left")))
          ((equal new "invite") "invited")
          ((equal new "ban")
           (cond ((equal old "join") "kicked and banned")
                 ((member old '("invite" "leave")) "banned"))))))

(defun leman-room--membership-kicked-p (event)
  "Return non-nil if \"m.room.member\" EVENT is a kick.
I.e. the user was kicked by another user, rather than leaving on
their own."
  (and (equal "join" (map-nested-elt (leman-event-unsigned event) '(prev_content membership)))
       (equal "leave" (alist-get 'membership (leman-event-content event)))
       (not (equal (leman-user-id (leman-event-sender event))
                   (leman-event-state-key event)))))

(defun leman-room--format-membership-summary (events room)
  "Return summary string for \"m.room.member\" EVENTS in ROOM.
EVENTS should contain more than one event."
  (cl-labels ((event-user (event)
                (propertize (if-let (user (gethash (leman-event-state-key event) leman-users))
                                (leman--user-displayname-in room user)
                              (leman-event-state-key event))
                            'help-echo (concat (leman-room--format-member-event event room)
                                               " <" (leman-event-state-key event) ">")))
              (state-key-in (events)
                (lambda (event)
                  (cl-find (leman-event-state-key event) events
                           :test #'equal :key #'leman-event-state-key))))
    (let* ((kicked-events (cl-remove-if-not #'leman-room--membership-kicked-p events))
           (buckets (let (buckets)
                      (dolist (event events)
                        (when-let ((type (leman-room--membership-summary-type event)))
                          (push event (alist-get type buckets nil nil #'equal))))
                      ;; The buckets were built by pushing, which
                      ;; reverses the events' order; restore it.
                      (cl-loop for (type . bucket-events) in buckets
                               collect (cons type (nreverse bucket-events)))))
           (rejoin-events (alist-get "rejoined" buckets nil nil #'equal))
           (join-events (alist-get "joined" buckets nil nil #'equal))
           (left-events (alist-get "left" buckets nil nil #'equal))
           ;; Events that are both joined and rejoined are counted as rejoined.
           (join-events (cl-delete-if (state-key-in rejoin-events) join-events)))
      ;; Joins followed by a leave are counted as "joined and left"; rejoins
      ;; following a kick are counted as "was kicked and rejoined", and the
      ;; paired kicks are removed from the left events, in which they would
      ;; otherwise be counted as merely leaving; and remaining rejoins
      ;; followed by a leave are counted as "rejoined and left".
      (pcase-let* ((`(,joined-and-left-events ,join-events ,left-events)
                    (leman-room--pair-events join-events left-events))
                   (`(,kicked-and-rejoined-events ,rejoin-events _)
                    (leman-room--pair-events rejoin-events kicked-events))
                   (`(,rejoined-and-left-events ,rejoin-events ,left-events)
                    (leman-room--pair-events
                     rejoin-events
                     (cl-delete-if (state-key-in kicked-and-rejoined-events)
                                   left-events))))
        (format "Membership: %s."
                (string-join
                 (cl-loop for (type . events)
                          in (leman-alist "rejoined" rejoin-events
                                          "joined" join-events
                                          "left" left-events
                                          "joined and left" joined-and-left-events
                                          "was kicked and rejoined" kicked-and-rejoined-events
                                          "rejoined and left" rejoined-and-left-events
                                          "invited" (alist-get "invited" buckets nil nil #'equal)
                                          "rejected invitation" (alist-get "rejected invitation" buckets nil nil #'equal)
                                          "banned" (alist-get "banned" buckets nil nil #'equal)
                                          "unbanned" (alist-get "unbanned" buckets nil nil #'equal)
                                          "kicked and banned" (alist-get "kicked and banned" buckets nil nil #'equal)
                                          "changed name" (alist-get "changed name" buckets nil nil #'equal)
                                          "changed avatar" (alist-get "changed avatar" buckets nil nil #'equal))
                          for users = (mapcar #'event-user
                                              (cl-delete-duplicates
                                               events :key #'leman-event-state-key))
                          when events
                          collect (format "%s %s (%s)" (length users)
                                          (propertize type 'face 'bold)
                                          (string-join users ", ")))
                 "; "))))))

(defun leman-room--format-membership-events (struct room)
  "Return string for STRUCT in ROOM.
STRUCT should be an `leman-room-membership-events' struct."
  (pcase-let* (((cl-struct leman-room-membership-events events) struct))
    (pcase (length events)
      (0 (warn "No events in `leman-room-membership-events' struct"))
      (1 (leman-room--format-member-event (car events) room))
      (_ (leman-room--format-membership-summary events room)))))

;;;;; Images

;; Downloading and displaying images in messages, room/user avatars, etc.

(require 'image)

(defvar leman-room-image-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map image-map)
    (define-key map (kbd "M-RET") #'leman-room-image-scale)
    (define-key map (kbd "RET") #'leman-room-image-show)
    (define-key map [mouse-1] #'leman-room-image-scale-mouse)
    (define-key map [double-mouse-1] #'leman-room-image-show-mouse)
    map)
  "Keymap for images in room buffers.")

(defgroup leman-room-images nil
  "Showing images in rooms."
  :group 'leman-room)

(defcustom leman-room-images t
  "Download and show images in messages, avatars, etc."
  :type 'boolean
  :set (lambda (option value)
         (if (or (fboundp 'imagemagick-types)
                 (when (fboundp 'image-transforms-p)
                   (image-transforms-p)))
             (set-default option value)
           (set-default option nil)
           (when (and value (display-images-p))
             (display-warning 'leman "This Emacs was not built with ImageMagick support, nor does it support Cairo/XRender scaling, so images can't be displayed in Leman")))))

(defcustom leman-room-image-thumbnail-height 0.2
  "Scale thumbnail images to this multiple of the window body height.
Should be a number between 0 and 1.
See also `leman-room-image-thumbnail-height-min'."
  :type '(number :tag "Multiple of the window body height"))

(defcustom leman-room-image-thumbnail-height-min 30
  "Minimum height in pixels when scaling thumbnail images.
See also `leman-room-image-thumbnail-height'."
  :type 'natnum)

(defcustom leman-room-image-initial-height leman-room-image-thumbnail-height
  "Limit images' initial display height.
If a number, it should be no larger than 1 (because Emacs can't
display images larger than the window body height)."
  :type '(choice (const :tag "Use full window height (or width)" nil)
                 (number :tag "Multiple of the window body height")))

(defcustom leman-room-image-margin 5
  "How many pixels to add as an extra margin around the image."
  :type 'natnum)

(defcustom leman-room-image-relief 2
  "Width in pixels of shadow rectangle around the image.
If negative, shadows are drawn so that the image appears as a
pressed button; otherwise, it appears as an unpressed button."
  :type 'integer)

(defun leman-room-image-scale-mouse (event)
  "Toggle scale of image at mouse EVENT.
Scale image to fit within the window's body.  If image is already
fit to the window, reduce its max-height to 10% of the window's
height."
  (interactive "e")
  (let* ((pos (event-start event))
         (window (posn-window pos)))
    (with-selected-window window
      (leman-room-image-scale (posn-point pos)))))

(defun leman-room-image-scale (pos)
  "Toggle scale of image at POS.
Scale image to fit the window body.  If the image already fits
the window body, reduce its max-height in accordance with user
options `leman-room-image-thumbnail-height' and
`leman-room-image-thumbnail-height-min'."
  (interactive "d")
  (pcase-let* ((image (get-text-property pos 'display))
               (max-height (image-property image :max-height))
               (xy (posn-x-y (posn-at-point pos)))
               (window-width (window-body-width nil t))
               (max-width (- window-width (car xy)))
               (window-height (window-body-height nil t))
               (use-window-body-size (not (and (numberp max-height)
                                               (= window-height max-height))))
               ;; Image scaling commands set :max-height and friends to nil.
               (new-height (if use-window-body-size
                               window-height
                             (max leman-room-image-thumbnail-height-min
                                  ;; Emacs doesn't like floats as the max-height.
                                  (truncate (* window-height
                                               leman-room-image-thumbnail-height))))))
    (when (fboundp 'imagemagick-types)
      ;; Only do this when ImageMagick is supported.
      ;; FIXME: When requiring Emacs 27+, remove this (I guess?).
      (setf (image-property image :type) 'imagemagick))
    ;; Set :scale to nil since image scaling commands might have changed it.
    (setf (image-property image :scale) nil
          (image-property image :max-width) max-width
          (image-property image :max-height) new-height)
    ;; When maximising, eliminate all padding around the image, so that the line
    ;; height will not exceed the window height.  This prevents window scrolling
    ;; issues.  Set the window start to ensure the image is displayed in full.
    (if use-window-body-size
        (setf (image-property image :relief) nil
              (image-property image :margin) nil
              (window-start) pos)
      (setf (image-property image :relief) leman-room-image-relief
            (image-property image :margin) leman-room-image-margin))))

(defun leman-room-image-show-mouse (event)
  "Show image at mouse EVENT in a new buffer."
  (interactive "e")
  (let* ((pos (event-start event))
         (window (posn-window pos)))
    (with-selected-window window
      (leman-room-image-show (posn-point pos)))))

(defun leman-room-image-show (pos)
  "Show image at POS in a new buffer."
  (interactive "d")
  (pcase-let* ((image (copy-sequence (get-text-property pos 'display)))
               (leman-event (ewoc-data (ewoc-locate leman-ewoc pos)))
               ((cl-struct leman-event id) leman-event)
               (buffer-name (format "*Leman image: %s*" id)))
    (when (fboundp 'imagemagick-types)
      ;; Only do this when ImageMagick is supported.
      ;; FIXME: When requiring Emacs 27+, remove this (I guess?).
      (setf (image-property image :type) 'imagemagick))
    (setf (image-property image :scale) 1.0
          (image-property image :max-width) nil
          (image-property image :max-height) nil)
    (unless (get-buffer buffer-name)
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (insert-image image)
        (image-mode)))
    (pop-to-buffer buffer-name
                   '((display-buffer-pop-up-frame
                      (pop-up-frame-parameters . ((fullscreen . t) (maximized . t))))))))

(cl-defun leman-room--image-download (event session &key then else (authenticatedp t))
  "Download image EVENT on SESSION and call THEN, else ELSE.
If AUTHENTICATEDP, send authenticated request to new
endpoint (Matrix 1.11, MSC3911); otherwise send old-style,
unauthenticated request to old endpoint.  Prefers the event's
thumbnail, if any, for speed (the full-size image can be very
large; use `leman-room-image-show' to view it)."
  (declare (indent defun))
  (pcase-let* (((cl-struct leman-event content) event)
               ((map ('url mxc)) content)
               ((map ('info (map ('thumbnail_url thumbnail-mxc)))) content)
               (mxc (or thumbnail-mxc mxc)))
    (leman--media-request mxc session :then then :else else
      :queue leman-images-queue :authenticatedp authenticatedp)))

(defun leman-room--m.image-download-error (event session then plz-error)
  "Handle PLZ-ERROR from a failed request to download EVENT's image on SESSION.
THEN is the success continuation, to which the request may be
retried unauthenticated if the server returns M_UNRECOGNIZED."
  ;; Allow a later re-rendering (e.g. after a network failure is
  ;; resolved) to retry the download.
  (setf (map-elt (leman-event-local event) 'image-downloading) nil)
  (pcase-let* (((cl-struct plz-error response
                           (message plz-message)
                           (curl-error `(,curl-exit-code . ,curl-message)))
                plz-error)
               (status (when (plz-response-p response)
                         (plz-response-status response)))
               (body (when (plz-response-p response)
                       (plz-response-body response)))
               (json-object (when body
                              (ignore-errors
                                (json-read-from-string body))))
               (errcode (alist-get 'errcode json-object))
               (error-message (format "%S: %s"
                                      (or curl-exit-code status)
                                      (or (when json-object
                                            (alist-get 'error json-object))
                                          curl-message
                                          plz-message))))
    (pcase errcode
      ("M_UNRECOGNIZED"
       ;; Resend unauthenticated media request for older servers.
       ;; FIXME: Test the "/versions" endpoint to see what's supported.  See
       ;; <https://matrix.org/blog/2024/06/20/matrix-v1.11-release/>.
       (leman-room--image-download event session :authenticatedp nil
         :then then))
      (_ (signal 'leman-api-error (list error-message))))))

(defun leman-room--image-max-sizes (buffer-window)
  "Return (MAX-HEIGHT . MAX-WIDTH) for an image shown in BUFFER-WINDOW.
Uses user options `leman-room-image-initial-height' and
`leman-room-image-thumbnail-height-min'; if BUFFER-WINDOW is nil,
uses the frame's size."
  (let (max-height max-width)
    (cond (leman-room-image-initial-height
           ;; Use configured value.
           (setf max-height (max leman-room-image-thumbnail-height-min
                                 ;; Emacs doesn't like floats as the max-height.
                                 (truncate
                                  (* (window-body-height buffer-window t)
                                     leman-room-image-initial-height)))
                 max-width (window-body-width buffer-window t)))
          (buffer-window
           ;; Buffer displayed: use window size.
           (setf max-height (window-body-height buffer-window t)
                 max-width (window-body-width buffer-window t)))
          (t
           ;; Buffer not displayed: use frame size.
           (setf max-height (frame-pixel-height)
                 max-width (frame-pixel-width))))
    (cons max-height max-width)))

(defun leman-room--format-m.image (event session)
  "Return \"m.image\" EVENT on SESSION formatted as a string.
When `leman-room-images' is non-nil, also download it and then
show it in the buffer."
  (pcase-let* (((cl-struct leman-event (local event-local)) event)
               ;; HACK: Get the room's buffer from the variable (the current buffer
               ;; will be a temp formatting buffer when this is called, but it still
               ;; inherits the `leman-room' variable from the room buffer, thankfully).
               ((cl-struct leman-room local) leman-room)
               ((map buffer) local)
               ;; TODO: Thumbnail support.
               ((map image) event-local)
               (then (apply-partially #'leman-room--m.image-callback event leman-room))
               (else (apply-partially #'leman-room--m.image-download-error
                                      event session then)))
    (if (and leman-room-images image)
        ;; Images enabled and image downloaded: create image and
        ;; return it in a string.
        (condition-case err
            (let ((image (create-image image nil 'data-p :ascent 'center))
                  (buffer-window (when buffer
                                   (get-buffer-window buffer))))
              (pcase-let ((`(,max-height . ,max-width)
                           (leman-room--image-max-sizes buffer-window)))
                (when (and (fboundp 'imagemagick-types)
                           ;; ImageMagick rescales every frame of an
                           ;; animated image on each display, which is
                           ;; very slow; use it only for static images.
                           (not (cdr (image-multi-frame-p image))))
                  ;; Only do this when ImageMagick is supported.
                  ;; FIXME: When requiring Emacs 27+, remove this (I guess?).
                  (setf (image-property image :type) 'imagemagick))
                (setf (image-property image :max-width) max-width
                      (image-property image :max-height) max-height
                      (image-property image :relief) leman-room-image-relief
                      (image-property image :margin) leman-room-image-margin
                      (image-property image :pointer) 'hand)
                (concat "\n"
                        (leman-room-wrap-prefix " "
                          'display image
                          'keymap leman-room-image-keymap))))
          (error (format "\n [error inserting image: %s]" (error-message-string err))))
      ;; Image not downloaded: insert URL as button, and download if enabled.
      (prog1
          (leman-room-wrap-prefix "[image]"
            'action (apply-partially #'apply #'leman-room--image-download)
            'button t
            'button-data (list event session
                               :then (lambda (&rest args)
                                       ;; Bind non-nil to force the image to be displayed.
                                       (let ((leman-room-images t))
                                         (apply then args)))
                               :else else)
            'category t
            'face 'button
            'follow-link t
            'help-echo "Show image"
            'keymap button-map
            'mouse-face 'highlight)
        (when (and leman-room-images
                   (not (map-elt (leman-event-local event) 'image-downloading)))
          ;; Images enabled: download it, unless a download is
          ;; already in flight.  Re-rendering the event while the
          ;; image is being downloaded (e.g. when the buffer is
          ;; re-created or the event is invalidated) must not start
          ;; another download.
          (setf (map-elt (leman-event-local event) 'image-downloading) t)
          (leman-room--image-download event session
            :then then :else else))))))
(defun leman-room--m.image-callback (event room data)
  "Add downloaded image from DATA to EVENT in ROOM.
Then invalidate EVENT's node to show the image."
  (setf (map-elt (leman-event-local event) 'image) data
        ;; The download has finished, so allow re-rendering to show
        ;; the image without starting another download.
        (map-elt (leman-event-local event) 'image-downloading) nil)
  (leman-room--invalidate-event-node event room))

(defun leman-room--format-m.file (event)
  "Return \"m.file\" EVENT formatted as a string."
  ;; TODO: Insert thumbnail images when enabled.
  (pcase-let* (((cl-struct leman-event
                           (content (map body
                                         filename
                                         ('info (map mimetype size))
                                         ('url mxc-url))))
                event)
               ;; The spec now prefers "filename", but older events
               ;; only have "body".
               (filename (or filename body))
               (human-size (when size
                             (file-size-human-readable size)))
               (string (concat "[file: " filename
                               (when mimetype
                                 (format " (%s)" mimetype))
                               (when human-size
                                 (format " (%s)" human-size))
                               "]")))
    (concat (propertize string
                        'action #'call-interactively
                        'button t
                        'button-data #'leman-room-download-file
                        'category t
                        'face 'button
                        'follow-link t
                        'help-echo mxc-url
                        'keymap button-map
                        'mouse-face 'highlight)
            (propertize " "
                        'display '(space :relative-height 1.5)))))

(defun leman-room--format-m.video (event)
  "Return \"m.video\" EVENT formatted as a string."
  ;; TODO: Insert thumbnail images when enabled.
  (pcase-let* (((cl-struct leman-event
                           (content (map body
                                         ('info (map mimetype size w h))
                                         ('url mxc-url))))
                event)
               (human-size (when size
                             (file-size-human-readable size)))
               (string (concat "[video: " body
                               (when mimetype
                                 (format " (%s)" mimetype))
                               (when (and w h)
                                 (format " (%sx%s)" w h))
                               (when human-size
                                 (format " (%s)" human-size))
                               "]")))
    (concat (propertize string
                        'action #'call-interactively
                        'button t
                        'button-data #'leman-room-download-file
                        'category t
                        'face 'button
                        'follow-link t
                        'help-echo mxc-url
                        'keymap button-map
                        'mouse-face 'highlight)
            (propertize " "
                        'display '(space :relative-height 1.5)))))

(defun leman-room--format-m.audio (event)
  "Return \"m.audio\" EVENT formatted as a string."
  (pcase-let* (((cl-struct leman-event
                           (content (map body
                                         ('info (map mimetype duration size))
                                         ('url mxc-url))))
                event)
               (human-size (when size
                             (file-size-human-readable size)))
               (human-duration (when (> (or duration 0) 0)
                                 (format-seconds "%m:%s" (/ duration 1000))))
               (string (concat "[audio: " body
                               (when mimetype
                                 (format " (%s)" mimetype))
                               (when human-duration
                                 (format " (%s)" human-duration))
                               (when human-size
                                 (format " (%s)" human-size))
                               "]")))
    (concat (propertize string
                        'action #'leman-room-download-file
                        'button t
                        'button-data event
                        'category t
                        'face 'button
                        'follow-link t
                        'help-echo mxc-url
                        'keymap button-map
                        'mouse-face 'highlight)
            (propertize " "
                        'display '(space :relative-height 1.5)))))

;;;;; Org format sending

;; Some of these declarations may need updating as Org changes.

(defvar org-export-with-toc)
(defvar org-export-with-broken-links)
(defvar org-export-with-section-numbers)
(defvar org-export-with-sub-superscripts)
(defvar org-html-inline-images)

(declare-function org-elleman-property "org-element")
(declare-function org-export-data "ox")
(declare-function org-export-get-caption "ox")
(declare-function org-export-get-ordinal "ox")
(declare-function org-export-get-reference "ox")
(declare-function org-export-read-attribute "ox")
(declare-function org-html--has-caption-p "ox-html")
(declare-function org-html--textarea-block "ox-html")
(declare-function org-html--translate "ox-html")
(declare-function org-html-export-as-html "ox-html")
(declare-function org-html-format-code "ox-html")

(defun leman-room-compose-org ()
  "Activate `org-mode' in current compose buffer.
Configures the buffer appropriately so that saving it will export
the Org buffer's contents."
  (interactive)
  (unless leman-room-compose-buffer
    (user-error "This command should be run in a compose buffer.  Use `leman-room-compose-message' first"))
  ;; Calling `org-mode' seems to wipe out local variables.
  (let ((room leman-room)
        (session leman-session))
    (org-mode)
    (leman-room-init-compose-buffer room session))
  (setq-local leman-room-send-message-filter #'leman-room-send-org-filter))

(defun leman-room-send-org-filter (content room)
  "Return event CONTENT for ROOM having processed its Org content.
The CONTENT's body is exported with
`org-html-export-as-html' (with some adjustments for
compatibility), and the result is added to the CONTENT as
\"formatted_body\"."
  (require 'ox-html)
  ;; The CONTENT alist has string keys before being sent.
  (pcase-let* ((body (alist-get "body" content nil nil #'equal))
               (formatted-body
                (save-window-excursion
                  (with-temp-buffer
                    (insert (leman--format-body-mentions body room
                              :template "[[https://matrix.to/#/%s][%s]]"))
                    (cl-letf (((symbol-function 'org-html-src-block)
                               (symbol-function 'leman-room--org-html-src-block)))
                      (let ((org-export-with-toc nil)
                            (org-export-with-broken-links t)
                            (org-export-with-section-numbers nil)
                            (org-export-with-sub-superscripts nil)
                            (org-html-inline-images nil)
                            (display-buffer-alist (cons '("^\\*Org HTML Export\\*$"
                                                          . (display-buffer-no-window nil))
                                                        display-buffer-alist)))
                        (org-html-export-as-html nil nil nil 'body-only)))
                    (with-current-buffer "*Org HTML Export*"
                      (prog1 (string-trim (buffer-string))
                        (kill-buffer)))))))
    (setf (alist-get "formatted_body" content nil nil #'equal) formatted-body
          (alist-get "format" content nil nil #'equal) "org.matrix.custom.html")
    content))

(defun leman-room--org-html-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information.

This is a copy of `org-html-src-block' that uses Riot
Web-compatible HTML output, using HTML like:

<pre><code class=\"language-python\">..."
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let ((lang (pcase (org-elleman-property :language src-block)
                  ;; Riot's syntax coloring doesn't support "elisp", but "lisp" works.
                  ("elisp" "lisp")
                  (else else)))
	  (code (org-html-format-code src-block info))
	  (label (let ((lbl (and (org-elleman-property :name src-block)
				 (org-export-get-reference src-block info))))
		   (if lbl (format " id=\"%s\"" lbl) ""))))
      (if (not lang) (format "<pre class=\"example\"%s>\n%s</pre>" label code)
	(format "<div class=\"org-src-container\">\n%s%s\n</div>"
		;; Build caption.
		(let ((caption (org-export-get-caption src-block)))
		  (if (not caption) ""
		    (let ((listing-number
			   (format
			    "<span class=\"listing-number\">%s </span>"
			    (format
			     (org-html--translate "Listing %d:" info)
			     (org-export-get-ordinal
			      src-block info nil #'org-html--has-caption-p)))))
		      (format "<label class=\"org-src-name\">%s%s</label>"
			      listing-number
			      (string-trim (org-export-data caption info))))))
		;; Contents.
		(format "<pre><code class=\"src language-%s\"%s>%s</code></pre>"
			lang label code))))))

;;;;; Completion

;; Completing member and room names.

(defun leman-room--complete-members-at-point ()
  "Complete member names and IDs at point.
Uses members in the current buffer's room.  For use in
`completion-at-point-functions'."
  (let ((beg (save-excursion
               (when (re-search-backward (rx (or bol bos blank)) nil t)
                 (skip-syntax-forward "-")
                 (point))))
        (end (point))
        (collection-fn (completion-table-dynamic
                        ;; The manual seems to show the FUN ignoring any
                        ;; arguments, but the `completion-table-dynamic' docstring
                        ;; seems to say that it should use the argument.
                        (lambda (_ignore)
                          (leman-room--member-names-and-ids)))))
    (when beg
      (list beg end collection-fn :exclusive 'no))))

(defun leman-room--complete-rooms-at-point ()
  "Complete room aliases and IDs at point.
For use in `completion-at-point-functions'."
  (let ((beg (save-excursion
               (when (re-search-backward (rx (or bol bos blank) (or "!" "#")) nil t)
                 (skip-syntax-forward "-")
                 (point))))
        (end (point))
        (collection-fn (completion-table-dynamic
                        ;; The manual seems to show the FUN ignoring any
                        ;; arguments, but the `completion-table-dynamic' docstring
                        ;; seems to say that it should use the argument.
                        (lambda (_ignore)
                          (leman-room--room-aliases-and-ids)))))
    (when beg
      (list beg end collection-fn :exclusive 'no))))

;; TODO: Use `cl-pushnew' in these two functions instead of `delete-dups'.

(defun leman-room--member-names-and-ids ()
  "Return a list of member names and IDs seen in current room.
If room's `members' table is filled, use it; otherwise, fetch
members list and return already-seen members instead.  For use in
`completion-at-point-functions'."
  ;; For now, we just collect a list of members from events we've seen.
  ;; TODO: In the future, we may maintain a per-room table of members, which
  ;; would be more suitable for completing names according to the spec.
  (pcase-let* ((room (if (minibufferp)
                         (buffer-local-value
                          'leman-room (window-buffer (minibuffer-selected-window)))
                       leman-room))
               (session (if (minibufferp)
                            (buffer-local-value
                             'leman-session (window-buffer (minibuffer-selected-window)))
                          leman-session))
               ((cl-struct leman-room members) room)
               (members (if (alist-get 'fetched-members-p (leman-room-local room))
                            (hash-table-values members)
                          ;; HACK: Members table empty: update list and use known events
                          ;; for now.
                          (leman-singly (alist-get 'getting-members-p (leman-room-local room))
                            (leman--get-joined-members room session
                              :then (lambda (_) (setf (alist-get 'getting-members-p (leman-room-local room)) nil))
                              :else (lambda (_) (setf (alist-get 'getting-members-p (leman-room-local room)) nil))))
                          (mapcar #'leman-event-sender
                                  (leman-room-timeline leman-room)))))
    (delete-dups
     (cl-loop for member in members
              collect (leman-user-id member)
              collect (leman--user-displayname-in room member)))))

(defun leman-room--room-aliases-and-ids ()
  "Return a list of room names and aliases seen in current session.
For use in `completion-at-point-functions'."
  (let* ((session (if (minibufferp)
                      (buffer-local-value
                       'leman-session (window-buffer (minibuffer-selected-window)))
                    leman-session)))
    (delete-dups
     (delq nil (cl-loop for room in (leman-session-rooms session)
                        collect (leman-room-id room)
                        collect (leman-room-canonical-alias room))))))

;;;;; Transient

(require 'transient)

(transient-define-prefix leman-room-transient ()
  "Transient for Leman Room buffers."
  [:pad-keys t
             ["Movement"
              ("TAB" "Next event" leman-room-goto-next)
              ("<backtab>" "Previous event" leman-room-goto-prev)
              ("SPC" "Scroll up and mark read" leman-room-scroll-up-mark-read)
              ("S-SPC" "Scroll down" leman-room-scroll-down-command)
              ("M-SPC" "Jump to fully-read marker" leman-room-goto-fully-read-marker)
              ("m" "Move read markers to point" leman-room-mark-read)]
             ["Switching"
              ("M-g M-l" "List rooms" leman-room-list)
              ("M-g M-r" "Switch to other room" leman-view-room)
              ("M-g M-m" "Switch to mentions buffer" leman-notify-switch-to-mentions-buffer)
              ("M-g M-n" "Switch to notifications buffer" leman-notify-switch-to-notifications-buffer)
              ("q" "Quit window" quit-window)]]
  [:pad-keys t
             ["Messages"
              ("c" "Composition format" leman-room-set-composition-format
               :description (lambda ()
                              (concat "Composition format: "
                                      (propertize (car (cl-rassoc leman-room-send-message-filter
                                                                  (list (cons "Plain-text" nil)
                                                                        (cons "Org-mode" 'leman-room-send-org-filter))
                                                                  :test #'equal))
                                                  'face 'transient-value))))
              ("RET" "Write message" leman-room-dispatch-new-message)
              ("M-RET" "Write message (alternative)" leman-room-dispatch-new-message-alt)
              ("S-<return>" "Write reply" leman-room-dispatch-reply-to-message)
              ("<insert>" "Edit message" leman-room-dispatch-edit-message)
              ("C-k" "Delete message" leman-room-delete-message)
              ("s r" "Send reaction" leman-room-send-reaction)
              ("s e" "Send emote" leman-room-send-emote)
              ("s f" "Send file" leman-room-send-file)
              ("s i" "Send image" leman-room-send-image)
              ("D" "Download event media" leman-room-download-file)]
             ["Users"
              ("u RET" "Send direct message" leman-send-direct-message)
              ("u i" "Invite user" leman-invite-user)
              ("u I" "Ignore user" leman-ignore-user)]]
  [:pad-keys t
             ["Room"
              ("M-s o" "Occur search in room" leman-room-occur)
              ("r d" "Describe room" leman-describe-room)
              ("r m" "List members" leman-list-members)
              ("r t" "Set topic" leman-room-set-topic)
              ("r f" "Set message format" leman-room-set-message-format)
              ("r N" "Override name" leman-room-override-name
               :description (lambda ()
                              (format "Name override: %s"
                                      (if-let* ((event (alist-get "org.matrix.msc3015.m.room.name.override"
                                                                  (leman-room-account-data leman-room) nil nil #'equal))
                                                (name (map-nested-elt event '(content name))))
                                          (propertize name 'face 'transient-value)
                                        (propertize "none" 'face 'transient-inactive-value)))))
              ("r n" "Set notification state" leman-room-set-notification-state
               :description (lambda ()
                              (let ((state (leman-room-notification-state leman-room leman-session)))
                                (format "Notifications (%s|%s|%s|%s|%s)"
                                        (propertize "default"
                                                    'face (pcase state
                                                            (`nil 'transient-value)
                                                            (_ 'transient-inactive-value)))
                                        (propertize "all-loud"
                                                    'face (pcase state
                                                            ('all-loud 'transient-value)
                                                            (_ 'transient-inactive-value)))
                                        (propertize "all"
                                                    'face (pcase state
                                                            ('all 'transient-value)
                                                            (_ 'transient-inactive-value)))
                                        (propertize "mentions"
                                                    'face (pcase state
                                                            ('mentions-and-keywords 'transient-value)
                                                            (_ 'transient-inactive-value)))
                                        (propertize "none"
                                                    'face (pcase state
                                                            ('none 'transient-value)
                                                            (_ 'transient-inactive-value)))))))
              ("r T" "Tag/untag room" leman-tag-room
               :description (lambda ()
                              (format "Tag/untag room (%s|%s)"
                                      (propertize "Fav"
                                                  'face (if (leman--room-tagged-p "m.favourite" leman-room)
                                                            'transient-value 'transient-inactive-value))
                                      (propertize "Low-prio"
                                                  'face (if (leman--room-tagged-p "m.lowpriority" leman-room)
                                                            'transient-value 'transient-inactive-value)))))]
             ["Room membership"
              ("R c" "Create room" leman-create-room)
              ("R j" "Join room" leman-join-room)
              ("R l" "Leave room" leman-leave-room)
              ("R F" "Forget room" leman-forget-room)
              ("R n" "Set nick" leman-room-set-display-name
               :description (lambda ()
                              (format "Set nick (%s)"
                                      (propertize (leman--user-displayname-in
                                                   leman-room (gethash (leman-user-id (leman-session-user leman-session))
                                                                       leman-users))
                                                  'face 'transient-value))))
              ("R s" "Toggle spaces" leman-room-toggle-space
               :description (lambda ()
                              (format "Toggle spaces (%s)"
                                      (if-let ((spaces (leman--room-spaces leman-room leman-session)))
                                          (string-join
                                           (mapcar (lambda (space)
                                                     (propertize (leman-room-display-name space)
                                                                 'face 'transient-value))
                                                   spaces)
                                           ", ")
                                        (propertize "none" 'face 'transient-inactive-value)))))]]
  ["Other"
   ("v" "View event" leman-room-view-event)
   ("g" "Sync new messages" leman-room-sync
    :if (lambda ()
          (interactive)
          (or (not leman-auto-sync)
              (not (map-elt leman-syncs leman-session)))))])

;;;; Browsing URLs, EWW

(defun leman-room-browse-mxc (mxc)
  ;; TODO: If prefix arg, prompt for destination and download to file.
  "Browse MXC URL on current `leman-session'."
  ;; For authenticated media, we have to provide our own version of `eww-retrieve'.
  (let ((session leman-session))
    (cl-letf (((symbol-function 'eww-retrieve)
               (lambda (mxc callback cbargs)
                 (leman--media-request mxc session
                   :as (lambda ()
                         ;; EWW wants to parse the headers itself, so widen and decode them.
                         (widen)
                         (decode-coding-region (point-min) (point) 'utf-8)
                         ;; HACK: This STATUS argument to `eww-render' is bogus.
                         (apply callback 'status cbargs))))))
      (eww-browse-url mxc))))

;;;; Downloading media/files

;; We load `eww' to define this variable on-demand.
(defvar eww-download-directory)

(defun leman-room-download-file (event destination)
  "Download EVENT's file to DESTINATION.
If DESTINATION is a directory, use the file's default name;
otherwise, download to the filename.  Interactively, download to
`eww-download-directory'; with prefix, prompt for destination."
  (interactive (progn
                 (require 'eww)
                 (list (leman-room--event-at (point))
                       (if current-prefix-arg
                           (expand-file-name
                            (read-file-name
                             "Download to: "
                             (cl-typecase eww-download-directory
                               (string eww-download-directory)
                               (function (funcall eww-download-directory)))))
                         (expand-file-name
                          (cl-typecase eww-download-directory
                            (string eww-download-directory)
                            (function (funcall eww-download-directory))))))))
  (pcase-let* (((cl-struct leman-event
                           (content (map ('filename event-filename) ('url plain-url)
                                         ('file encrypted-file) body)))
                event)
               ;; For encrypted attachments, the mxc:// URI of the
               ;; ciphertext lives inside the "file" object.
               (mxc-url (or plain-url (map-elt encrypted-file 'url)))
               (started-at (current-time))
               (filename (if (not event-filename)
                             body
                           (if (equal body event-filename)
                               body
                             event-filename)))
               ;; The remote sender controls the attachment's filename:
               ;; keep only its base name so a crafted name can't write
               ;; outside DESTINATION (blank, ".", or ".." becomes
               ;; "download").
               (filename (cond ((member filename '("" "." "..")) "download")
                               (filename (directory-file-name
                                          (file-name-nondirectory filename)))
                               (t filename))))
    (when (file-directory-p destination)
      (unless (file-exists-p destination)
        (make-directory destination 'parents))
      (setf destination (file-name-concat destination filename)))
    (unless (file-writable-p destination)
      ;; FIXME: Pressing "C-u" before clicking a download link doesn't work.
      (user-error "Destination path not writable: %S (Call with prefix to prompt for filename)"
                  destination))
    (when (file-exists-p destination)
      (user-error "File already exists: %S (Call with prefix to prompt for filename)" destination))
    ;; TODO: For bonus points, provide a way to cancel a download (otherwise the user
    ;; would have to use `list-processes' and find the right one to delete), and to see
    ;; progress (perhaps borrowing some of the relevant code in hyperdrive.el).
    (let ((download-file (if encrypted-file
                             (make-temp-file "leman-download-")
                           destination)))
      (leman--media-request mxc-url leman-session :authenticatedp t
        :as `(file ,download-file)
        :then (lambda (&rest _)
                (when encrypted-file
                  ;; Verify and decrypt the downloaded ciphertext
                  ;; (hash first), then remove it.
                  (unwind-protect
                      (leman-room--decrypt-download-file
                       encrypted-file download-file destination)
                    (ignore-errors (delete-file download-file))))
                (let* ((file-size (file-attribute-size
                                   (file-attributes destination)))
                       (duration (float-time (time-subtract (current-time) started-at)))
                       (speed (file-size-human-readable (/ file-size duration))))
                  (message "File downloaded: %S (%s in %s at %s/sec) "
                           destination (file-size-human-readable file-size)
                           (format-seconds "%h:%m:%s%z seconds" duration)
                           speed)))
        :else (lambda (plz-error)
                (ignore-errors (delete-file download-file))
                (leman-api-error plz-error)))
      (message "Downloading to %S..." destination))))

(defun leman-room--decrypt-download-file (encrypted-file input output)
  "Decrypt the downloaded ciphertext at INPUT to OUTPUT.
ENCRYPTED-FILE is the event content's \"file\" object.  The
ciphertext's hash is verified with the agent before decrypting
(the hash is of the ciphertext, as included in the event)."
  (let ((agent (leman-session-e2ee leman-session)))
    (unless agent
      (user-error "No E2EE agent running to decrypt the downloaded file; see M-x leman-e2ee-status"))
    (leman-e2ee-decrypt-file agent input output
                             (map-nested-elt encrypted-file '(key k))
                             (map-elt encrypted-file 'iv)
                             (map-nested-elt encrypted-file '(hashes sha256)))))

;;;; Footer

(provide 'leman-room)

;;; leman-room.el ends here
