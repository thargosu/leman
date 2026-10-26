;;; leman-inbox.el --- IM-style inbox of Leman rooms  -*- lexical-binding: t; -*-

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

;; This library implements a lean, instant-messaging-style inbox of
;; Leman rooms, inspired by modern chat apps like WhatsApp and
;; FluffyChat.  It is an alternative to `leman-room-list' and
;; `leman-tabulated-room-list', which show every room attribute in
;; tabular form; this one shows only what matters for triaging
;; conversations:
;;
;; * A flat list of conversations in two-line rows: the room's avatar
;;   and name beside a timestamp on the first line, and a preview of
;;   the latest message beside an unread badge on the second.
;;
;; * Invitations, if any, are listed first; then favourite rooms; then
;;   everything else.  Within each group, unread rooms float to the
;;   top, most recently active first.  That makes
;;   `leman-inbox-next-unread' (bound to SPC) the main flow: press it
;;   repeatedly to open each unread room in turn.  Rooms are opened in
;;   another window (see `leman-inbox-display-buffer-action'), which
;;   remains unselected by default (see `leman-inbox-select-room'), so
;;   the inbox stays visible and in focus.
;;
;; * Left rooms are hidden by default, and low-priority rooms are not
;;   treated specially.
;;
;; To have the inbox update itself as events arrive, add this to your
;; configuration:
;;
;;   (add-hook 'leman-sync-callback-hook #'leman-inbox-auto-update)

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'hl-line)
(require 'subr-x)

(require 'svg-lib)

(require 'leman-lib)

;;;; Customization

(defgroup leman-inbox nil
  "Options for `leman-inbox' buffers."
  :group 'leman)

(defgroup leman-inbox-faces nil
  "Faces for `leman-inbox' buffers."
  :group 'leman-inbox
  :group 'leman-faces)

(defcustom leman-inbox-buffer-name "*Leman Inbox*"
  "Buffer name used by `leman-inbox'."
  :type 'string)

(defcustom leman-inbox-auto-update t
  "Automatically update inbox buffers as events arrive.
Updates are triggered from `leman-sync-callback-hook' by
`leman-inbox-auto-update'."
  :type 'boolean)

(defcustom leman-inbox-auto-update-interval 5
  "Minimum number of seconds between inbox auto-updates.
More frequent updates are coalesced."
  :type 'natnum)

(defcustom leman-inbox-avatars (display-images-p)
  "Show room avatars in the inbox."
  :type 'boolean)

(defcustom leman-inbox-avatar-generation (image-type-available-p 'svg)
  "Generate avatars for rooms that have none.
An avatar is generated from the room's first letter and a color
derived from its name."
  :type 'boolean)

(defcustom leman-inbox-avatar-size 1.75
  "Side of room avatars, in multiples of the default line height."
  :type 'number)

(defcustom leman-inbox-previews t
  "Show a preview of each room's latest message."
  :type 'boolean)

(defcustom leman-inbox-preview-max-events 30
  "Maximum number of latest events to search for a message preview."
  :type 'natnum)

(defcustom leman-inbox-show-left nil
  "Show rooms that have been left."
  :type 'boolean)

(defcustom leman-inbox-line-spacing 2
  "Space in pixels between inbox rows, or nil for none."
  :type '(choice (const :tag "None" nil) natnum))

(defcustom leman-inbox-display-buffer-action '((leman-inbox--display-room))
  "Display action used when opening a room from the inbox.
By default, a window already showing a Leman room buffer is
reused, or a new one is split off, so the inbox remains visible.
See function `display-buffer' and Info node `(elisp) Buffer
Display Action Functions'."
  :type display-buffer--action-custom-type
  :risky t)

(defcustom leman-inbox-select-room nil
  "Select the room's window after opening it from the inbox.
When nil, point and input focus remain in the inbox, so that,
e.g. \\[leman-inbox-next-unread] can be pressed repeatedly to
open each unread room in turn.  To switch to the room, use, e.g.
\\[other-window]."
  :type 'boolean)

;;;; Faces

(defface leman-inbox-name
  '((t (:inherit default)))
  "Room names."
  :group 'leman-inbox-faces)

(defface leman-inbox-name-unread
  '((t (:inherit (bold leman-inbox-name))))
  "Room names with unread messages."
  :group 'leman-inbox-faces)

(defface leman-inbox-name-invited
  '((t (:inherit (italic leman-inbox-name))))
  "Room names the local user has been invited to."
  :group 'leman-inbox-faces)

(defface leman-inbox-name-left
  '((t (:inherit (shadow leman-inbox-name))))
  "Room names that have been left."
  :group 'leman-inbox-faces)

(defface leman-inbox-preview
  '((t (:inherit font-lock-comment-face)))
  "Previews of rooms' latest messages."
  :group 'leman-inbox-faces)

(defface leman-inbox-preview-mention
  '((t (:inherit (bold leman-inbox-preview))))
  "Mentions of the local user in previews."
  :group 'leman-inbox-faces)

(defface leman-inbox-time
  '((t (:inherit font-lock-comment-face)))
  "Timestamps of rooms' latest messages."
  :group 'leman-inbox-faces)

(defface leman-inbox-time-recent
  '((t (:inherit (bold leman-inbox-time))))
  "Timestamps of messages less than an hour old."
  :group 'leman-inbox-faces)

(defface leman-inbox-time-old
  '((t (:inherit (shadow leman-inbox-time))))
  "Timestamps of messages more than a day old."
  :group 'leman-inbox-faces)

(defface leman-inbox-header
  '((t (:inherit (bold font-lock-comment-face))))
  "Group headings."
  :group 'leman-inbox-faces)

(defface leman-inbox-favourite
  '((t (:inherit font-lock-doc-face)))
  "Favourite-room markers."
  :group 'leman-inbox-faces)

(defface leman-inbox-badge
  '((t (:inherit (bold success))))
  "Unread badges.
Its foreground color is used as the background of badge pills
when SVG images are available."
  :group 'leman-inbox-faces)

(defface leman-inbox-badge-mention
  '((t (:inherit (bold error))))
  "Unread badges for rooms with highlights (mentions).
Its foreground color is used as the background of badge pills
when SVG images are available."
  :group 'leman-inbox-faces)

;;;; Keymaps

(defvar leman-inbox-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'leman-inbox-open)
    (define-key map (kbd "SPC") #'leman-inbox-next-unread)
    (define-key map (kbd "TAB") #'leman-inbox-next-unread)
    (define-key map "n" #'leman-inbox-next-row)
    (define-key map "p" #'leman-inbox-previous-row)
    (define-key map "g" #'revert-buffer)
    (define-key map "k" #'leman-inbox-kill-room-buffer)
    (define-key map "m" #'leman-inbox-mark-read)
    (define-key map "/" #'leman-inbox-filter)
    map)
  "Keymap for `leman-inbox' buffers.
See also `leman-inbox-row-map'.")

(defvar leman-inbox-row-map
  ;; Applied to every row's text, so that clicking anywhere in a row
  ;; opens the room.
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'leman-inbox-mouse-open)
    map)
  "Keymap applied to rows in `leman-inbox' buffers.")

;;;; Variables

(defvar leman-room-prism-minimum-contrast)
(defvar leman-sessions)
(defvar leman-users)
(defvar leman-view-room-display-buffer-action)

(defvar leman-inbox-filter-regexp nil
  "Regexp filtering displayed rooms, or nil.
Set by `leman-inbox-filter'.")

(defvar leman-inbox--render-width nil
  "Window body width, in columns, at the last render.")

(defvar leman-inbox--auto-update-timer nil
  "Pending timer for a coalesced inbox auto-update.")

(defvar leman-inbox--last-update-time nil
  "Time of the last inbox auto-update.")

(defvar leman-inbox--resize-timer nil
  "Pending timer for a debounced re-render after a window resize.")

;;;; Colors

(defun leman-inbox--face-foreground (face &optional fallback)
  "Return FACE's foreground color, or FALLBACK if unset."
  (let ((foreground (face-foreground face nil 'default)))
    (if (and foreground (not (equal foreground "unspecified-fg")))
        foreground
      (or fallback "gray"))))

(defun leman-inbox--contrast-foreground (color)
  "Return \"white\" or \"black\", whichever contrasts with COLOR."
  (if-let ((rgb (ignore-errors (color-name-to-rgb color)))
           (luminance (+ (* 0.299 (car rgb))
                         (* 0.587 (cadr rgb))
                         (* 0.114 (caddr rgb)))))
      (if (> luminance 0.55) "black" "white")
    "white"))

;;;; Icons

;; Previews mark media and encrypted messages with small inline SVG
;; icons instead of emoji, which many default fonts (including the
;; common "Ricty" family) cannot render.  The icons are embedded so
;; that no network access or icon files are needed.

(defconst leman-inbox--icon-paths
  '(("image" . "<rect x=\"3\" y=\"3\" width=\"18\" height=\"18\" rx=\"2\"/><circle cx=\"8.5\" cy=\"8.5\" r=\"1.5\"/><path d=\"M21 15l-5-5L5 21\"/>")
    ("file" . "<path d=\"M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z\"/><path d=\"M14 2v6h6\"/>")
    ("audio" . "<path d=\"M12 1a3 3 0 0 0-3 3v8a3 3 0 0 0 6 0V4a3 3 0 0 0-3-3z\"/><path d=\"M19 10v2a7 7 0 0 1-14 0v-2\"/><path d=\"M12 19v4\"/>")
    ("video" . "<path d=\"M23 7l-7 5 7 5V7z\"/><rect x=\"1\" y=\"5\" width=\"15\" height=\"14\" rx=\"2\"/>")
    ("location" . "<path d=\"M21 10c0 7-9 13-9 13s-9-6-9-13a9 9 0 0 1 18 0z\"/><circle cx=\"12\" cy=\"10\" r=\"3\"/>")
    ("sticker" . "<circle cx=\"12\" cy=\"12\" r=\"10\"/><path d=\"M8 14s1.5 2 4 2 4-2 4-2\"/><path d=\"M9 9h.01\"/><path d=\"M15 9h.01\"/>")
    ("lock" . "<rect x=\"3\" y=\"11\" width=\"18\" height=\"11\" rx=\"2\"/><path d=\"M7 11V7a5 5 0 0 1 10 0v4\"/>"))
  "SVG path data for preview icons, drawn in a 24x24 view box.")

(defvar leman-inbox--icon-cache (make-hash-table :test #'equal)
  "Cache of icon strings, keyed by name, color, and size.")

(defun leman-inbox--icon (name tty-label frame)
  "Return a string prefix for the icon NAME on FRAME.
When images can be displayed, this is a one-character string whose
display property shows the icon drawn in the preview face's
foreground color, followed by a space.  Otherwise it is a text
marker labelled TTY-LABEL.  The result is cached."
  (let* ((size (frame-char-height frame))
         (color (leman-inbox--face-foreground 'leman-inbox-preview "#777"))
         (key (list name color size (display-images-p))))
    (or (gethash key leman-inbox--icon-cache)
        (puthash
         key
         (if (not (display-images-p))
             (format "[%s] " tty-label)
           (propertize
            " " 'display
            (list 'image :type 'svg :ascent 'center
                  :data (concat
                         (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"%s\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\">"
                                 size size color)
                         (alist-get name leman-inbox--icon-paths nil nil #'equal)
                         "</svg>"))))
         leman-inbox--icon-cache))))

;;;; Items

;; A row's data is a [room session] vector, like in `leman-room-list'.

(defun leman-inbox--item-at-point (&optional position)
  "Return the [room session] vector of the row at POSITION.
POSITION defaults to point.  Return nil if point is not on a row."
  (get-text-property (or position (point)) 'leman-inbox-item))

(defun leman-inbox--row-bounds (&optional position)
  "Return (BEG . END) of the row at POSITION, or nil.
POSITION defaults to point."
  (when (get-text-property (or position (setq position (point)))
                           'leman-inbox-item)
    (cons (or (previous-single-property-change position 'leman-inbox-item)
              (point-min))
          (or (next-single-property-change position 'leman-inbox-item)
              (point-max)))))

(defun leman-inbox--next-item-position (position)
  "Return the position of the first row after POSITION, or nil."
  (let ((position (or position (point-min))))
    (cl-loop for next = (next-single-property-change position 'leman-inbox-item)
             while next
             when (get-text-property next 'leman-inbox-item)
             return next
             do (setf position next))))

(defun leman-inbox--row-positions ()
  "Return the list of row-start positions in the buffer, in order."
  (let ((positions nil)
        (position (if (get-text-property (point-min) 'leman-inbox-item)
                      (point-min)
                    (leman-inbox--next-item-position (point-min)))))
    (while position
      (push position positions)
      (setf position (leman-inbox--next-item-position position)))
    (nreverse positions)))

(defun leman-inbox--move-row (count)
  "Move point COUNT rows forward, or backward if COUNT is negative.
Moving before the first row or after the last keeps point where it
is, on the boundary row."
  (let ((remaining (abs count)))
    (while (> remaining 0)
      (let* ((positions (leman-inbox--row-positions))
             (target (if (> count 0)
                         ;; The first row starting after point.
                         (cl-position-if (lambda (start) (> start (point)))
                                         positions)
                       ;; Point at a row's start: the row above it;
                       ;; otherwise the nearest row before point.
                       (if-let ((index (cl-position-if (lambda (start)
                                                         (= start (point)))
                                                       positions)))
                           (1- index)
                         (if-let ((index (cl-position-if (lambda (start)
                                                           (> start (point)))
                                                         positions)))
                             (1- index)
                           (1- (length positions)))))))
        (if (and target (>= target 0) (< target (length positions)))
            (progn (goto-char (nth target positions))
                   (cl-decf remaining))
          (message "No more rows")
          (setf remaining 0))))))

(defun leman-inbox-next-row (&optional count)
  "Move to the next row.
With COUNT, move that many rows."
  (interactive "p")
  (leman-inbox--move-row (or count 1)))

(defun leman-inbox-previous-row (&optional count)
  "Move to the previous row.
With COUNT, move that many rows."
  (interactive "p")
  (leman-inbox--move-row (- (or count 1))))

(defun leman-inbox--hl-line-range ()
  "Return the bounds of the row at point, for `hl-line-mode'."
  (leman-inbox--row-bounds))

;;;; Avatars

(defun leman-inbox--avatar-side (&optional frame)
  "Return the side, in pixels, of avatars on FRAME."
  (round (* leman-inbox-avatar-size (frame-char-height frame))))

(defun leman-inbox--avatar-image (image side)
  "Return a copy of the image spec IMAGE displayed SIDE pixels square.
The returned spec keeps IMAGE's data and other properties, but
drops the sizing and scaling properties, which are replaced by
exact dimensions.  Note the result must be a proper image spec,
starting with the `image' symbol: mutating the property list of
the original spec with `setf' would prepend missing properties
before it, producing an invalid spec."
  (append (list 'image :width side :height side :ascent 'center)
          (cl-loop for (prop value) on (cdr image) by #'cddr
                   unless (memq prop '(:width :height :ascent
                                       :max-width :max-height :scale))
                   append (list prop value))))

(defun leman-inbox--svg-image (svg)
  "Return the image spec produced by an `svg-lib-*' function.
SVG is either an image spec, or a string whose display property
contains one (the return format has varied between versions)."
  (pcase svg
    (`(image . ,_) svg)
    ((pred stringp) (get-text-property 0 'display svg))))

(defun leman-inbox--generated-avatar (room side)
  "Return an avatar image spec generated for ROOM, SIDE pixels square."
  (let* ((name (or (leman-room-display-name room)
                   (leman--room-display-name room)
                   "?"))
         (letter (if (string-match (rx bos (or "#" "!" "@")) name)
                     (substring name 1)
                   name))
         (letter (if (string-empty-p letter) "?" letter))
         (leman-room-prism-minimum-contrast 1)
         (color (leman--prism-color letter :contrast-with "white")))
    (when-let ((image (leman-inbox--svg-image
                       (svg-lib-tag (substring letter 0 1) nil
                                    :background color :foreground "white"
                                    :stroke 0 :margin 0))))
      (leman-inbox--avatar-image image side))))

(defun leman-inbox--avatar (room frame)
  "Return (AVATAR-STRING . COLUMNS) for ROOM's avatar on FRAME.
AVATAR-STRING occupies exactly COLUMNS columns (COLUMNS may be a
fraction when an image is displayed), or is \"\" when avatars are
disabled.  The result is cached in ROOM's local data and
regenerated when its avatar or name changes."
  (if (not (and leman-inbox-avatars (display-images-p)))
      '("" . 0)
    (let* ((side (leman-inbox--avatar-side frame))
           (name (or (leman-room-display-name room)
                     (leman--room-display-name room)))
           (source (leman-room-avatar room))
           (cached (alist-get 'inbox-avatar (leman-room-local room))))
      (or (when (and cached
                     (eq (nth 0 cached) source)
                     (equal (nth 1 cached) name)
                     (equal (nth 2 cached) side))
            (cons (nth 3 cached) (nth 4 cached)))
          (let* ((display (and (stringp source)
                               (get-text-property 0 'display source)))
                 ;; Only a genuine image specification can be
                 ;; displayed; anything else (a string of raw image
                 ;; data, say) would render as binary garbage.
                 (image (or (and (consp display) (eq (car display) 'image)
                                 display)
                            (when leman-inbox-avatar-generation
                              (leman-inbox--generated-avatar room side))))
                 (columns (/ side (frame-char-width frame) 1.0))
                 (string (if image
                             (propertize " " 'display
                                         (leman-inbox--avatar-image image side))
                           (make-string (max 1 (round columns)) ?\s))))
            (unless image
              (setf columns (string-width string)))
            (setf (alist-get 'inbox-avatar (leman-room-local room))
                  (list source name side string columns))
            (cons string columns))))))

;;;; Previews

(defun leman-inbox--display-name (room user-id)
  "Return a display name for USER-ID in ROOM."
  (or (when-let ((user (or (gethash user-id (leman-room-members room))
                           (gethash user-id leman-users))))
        (leman--user-displayname-in room user))
      user-id))

(defun leman-inbox--sender-name (room event)
  "Return the display name of EVENT's sender in ROOM."
  (when-let ((sender (leman-event-sender event)))
    (leman--user-displayname-in room sender)))

(defun leman-inbox--single-line (string)
  "Return STRING with newlines and repeated spaces collapsed.
A body that contains control characters (for example raw image
data mislabeled as text by a bridge) is replaced by a
placeholder, as rendering it would show binary garbage."
  (let ((string (replace-regexp-in-string "[\n\r\t]+" " " (or string ""))))
    (setf string (replace-regexp-in-string "  +" " " string))
    (if (string-match-p "[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]" string)
        "[binary data]"
      string)))

(defun leman-inbox--message-preview (event room session frame)
  "Return preview text for \"m.room.message\" EVENT in ROOM on SESSION."
  (pcase-let* (((cl-struct leman-event content) event)
               ((map ('body main-body) ('msgtype main-msgtype)
                     ('m.new_content (map ('body new-body) ('msgtype new-msgtype))))
                 content)
               (body (or new-body main-body))
               (msgtype (or new-msgtype main-msgtype)))
    (when body
      (let ((body (leman-inbox--single-line body))
            (sender-name (leman-inbox--sender-name room event))
            (our-message-p (equal (leman-user-id (leman-session-user session))
                                  (leman-user-id (leman-event-sender event)))))
        (leman-inbox--highlight-mentions
         (pcase msgtype
           ("m.emote" (format "* %s %s" (or sender-name "?") body))
           ("m.image" (concat (leman-inbox--icon "image" "photo" frame) body))
           ("m.file" (concat (leman-inbox--icon "file" "file" frame) body))
           ("m.audio" (concat (leman-inbox--icon "audio" "audio" frame) body))
           ("m.video" (concat (leman-inbox--icon "video" "video" frame) body))
           ("m.location" (concat (leman-inbox--icon "location" "pin" frame) body))
           (_ (cond (our-message-p (concat "You: " body))
                    ((not (leman--room-direct-p room session))
                     (concat sender-name ": " body))
                    (t body))))
         room session)))))

(defun leman-inbox--highlight-mentions (string room session)
  "Return STRING with mentions of the local user highlighted.
Highlights the user's display name and ID, and \"@room\", with
the `leman-inbox-preview-mention' face."
  (let* ((me (leman-session-user session))
         (patterns (delq nil (list (leman--user-displayname-in room me)
                                   (leman-user-id me)
                                   "@room")))
         (regexp (and patterns (regexp-opt patterns))))
    (when (and regexp (not (string-empty-p string)))
      (let ((start 0))
        (while (string-match regexp string start)
          (put-text-property (match-beginning 0) (match-end 0)
                             'face 'leman-inbox-preview-mention string)
          (setf start (match-end 0)))))
    string))

(defun leman-inbox--actor-preview (event room description)
  "Return \"SENDER DESCRIPTION\" for EVENT in ROOM."
  (if-let ((sender-name (leman-inbox--sender-name room event)))
      (format "%s %s" sender-name description)
    description))

(defun leman-inbox--membership-preview (event room session)
  "Return preview text for \"m.room.member\" EVENT in ROOM on SESSION."
  (pcase-let* (((cl-struct leman-event content state-key) event)
               ((map membership) content)
               (actor-name (leman-inbox--sender-name room event))
               (target-id (or state-key (leman-user-id (leman-event-sender event))))
               (target-name (leman-inbox--display-name room target-id))
               (our-id (leman-user-id (leman-session-user session))))
    (pcase membership
      ("invite" (if (equal target-id our-id)
                    (format "%s invited you" actor-name)
                  (format "%s invited %s" actor-name target-name)))
      ("join" (format "%s joined" target-name))
      ("leave" (if (equal target-id our-id)
                   "You left"
                 (format "%s left" target-name)))
      ("ban" (format "%s banned %s" actor-name target-name))
      (_ nil))))

(defun leman-inbox--event-preview (event room session frame)
  "Return preview text for EVENT in ROOM on SESSION.
Return nil for events that do not make a useful preview, like
reactions; the latest message is shown instead."
  (pcase (leman-event-type event)
    ("m.room.message" (leman-inbox--message-preview event room session frame))
    ("m.sticker" (concat (leman-inbox--icon "sticker" "sticker" frame) "Sticker"))
    ("m.room.encrypted" (concat (leman-inbox--icon "lock" "encrypted" frame)
                                "Encrypted message"))
    ("m.room.member" (leman-inbox--membership-preview event room session))
    ("m.room.name" (leman-inbox--actor-preview event room "changed the room name"))
    ("m.room.topic" (leman-inbox--actor-preview event room "changed the topic"))
    ("m.room.avatar" (leman-inbox--actor-preview event room "changed the room avatar"))
    ("m.room.encryption" (leman-inbox--actor-preview event room "enabled encryption"))
    ("m.room.create" (leman-inbox--actor-preview event room "created the room"))
    (_ nil)))

(defun leman-inbox--invite-preview (room session)
  "Return preview text for an invitation to ROOM on SESSION."
  (let* ((our-id (leman-user-id (leman-session-user session)))
         (event (or (cl-loop for event in (leman-room-invite-state room)
                             when (and (equal "m.room.member" (leman-event-type event))
                                       (equal our-id (leman-event-state-key event)))
                             return event)
                    (cl-loop for event in (leman-room-invite-state room)
                             when (equal "m.room.member" (leman-event-type event))
                             return event))))
    (if-let ((sender-name (when event (leman-inbox--sender-name room event))))
        (format "%s invited you" sender-name)
      "Invitation")))

(defun leman-inbox--typing-preview (names)
  "Return a preview for NAMES, the members currently typing."
  (pcase names
    (`(,name) (format "%s is typing…" name))
    (`(,one ,two) (format "%s and %s are typing…" one two))
    (`(,_ ,_ . ,_) "Several people are typing…")
    (_ nil)))

(defun leman-inbox--preview (room session &optional frame)
  "Return the preview text for ROOM on SESSION."
  (if (not leman-inbox-previews)
      ""
    (or (pcase (leman-room-status room)
          ('invite (leman-inbox--invite-preview room session))
          (_ (or (leman-inbox--typing-preview
                  (map-elt (leman-room-local room) 'typing))
                 (cl-loop for event in (leman-room-timeline room)
                          for index from 1 upto leman-inbox-preview-max-events
                          for text = (leman-inbox--event-preview event room session frame)
                          when text return text))))
        "")))

;;;; Badges

(defun leman-inbox--badge-pill (label face)
  "Return a pill-shaped badge showing LABEL with FACE's color."
  (if (not (display-images-p))
      ;; Images can't be shown (e.g. on a TTY): plain text badge.
      (propertize (format " %s " label) 'face face)
    (or (when-let* ((color (leman-inbox--face-foreground face "#666"))
                    (image (leman-inbox--svg-image
                            (svg-lib-tag label nil
                                         :background color
                                         :foreground (leman-inbox--contrast-foreground
                                                      color)
                                         :stroke 0 :margin 1))))
          (propertize " " 'display image))
        ;; SVG generation failed: plain text badge.
        (propertize (format " %s " label) 'face face))))

(defun leman-inbox--badge (room session)
  "Return the unread badge string for ROOM on SESSION.
The string is empty when the room has no unread messages."
  (pcase-let* (((cl-struct leman-room unread-notifications) room)
               ((map notification_count highlight_count) unread-notifications)
               ;; Pings detected client-side (bridges often don't
               ;; trigger server-side highlights).
               (pings (map-elt (leman-room-local room) 'pings)))
    (cond ((and highlight_count (not (zerop highlight_count)))
           (leman-inbox--badge-pill (format "@%d" highlight_count)
                                    'leman-inbox-badge-mention))
          ((and pings (not (zerop pings)))
           (leman-inbox--badge-pill (format "@%d" pings)
                                    'leman-inbox-badge-mention))
          ((and notification_count (not (zerop notification_count)))
           (leman-inbox--badge-pill (number-to-string notification_count)
                                    'leman-inbox-badge))
          ((leman--room-unread-p room session)
           ;; Unread without notifications (e.g. a muted room): a dot.
           (propertize "•" 'face 'leman-inbox-badge))
          (t ""))))

;;;; Timestamps

(defun leman-inbox--format-time (latest-ts)
  "Return a compact display of the timestamp LATEST-TS.
LATEST-TS is in milliseconds since the epoch, like
`leman-room-latest-ts'.  Messages from today show their time,
from this week their weekday, and older ones their date."
  (let* ((seconds (floor (/ latest-ts 1000.0)))
         (days (- (time-to-days (seconds-to-time seconds))
                  (time-to-days nil))))
    (cond ((equal days 0) (format-time-string "%H:%M" seconds))
          ((equal days 1) "Yesterday")
          ((< days 7) (format-time-string "%a" seconds))
          ((< days 365) (format-time-string "%d %b" seconds))
          (t (format-time-string "%d %b %y" seconds)))))

(defun leman-inbox--time (room)
  "Return the propertized timestamp string for ROOM."
  (if-let ((latest-ts (leman-room-latest-ts room)))
      (let* ((age (- (float-time) (/ latest-ts 1000.0)))
             (face (cond ((< age 3600) 'leman-inbox-time-recent)
                         ((< age 86400) 'leman-inbox-time)
                         (t 'leman-inbox-time-old))))
        (propertize (leman-inbox--format-time latest-ts) 'face face))
    ""))

;;;; Names

(defun leman-inbox--name (room session)
  "Return the propertized room-name string for ROOM on SESSION."
  (let* ((status (leman-room-status room))
         (face (pcase status
                 ('invite 'leman-inbox-name-invited)
                 ('leave 'leman-inbox-name-left)
                 (_ (if (leman--room-unread-p room session)
                        'leman-inbox-name-unread
                      'leman-inbox-name))))
         (name (or (leman-room-display-name room)
                   (leman--room-display-name room)
                   (leman-room-id room)))
         (name (if (leman-inbox--binary-string-p name)
                   ;; A name of binary data (e.g. a mislabeled image)
                   ;; would render as garbage; the room id is safer.
                   (leman-room-id room)
                 name))
         (favourite-p (leman--room-favourite-p room)))
    (propertize (if favourite-p (concat name " ★") name)
                'face face
                'help-echo (or (leman-room-canonical-alias room)
                               (leman-room-topic room)))))

;;;; Rows

(defun leman-inbox--truncate (string columns)
  "Return STRING truncated to COLUMNS columns with an ellipsis."
  (if (> (string-width string) columns)
      (truncate-string-to-width string (max 0 (- columns 1)) 0 nil "…")
    string))

(defun leman-inbox--display-columns (string frame)
  "Return STRING's displayed width in columns on FRAME."
  (or (leman-inbox--image-columns
       (get-text-property 0 'display string) frame)
      (string-width string)))

(defun leman-inbox--image-columns (display frame)
  "Return the width in columns of an image display spec on FRAME.
Return nil if DISPLAY is not an image display specification, or
images cannot be displayed."
  (declare (function image-size "image"))
  (when (and (display-images-p)
             (consp display)
             (eq (car display) 'image))
    (ceiling (/ (car (image-size display t))
                (frame-char-width frame) 1.0))))

(defun leman-inbox--align-to-right (width-columns)
  "Return a display space aligning following text to the right.
WIDTH-COLUMNS is the width, in default-character-width units, of
the text that follows the space."
  (propertize " " 'display `(space :align-to (- right ,width-columns))))

(defun leman-inbox--insert-row (item window)
  "Insert a row for ITEM into the current buffer.
ITEM is a [room session] vector.  WINDOW is the window the buffer
is displayed in, used to compute row widths, or nil if it is not
currently displayed."
  (pcase-let* ((`[,room ,session] item)
               (frame (if window (window-frame window) (selected-frame)))
               (columns (if window (window-body-width window) 80))
               (avatar (leman-inbox--avatar room frame))
               (avatar-string (car avatar))
               (avatar-columns (cdr avatar))
               (gutter (if (> avatar-columns 0)
                           (concat avatar-string " ")
                         ""))
               (time (leman-inbox--time room))
               (badge (leman-inbox--badge room session))
               (badge-columns (leman-inbox--display-columns badge frame))
               (preview (leman-inbox--preview room session frame))
               (icon-columns (leman-inbox--image-columns
                              (get-text-property 0 'display preview)
                              frame))
               (preview-columns (- columns (ceiling avatar-columns)
                                   badge-columns
                                   (if icon-columns (1- icon-columns) 0)
                                   5))
               (name-columns (- columns (ceiling avatar-columns)
                                (string-width time) 4))
               (indent (if (> avatar-columns 0)
                           (propertize " " 'display
                                       `(space :width ,(+ 1.0 avatar-columns)))
                         "")))
    (insert (propertize
             (concat gutter
                     (leman-inbox--truncate (leman-inbox--name room session)
                                            name-columns)
                     (unless (equal time "")
                       (concat (leman-inbox--align-to-right (string-width time))
                               time))
                     "\n"
                     indent
                     (leman-inbox--truncate
                      ;; Append the preview face rather than
                      ;; replacing the string's own (e.g. the
                      ;; mention highlight's).
                      (let ((string (copy-sequence preview)))
                        (add-face-text-property 0 (length string)
                                                'leman-inbox-preview 'append string)
                        (propertize string 'help-echo preview))
                      preview-columns)
                     (unless (equal badge "")
                       (concat (leman-inbox--align-to-right badge-columns)
                               badge))
                     "\n")
             'leman-inbox-item item
             'mouse-face 'highlight
             'keymap leman-inbox-row-map))))

;;;; Entries

(defun leman-inbox--included-p (item)
  "Return non-nil if ITEM, a [room session] vector, should be shown."
  (pcase-let* ((`[,room ,_session] item)
               (name (or (leman-room-display-name room)
                         (leman--room-display-name room)
                         "")))
    (and (or leman-inbox-show-left
             (not (equal 'leave (leman-room-status room))))
         (or (null leman-inbox-filter-regexp)
             (string-match-p leman-inbox-filter-regexp name)
             (when-let ((alias (leman-room-canonical-alias room)))
               (string-match-p leman-inbox-filter-regexp alias))))))

(defun leman-inbox--sort (items)
  "Sort ITEMS, a list of [room session] vectors, for display.
Unread rooms come first, then others; within each group, rooms
are sorted by their latest activity, most recent first.  Ties are
broken by room ID so that the order is deterministic."
  (cl-labels ((newer-p (a b)
                (pcase-let* ((`[,room-a ,session-a] a)
                             (`[,room-b ,session-b] b)
                             (unread-a (leman--room-unread-p room-a session-a))
                             (unread-b (leman--room-unread-p room-b session-b))
                             (ts-a (or (leman-room-latest-ts room-a) 0))
                             (ts-b (or (leman-room-latest-ts room-b) 0)))
                  (cond ((not (equal unread-a unread-b)) unread-a)
                        ((/= ts-a ts-b) (> ts-a ts-b))
                        (t (string< (leman-room-id room-a)
                                    (leman-room-id room-b)))))))
    (cl-sort items #'newer-p)))

(defun leman-inbox--groups ()
  "Return (INVITED FAVOURITES OTHER) for display.
Each is a list of [room session] vectors, sorted by
`leman-inbox--sort'.  Left rooms and rooms not matching
`leman-inbox-filter-regexp' are omitted."
  (let ((invited nil) (favourites nil) (other nil))
    (cl-loop for (_id . session) in leman-sessions
             do (cl-loop for room in (leman-session-rooms session)
                         for item = (vector room session)
                         when (leman-inbox--included-p item)
                         do (pcase (leman-room-status room)
                              ('invite (push item invited))
                              (_ (if (leman--room-favourite-p room)
                                     (push item favourites)
                                   (push item other))))))
    (list (leman-inbox--sort invited)
          (leman-inbox--sort favourites)
          (leman-inbox--sort other))))

;;;; Rendering

(defun leman-inbox--insert-group (label items window)
  "Insert LABEL and ITEMS, a list of [room session] vectors.
ITEMS must be non-empty.  WINDOW is the window displaying the
buffer, used to compute row widths, or nil if not displayed."
  (insert (propertize (format " %s\n" label) 'face 'leman-inbox-header))
  (dolist (item items)
    (leman-inbox--insert-row item window))
  (insert "\n"))

(defun leman-inbox--render ()
  "Rebuild the current inbox buffer's contents."
  (cl-labels ((find-item-position (item)
                (cl-loop with position = (point-min)
                         for next = (leman-inbox--next-item-position position)
                         while next
                         when (equal (elt (get-text-property
                                           next 'leman-inbox-item)
                                          0)
                                     (elt item 0))
                         return next
                         do (setf position next))))
    (pcase-let* ((window (get-buffer-window (current-buffer)))
                 (inhibit-read-only t)
                 (`(,invited ,favourites ,other) (leman-inbox--groups))
                 (entries (append invited favourites other))
                 (unread-count (cl-count-if
                                (lambda (item)
                                  (leman--room-unread-p (elt item 0)
                                                        (elt item 1)))
                                entries))
                 (previous-item (leman-inbox--item-at-point)))
      (setf leman-inbox--render-width
            (if window (window-body-width window) 80))
      (delete-all-overlays)
      (erase-buffer)
      (save-excursion
        (when invited
          (leman-inbox--insert-group "Invites" invited window))
        (when favourites
          (leman-inbox--insert-group "Pinned" favourites window))
        (if (and (null invited) (null favourites) (null other))
            (insert (propertize " No chats\n" 'face 'leman-inbox-header))
          (dolist (item other)
            (leman-inbox--insert-row item window))))
      (setf header-line-format
            (concat (propertize (format " Inbox · %d chats · %d unread"
                                        (length entries) unread-count)
                                'face 'leman-inbox-header)
                    (when leman-inbox-filter-regexp
                      (propertize (format " · filter: %s"
                                          leman-inbox-filter-regexp)
                                  'face 'leman-inbox-preview))))
      (goto-char (point-min))
      (when previous-item
        (when-let ((position (find-item-position previous-item)))
          (goto-char position))))))

;;;; Commands

;;;###autoload
(cl-defun leman-inbox (&key (buffer-name leman-inbox-buffer-name)
                            (display-buffer-action '((display-buffer-reuse-window
                                                      display-buffer-same-window))))
  "Show a lean, IM-style inbox of Leman rooms in a new buffer.
The buffer is named BUFFER-NAME and shown with
DISPLAY-BUFFER-ACTION; or if DISPLAY-BUFFER-ACTION is nil, the
buffer is not displayed."
  (interactive)
  (unless leman-sessions
    (error "Leman: Not connected.  Use `leman-connect' to connect"))
  (with-current-buffer (get-buffer-create buffer-name)
    (unless (derived-mode-p 'leman-inbox-mode)
      (leman-inbox-mode))
    (leman-inbox--render))
  (when display-buffer-action
    (when-let ((window (display-buffer buffer-name display-buffer-action)))
      (select-window window)))
  (set-buffer buffer-name))

(defun leman-inbox-revert (&optional _ignore-auto _noconfirm)
  "Revert the current inbox buffer.
To be used as `revert-buffer-function'."
  (interactive)
  (leman-inbox--render))

;;;; Debugging

(defun leman-inbox--debug-row-name ()
  "Return the room name of the row at point, for debugging."
  (when-let ((item (get-char-property (point) 'leman-inbox-item)))
    (let ((room (elt item 0)))
      (or (leman-room-display-name room)
          (leman--room-display-name room)
          (leman-room-id room)))))

(defun leman-inbox--debug-describe (display)
  "Return a readable description of an unrecognized DISPLAY value."
  (if (stringp display)
      (format "a string of %d bytes, starting %S"
              (length display)
              (substring display 0 (min 60 (length display))))
    (let ((text (format "%S" display)))
      (if (> (length text) 200)
          (concat (substring text 0 200) "...")
        text))))

(defun leman-inbox--binary-char-p (char)
  "Return non-nil if CHAR is a control or raw eight-bit character.
Such characters never appear in ordinary message text; a string
full of them is binary data, like a mislabeled image.  Newlines,
carriage returns, and tabs are ordinary and do not count."
  (and (/= char 9) (/= char 10) (/= char 13)
       (or (< char 32) (= char 127)
           (memq (char-charset char)
                 '(eight-bit-control eight-bit-graphic)))))

(defun leman-inbox--binary-string-p (string)
  "Return non-nil if STRING contains binary-looking characters."
  (and (stringp string)
       (cl-some #'leman-inbox--binary-char-p string)))

(defun leman-inbox--debug-sample (string)
  "Return a printable sample of STRING for debug reports."
  (let ((sample (substring-no-properties string 0 (min 60 (length string)))))
    (format "%S" sample)))

(defun leman-inbox--debug-session-data ()
  "Return a report of session data that looks like binary garbage.
Scans every room's avatar slot, display name, and recent event
bodies."
  (let ((issues nil))
    (cl-loop for (_id . session) in leman-sessions
             do (cl-loop for room in (leman-session-rooms session)
                         for name = (or (leman-room-display-name room)
                                        (leman--room-display-name room)
                                        (leman-room-id room))
                         for avatar = (leman-room-avatar room)
                         when (and (stringp avatar)
                                   (leman-inbox--binary-string-p avatar))
                         do (push (format "  room %S: the avatar slot itself contains binary data (%d chars), starting %S"
                                          name (length avatar)
                                          (leman-inbox--debug-sample avatar))
                                  issues)
                         when (and (stringp avatar)
                                   (null (get-text-property 0 'display avatar)))
                         do (push (format "  room %S: the avatar slot has no display property (%d chars, starting %S)"
                                          name (length avatar)
                                          (leman-inbox--debug-sample avatar))
                                  issues)
                         when (leman-inbox--binary-string-p
                               (leman-room-display-name room))
                         do (push (format "  room %S: the display name contains binary data"
                                          name)
                                  issues)
                         when (leman-inbox--binary-string-p
                               (leman--room-display-name room))
                         do (push (format "  room %S: the computed display name contains binary data (%S)"
                                          name
                                          (leman-inbox--debug-sample
                                           (or (leman--room-display-name room) "")))
                                  issues)
                         do (cl-loop for event in (leman-room-timeline room)
                                     for index from 1 upto 10
                                     for body = (alist-get 'body
                                                           (leman-event-content event))
                                     when (and (stringp body)
                                               (leman-inbox--binary-string-p body))
                                     do (push (format "  room %S: event %d (%s) has a binary body of %d chars, starting %S"
                                                      name index
                                                      (leman-event-type event)
                                                      (length body)
                                                      (leman-inbox--debug-sample body))
                                              issues))))
    (nreverse issues)))

(defun leman-inbox-debug ()
  "Pop a buffer reporting how the current inbox buffer is built.
This is a debugging aid.  It lists raw bytes and control
characters present as buffer text (with the row they occur in),
display properties that are not recognized image or space
specifications, and event data in the live sessions that looks
like binary garbage."
  (interactive)
  (let ((display-issues nil) (char-issues 0) (char-samples nil)
        (size (buffer-size))
        (seen (make-hash-table :test #'equal)))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let* ((char (char-after))
               (display (get-char-property (point) 'display))
               (binary (leman-inbox--binary-char-p char)))
          (when (and display
                     (not (and (consp display)
                               (memq (car display)
                                     '(image space when slice)))))
            (let ((key (prin1-to-string display)))
              (unless (gethash key seen)
                (puthash key t seen)
                (push (format "  %s\n  at position %d, in row %S"
                              (leman-inbox--debug-describe display)
                              (point)
                              (leman-inbox--debug-row-name))
                      display-issues))))
          (when (and binary (/= char 10) (/= char 9))
            (cl-incf char-issues)
            (when (< (length char-samples) 3)
              (push (format "  U+%04X at position %d, in row %S, context %S"
                            char (point) (leman-inbox--debug-row-name)
                            (buffer-substring-no-properties
                             (max (point-min) (- (point) 20))
                             (min (point-max) (+ (point) 20))))
                    char-samples))))
        (forward-char 1)))
    (let ((session-issues (ignore-errors (leman-inbox--debug-session-data)))
          (buffer-dump (buffer-substring-no-properties
                        (point-min) (min (point-max) (+ (point-min) 400)))))
      (with-current-buffer (get-buffer-create "*Leman Inbox Debug*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Inbox buffer: %d chars\n\n" size))
          (insert (format "Binary or control characters as buffer text: %d\n"
                          char-issues))
          (dolist (sample (nreverse char-samples))
            (insert sample "\n"))
          (insert (format "\nUnrecognized display properties: %d\n"
                          (length display-issues)))
          (dolist (issue (nreverse display-issues))
            (insert issue "\n\n"))
          (insert "\nBinary-looking data in the live sessions:\n")
          (if session-issues
              (dolist (issue session-issues)
                (insert issue "\n"))
            (insert "  none found\n"))
          (insert (format "\nFirst %d chars of the inbox buffer, escaped:\n  %S\n"
                          400 buffer-dump))
          (when (and (null display-issues) (zerop char-issues)
                     (null session-issues))
            (insert "\nNo problems found.\n"))
          (pop-to-buffer (current-buffer)))))))

(defun leman-inbox--display-room (buffer alist)
  "Display room BUFFER for rooms opened from the inbox.
Reuse a window already showing a Leman room or thread buffer, if
any; otherwise split off a new window, preferably from the
inbox's.  To be used as a `display-buffer' action function; see
`leman-inbox-display-buffer-action', which defaults to it."
  (or (display-buffer-reuse-window buffer alist)
      (if-let ((window (cl-loop for window in (window-list nil 'never)
                                when (and (not (eq window (selected-window)))
                                          (not (window-dedicated-p window))
                                          ;; A window showing a room or
                                          ;; thread buffer makes a good
                                          ;; "chat pane".
                                          (buffer-local-value 'leman-room
                                                              (window-buffer window)))
                                return window)))
          (window--display-buffer buffer window 'reuse)
        (let ((window (or (split-window-sensibly (selected-window))
                          (split-window (selected-window) nil 'below))))
          (window--display-buffer buffer window 'window)))))

(defun leman-inbox--view (room session)
  "View ROOM on SESSION, moving to its first unread message.
The room is displayed with `leman-inbox-display-buffer-action';
by default the inbox remains visible, and, unless
`leman-inbox-select-room' is set, selected."
  (declare (function leman-view-room "leman-room")
           (function leman-room-goto-fully-read-marker "leman-room")
           (function leman-view-space "leman-directory"))
  (let ((leman-view-room-display-buffer-action leman-inbox-display-buffer-action)
        (inbox-window (selected-window)))
    (if (leman--space-p room)
        (leman-view-space room session)
      (leman-view-room room session)
      (when (and (leman--room-unread-p room session)
                 (equal 'join (leman-room-status room)))
        (leman-room-goto-fully-read-marker)))
    (unless leman-inbox-select-room
      ;; Keep the inbox selected, e.g. so that `leman-inbox-next-unread'
      ;; can be pressed repeatedly to open each unread room in turn.
      (when (and (window-live-p inbox-window)
                 (not (eq inbox-window (selected-window))))
        (select-window inbox-window)))))

(defun leman-inbox-open ()
  "Open the room at point in another window.
For unread rooms, point is moved to the first unread message.
By default the inbox remains visible and selected; see
`leman-inbox-display-buffer-action' and `leman-inbox-select-room'."
  (interactive)
  (if-let ((item (leman-inbox--item-at-point)))
      (pcase-let ((`[,room ,session] item))
        (leman-inbox--view room session))
    (user-error "No room at point")))

(defun leman-inbox-mouse-open (event)
  "Open the room clicked with EVENT."
  (interactive "e")
  (mouse-set-point event)
  (leman-inbox-open))

(defun leman-inbox--unread-item-after (position)
  "Return the first unread item at a row at or after POSITION, or nil."
  (catch 'found
    (when-let ((item (get-text-property position 'leman-inbox-item))
               ((leman--room-unread-p (elt item 0) (elt item 1))))
      (throw 'found item))
    (let ((scan position))
      (while (setf scan (leman-inbox--next-item-position scan))
        (let ((item (get-text-property scan 'leman-inbox-item)))
          (when (leman--room-unread-p (elt item 0) (elt item 1))
            (throw 'found item))))
      nil)))

(defun leman-inbox-next-unread ()
  "Open the next unread room at or after the current row, wrapping.
By default the inbox remains visible and selected, so this may be
pressed repeatedly to open each unread room in turn.  With no
unread rooms, say so."
  (interactive)
  (let* ((cutoff (or (car (leman-inbox--row-bounds)) (point-min)))
         (item (or (leman-inbox--unread-item-after cutoff)
                   (leman-inbox--unread-item-after (point-min)))))
    (if item
        (pcase-let ((`[,room ,session] item))
          (leman-inbox--view room session))
      (message "No unread rooms"))))

(defun leman-inbox-mark-read ()
  "Mark the room at point as read up to its latest event."
  (declare (function leman-room-mark-read "leman-room"))
  (interactive)
  (if-let ((item (leman-inbox--item-at-point)))
      (pcase-let* ((`[,room ,session] item)
                   (latest-event (car (leman-room-timeline room))))
        (if latest-event
            (progn
              (leman-room-mark-read room session
                :read-event latest-event
                :fully-read-event latest-event)
              (leman-inbox--render))
          (user-error "Room has no events to mark read")))
    (user-error "No room at point")))

(defun leman-inbox-kill-room-buffer ()
  "Kill the buffer of the room at point, if any."
  (interactive)
  (if-let ((item (leman-inbox--item-at-point)))
      (pcase-let* ((`[,room ,_session] item)
                   ((cl-struct leman-room (local (map buffer))) room))
        (if (buffer-live-p buffer)
            (progn
              (kill-buffer buffer)
              (leman-inbox--render))
          (user-error "Room has no buffer")))
    (user-error "No room at point")))

(defun leman-inbox-filter (regexp)
  "Filter the inbox to rooms whose name matches REGEXP.
With empty input, clear the filter."
  (interactive
   (list (read-regexp "Filter rooms (empty to clear)"
                      leman-inbox-filter-regexp)))
  (setf leman-inbox-filter-regexp (and (not (string-empty-p regexp)) regexp))
  (leman-inbox--render))

;;;; Mode

(define-derived-mode leman-inbox-mode special-mode "Leman-Inbox"
  "Major mode for IM-style inbox buffers made by `leman-inbox'."
  (setq-local truncate-lines t
              revert-buffer-function #'leman-inbox-revert
              hl-line-range-function #'leman-inbox--hl-line-range
              line-spacing leman-inbox-line-spacing
              leman-inbox--render-width nil)
  (hl-line-mode 1))

;;;; Auto-update

(defun leman-inbox--do-auto-update ()
  "Revert the inbox buffer, if it still exists."
  (setf leman-inbox--auto-update-timer nil
        leman-inbox--last-update-time (current-time))
  (when-let ((buffer (get-buffer leman-inbox-buffer-name)))
    (with-current-buffer buffer
      (unless (region-active-p)
        ;; Don't refresh if the user is selecting rows.
        (leman-inbox--render)))))

;;;###autoload
(defun leman-inbox-auto-update (_session)
  "Automatically update the inbox buffer.
Does so when variable `leman-inbox-auto-update' is non-nil, at
most once every `leman-inbox-auto-update-interval' seconds (more
frequent updates are coalesced), and only while the inbox is
displayed in some window.  To be called in
`leman-sync-callback-hook'."
  (when (and leman-inbox-auto-update
             (buffer-live-p (get-buffer leman-inbox-buffer-name))
             (get-buffer-window leman-inbox-buffer-name 'all-frames))
    (let* ((elapsed (if leman-inbox--last-update-time
                        (float-time (time-subtract (current-time)
                                                   leman-inbox--last-update-time))
                      most-positive-fixnum))
           (delay (max 0 (- leman-inbox-auto-update-interval elapsed))))
      (if (zerop delay)
          (leman-inbox--do-auto-update)
        ;; Schedule a coalesced update, if none is already pending.
        (unless (timerp leman-inbox--auto-update-timer)
          (setf leman-inbox--auto-update-timer
                (run-with-timer delay nil #'leman-inbox--do-auto-update)))))))

(defun leman-inbox--window-resized (_frame)
  "Re-render the inbox after a resize of FRAME's windows.
The re-render is debounced and only happens when the inbox
window's width actually changed (rows are truncated to the window
width, so a resize can otherwise leave them misaligned)."
  (when-let ((buffer (get-buffer leman-inbox-buffer-name))
             (window (get-buffer-window buffer))
             ((not (equal (window-body-width window)
                          (buffer-local-value 'leman-inbox--render-width
                                              buffer)))))
    (unless (timerp leman-inbox--resize-timer)
      (setf leman-inbox--resize-timer
            (run-with-idle-timer 0.5 nil
                                 (lambda ()
                                   (setf leman-inbox--resize-timer nil)
                                   (when-let ((buffer (get-buffer
                                                       leman-inbox-buffer-name)))
                                     (with-current-buffer buffer
                                       (leman-inbox--render)))))))))

(add-hook 'window-size-change-functions #'leman-inbox--window-resized)

;;;; Footer

(provide 'leman-inbox)

;;; leman-inbox.el ends here
