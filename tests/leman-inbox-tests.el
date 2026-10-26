;;; leman-inbox-tests.el --- Tests for leman-inbox.el  -*- lexical-binding: t; -*-

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

;; Tests for `leman-inbox'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(require 'leman-room)
(require 'leman-inbox)

;;;; Fixtures

(defvar leman-sessions)

(defun leman-inbox-tests--ts (seconds-ago)
  "Return a timestamp in milliseconds SECONDS-AGO before now."
  (* (round (- (float-time) seconds-ago)) 1000))

(defun leman-inbox-tests--msg (id sender body &optional seconds-ago)
  "Return a \"m.room.message\" event with ID, SENDER, and BODY."
  (make-leman-event :id id :type "m.room.message" :sender sender
                    :origin-server-ts (leman-inbox-tests--ts (or seconds-ago 0))
                    :content (list (cons 'body body) (cons 'msgtype "m.text"))))

(defun leman-inbox-tests--session ()
  "Return a session with fixture rooms."
  (let* ((me (make-leman-user :id "@me:example.org" :displayname "Me"))
         (alice (make-leman-user :id "@alice:example.org" :displayname "Alice"))
         (bob (make-leman-user :id "@bob:example.org" :displayname "Bob"))
         (carol (make-leman-user :id "@carol:example.org" :displayname "Carol"))
         (dave (make-leman-user :id "@dave:example.org" :displayname "Dave"))
         (session (make-leman-session :user me))
         (dm-read
          (make-leman-room
           :id "!dm:example.org" :status 'join :display-name "Alice Chen"
           :account-data
           (list (cons "m.direct"
                       (list (cons 'type "m.direct")
                             (cons 'content (list (cons "@me:example.org"
                                                        (vector "!dm:example.org"))))))
                 (cons "m.fully_read"
                       (list (cons 'content (list (cons 'event_id "$dm-msg"))))))
           :timeline (list (leman-inbox-tests--msg "$dm-msg" alice "hello there" 3600))
           :latest-ts (leman-inbox-tests--ts 3600)))
         (group-unread
          (make-leman-room
           :id "!group:example.org" :status 'join :display-name "Team Chat"
           :unread-notifications '((notification_count . 3) (highlight_count . 0))
           :timeline (list (leman-inbox-tests--msg "$group-msg" bob "hi team" 120))
           :latest-ts (leman-inbox-tests--ts 120)))
         (mention-unread
          (make-leman-room
           :id "!mention:example.org" :status 'join :display-name "Ops Room"
           :unread-notifications '((notification_count . 5) (highlight_count . 2))
           :timeline (list (leman-inbox-tests--msg "$mention-msg" carol
                                                   "ping @me check this" 60))
           :latest-ts (leman-inbox-tests--ts 60)))
         (pinged-unread
          (make-leman-room
           :id "!pinged:example.org" :status 'join :display-name "Pinged Room"
           ;; Client-side pings: no server-side highlight.
           :local '((pings . 2))
           :timeline (list (leman-inbox-tests--msg "$pinged-msg" bob
                                                   "hey @me:example.org look" 90))
           :latest-ts (leman-inbox-tests--ts 90)))
         (edited-unread
          (make-leman-room
           :id "!edited:example.org" :status 'join :display-name "Edited Room"
           :timeline (list (make-leman-event
                            :id "$edit" :type "m.room.message" :sender dave
                            :origin-server-ts (leman-inbox-tests--ts 300)
                            :content '((m.new_content . ((body . "this is the fixed text")
                                                         (msgtype . "m.text")))))
                           (leman-inbox-tests--msg "$orig" dave "wrong text" 301))
           :latest-ts (leman-inbox-tests--ts 300)))
         (encrypted-unread
          (make-leman-room
           :id "!enc:example.org" :status 'join :display-name "Secret Room"
           :unread-notifications '((notification_count . 1) (highlight_count . 0))
           :timeline (list (make-leman-event
                            :id "$enc" :type "m.room.encrypted" :sender bob
                            :origin-server-ts (leman-inbox-tests--ts 600)
                            :content '((algorithm . "m.megolm.v1.aes-sha2"))))
           :latest-ts (leman-inbox-tests--ts 600)))
         (invite-room
          (make-leman-room
           :id "!invite:example.org" :status 'invite
           :invite-state (list (make-leman-event
                                :id "$inv" :type "m.room.member" :sender alice
                                :state-key "@me:example.org"
                                :content '((membership . "invite") (is_direct . t))))))
         (fav-read
          (make-leman-room
           :id "!fav:example.org" :status 'join :display-name "Important"
           :account-data
           (list (cons "m.tag"
                       (list (cons 'content
                                   (list (cons 'tags (list (cons 'm.favourite
                                                                 (list (cons 'order "0.5"))))))))))
           :timeline (list (leman-inbox-tests--msg "$fav-msg" me "my own message" 7200))
           :latest-ts (leman-inbox-tests--ts 7200)))
         (space-room
          (make-leman-room
           :id "!space:example.org" :status 'join :type "m.space"
           :display-name "My Space"
           :latest-ts (leman-inbox-tests--ts 100000)))
         (left-room
          (make-leman-room
           :id "!left:example.org" :status 'leave :display-name "Gone"
           :latest-ts (leman-inbox-tests--ts 10))))
    (setf (leman-session-rooms session)
          (list dm-read group-unread mention-unread pinged-unread edited-unread
                encrypted-unread invite-room fav-read space-room left-room))
    session))

(defmacro leman-inbox-tests--with-inbox (&rest body)
  "Run BODY in a rendered inbox buffer with fixture rooms."
  (declare (indent 0))
  `(let ((leman-inbox-buffer-name "*leman-inbox-tests*"))
     (setf leman-sessions (list (cons "@me:example.org"
                                      (leman-inbox-tests--session))))
     (leman-inbox :display-buffer-action nil)
     (with-current-buffer leman-inbox-buffer-name
       ,@body)))

(defun leman-inbox-tests--room-ids ()
  "Return the IDs of the rooms shown in the current inbox buffer."
  (let ((ids nil)
        (position (if (get-text-property (point-min) 'leman-inbox-item)
                      (point-min)
                    (leman-inbox--next-item-position (point-min)))))
    (while position
      (push (leman-room-id (elt (get-text-property position
                                                   'leman-inbox-item)
                                0))
            ids)
      (setf position (leman-inbox--next-item-position position)))
    (nreverse ids)))

(defmacro leman-inbox-tests--at-row (index &rest body)
  "Move to row INDEX (0-based) and run BODY."
  (declare (indent 1))
  `(progn (goto-char (point-min))
          (leman-inbox--move-row (1+ ,index))
          ,@body))

;;;; Tests

(ert-deftest leman-inbox-orders-rooms ()
  (leman-inbox-tests--with-inbox
    (should (equal (leman-inbox-tests--room-ids)
                   '("!invite:example.org"  ; Invites group.
                     "!fav:example.org"     ; Pinned group.
                     "!mention:example.org" ; Unread, most recent first.
                     "!group:example.org"
                     "!enc:example.org"
                     "!pinged:example.org"  ; Read, most recent first.
                     "!edited:example.org"
                     "!dm:example.org"
                     "!space:example.org")))
    ;; Left rooms are hidden by default.
    (should-not (member "!left:example.org" (leman-inbox-tests--room-ids)))))

(ert-deftest leman-inbox-groups ()
  (leman-inbox-tests--with-inbox
    (goto-char (point-min))
    (should (looking-at " Invites\n"))
    (should (string-match-p " Pinned" (buffer-string)))
    ;; The invite's preview names its sender.
    (should (string-match-p "Alice invited you" (buffer-string)))))

(ert-deftest leman-inbox-previews ()
  (leman-inbox-tests--with-inbox
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      ;; Edits show the replacement body.
      (should (string-match-p "this is the fixed text" text))
      ;; Encrypted messages are marked.
      (should (string-match-p "Encrypted message" text))
      ;; Media messages use text markers, not emoji.
      (should (string-match-p "\\[encrypted\\]" text))
      ;; Group messages are prefixed with the sender's name.
      (should (string-match-p "Bob: hi team" text))
      ;; Direct messages are not prefixed.
      (should (string-match-p "hello there" text))
      ;; Own messages are prefixed with "You:".
      (should (string-match-p "You: my own message" text)))))

(ert-deftest leman-inbox-badges ()
  (leman-inbox-tests--with-inbox
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      ;; Mentions show an @-badge.
      (should (string-match-p "@2" text))
      ;; Notifications show a count badge.
      (should (string-match-p " 3 " text))
      ;; Read rooms have no badge text.
      (should-not (string-match-p "hello there.*[0-9]" text)))))

(ert-deftest leman-inbox-ping-badge ()
  (leman-inbox-tests--with-inbox
    ;; The client-side-pinged room shows an @-badge without any
    ;; server-side highlight.
    (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
           (start (string-match "Pinged Room" text)))
      (should (string-match-p "@2"
                              (substring text (max 0 (- start 60))
                                         (+ start 60)))))))

(ert-deftest leman-inbox-muted-dot-badge ()
  (leman-inbox-tests--with-inbox
    ;; With `leman-room-unread-only-counts-notifications' disabled, a
    ;; room with unread messages but no notifications gets a dot badge.
    (let ((leman-room-unread-only-counts-notifications nil)
          (room (make-leman-room
                 :id "!quiet:example.org" :status 'join :display-name "Quiet"
                 :timeline (list (leman-inbox-tests--msg "$q" 
                                                         (make-leman-user
                                                          :id "@bob:example.org"
                                                          :displayname "Bob")
                                                         "psst" 30))
                 :latest-ts (leman-inbox-tests--ts 30))))
      (push room (leman-session-rooms (cdar leman-sessions)))
      (leman-inbox--render)
      (should (string-match-p "•" (buffer-substring-no-properties
                                   (point-min) (point-max)))))))

(ert-deftest leman-inbox-svg-image-normalizer ()
  ;; svg-lib 0.3 returns an image spec directly.
  (should (equal (leman-inbox--svg-image '(image :type svg :data "<svg/>"))
                 '(image :type svg :data "<svg/>")))
  ;; Some versions return a string whose display property holds it.
  (should (equal (leman-inbox--svg-image
                  (propertize " " 'display '(image :type svg :data "<svg/>")))
                 '(image :type svg :data "<svg/>")))
  ;; Anything else yields nil.
  (should (null (leman-inbox--svg-image "plain"))))

(ert-deftest leman-inbox-badge-pill-gui ()
  ;; In a GUI, the badge wraps the image produced by `svg-lib-tag',
  ;; whether that function returns an image spec (svg-lib 0.3) or a
  ;; propertized string (older versions).
  (let ((spec '(image :type svg :data "<svg/>")))
    (let ((badge (cl-letf (((symbol-function 'display-images-p)
                            (lambda (&rest _) t))
                           ((symbol-function 'svg-lib-tag)
                            (lambda (&rest _) spec)))
                   (leman-inbox--badge-pill "9" 'leman-inbox-badge))))
      (should (stringp badge))
      (should (equal (get-text-property 0 'display badge) spec)))
    (let ((badge (cl-letf (((symbol-function 'display-images-p)
                            (lambda (&rest _) t))
                           ((symbol-function 'svg-lib-tag)
                            (lambda (&rest _) (propertize " " 'display spec))))
                   (leman-inbox--badge-pill "9" 'leman-inbox-badge))))
      (should (stringp badge))
      (should (equal (get-text-property 0 'display badge) spec)))))

(ert-deftest leman-inbox-display-columns ()
  ;; Images are measured with `image-size' (stubbed: not all test
  ;; environments can decode images).
  (should (= (cl-letf (((symbol-function 'display-images-p)
                        (lambda (&rest _) t))
                       ((symbol-function 'image-size)
                        (lambda (&rest _) '(24 . 18))))
               (leman-inbox--display-columns
                (propertize " " 'display '(image :type svg :data "<svg/>"))
                (selected-frame)))
             (ceiling 24.0 (frame-char-width))))
  (should (= (leman-inbox--display-columns "abc" (selected-frame)) 3)))

(ert-deftest leman-inbox-no-emoji ()
  "Emoji are never inserted, even for media and encrypted previews."
  (leman-inbox-tests--with-inbox
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should-not (string-match-p "[📷📎🎤🎬📍🔒🖼✉️🤚]" text)))))

(ert-deftest leman-inbox-binary-body ()
  ;; A body containing raw binary data (for example image bytes that a
  ;; bridge mislabels as text) is shown as a placeholder, never as
  ;; binary garbage.
  (let* ((session (make-leman-session :user (make-leman-user :id "@me:example.org")))
         (room (make-leman-room
                :id "!bin:example.org" :status 'join :display-name "Binary Room"
                :timeline (list (make-leman-event
                                 :id "$bin" :type "m.room.message"
                                 :sender (make-leman-user :id "@u:example.org")
                                 :origin-server-ts (leman-inbox-tests--ts 60)
                                 :content (list (cons 'msgtype "m.text")
                                                (cons 'body (concat "\377\330\377\340"
                                                                    "JFIF\0ICC_PROFILE\0"
                                                                    "raw-bytes")))))
                :latest-ts (leman-inbox-tests--ts 60))))
    (setf (leman-session-rooms session) (list room)
          leman-sessions (list (cons "@me:example.org" session))
          leman-inbox-buffer-name "*leman-inbox-tests*")
    (leman-inbox :display-buffer-action nil)
    (with-current-buffer "*leman-inbox-tests*"
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "\\[binary data\\]" text))
        (should-not (string-match-p "JFIF" text))
        (should-not (string-match-p "[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]" text))))))

(ert-deftest leman-inbox-avatar-image ()
  ;; `leman-inbox--avatar-image' returns a PROPER image spec: it must
  ;; start with the `image' symbol, carry the exact dimensions, and
  ;; keep the original data.  Mutating the original spec's property
  ;; list with `setf' would prepend missing properties before the
  ;; `image' symbol, producing an invalid spec which Emacs renders as
  ;; binary garbage (the image data as text).
  (let* ((avatar (leman-inbox--avatar-image
                  '(image :type jpeg :data "jpeg-bytes"
                          :ascent center :max-width 32 :max-height 32)
                  40)))
    (should (eq (car avatar) 'image))
    (should (= (plist-get (cdr avatar) :width) 40))
    (should (= (plist-get (cdr avatar) :height) 40))
    (should (eq (plist-get (cdr avatar) :ascent) 'center))
    (should (equal (plist-get (cdr avatar) :type) 'jpeg))
    (should (equal (plist-get (cdr avatar) :data) "jpeg-bytes"))
    (should (null (plist-get (cdr avatar) :max-width)))
    (should-not (plist-member (cdr avatar) :max-height))
    ;; The plist is well-formed: no keyword appears before `image'.
    (should (null (string-match-p "^:" (prin1-to-string avatar))))))

(ert-deftest leman-inbox-avatar-malformed-display ()
  ;; A room avatar whose display property is not an image
  ;; specification (for example, a string of raw image data) is
  ;; ignored: the generated fallback avatar is used instead, and
  ;; rendering never shows raw bytes nor signals an error.
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
            ((symbol-function 'image-size) (lambda (&rest _) '(30 . 30)))
            ((symbol-function 'frame-char-width) (lambda (&rest _) 10))
            ((symbol-function 'frame-char-height) (lambda (&rest _) 20))
            ((symbol-function 'svg-lib-tag)
             (lambda (label &optional style &rest args)
               (list 'image :type 'svg :data (format "<svg>%s</svg>" label)))))
    (let* ((leman-inbox-avatar-generation t)
           (session (make-leman-session :user (make-leman-user :id "@me:example.org")))
           (string-display (make-leman-room :id "!s:example.org" :status 'join
                                            :display-name "String Display"
                                            :latest-ts (leman-inbox-tests--ts 60)))
           (non-string (make-leman-room :id "!n:example.org" :status 'join
                                        :display-name "Non String"
                                        :latest-ts (leman-inbox-tests--ts 120))))
      (setf (leman-room-avatar string-display)
            (propertize " " 'display "\377\330\377\340JFIF-raw-bytes"))
      (setf (leman-room-avatar non-string)
            (list 'image :type 'jpeg :data "raw"))
      (setf (leman-session-rooms session) (list string-display non-string)
            leman-sessions (list (cons "@me:example.org" session))
            leman-inbox-buffer-name "*leman-inbox-tests*")
      (leman-inbox :display-buffer-action nil)
      (with-current-buffer "*leman-inbox-tests*"
        (let ((text (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-match-p "String Display" text))
          (should (string-match-p "Non String" text))
          (should-not (string-match-p "JFIF" text)))))))

(ert-deftest leman-inbox-debug ()
  ;; `leman-inbox-debug' reports binary characters as text, including
  ;; the row they occur in, and binary data in the live sessions.
  (let* ((session (make-leman-session :user (make-leman-user :id "@me:example.org")))
         (room (make-leman-room
                :id "!bin:example.org" :status 'join :display-name "Binary Room"
                :timeline (list (make-leman-event
                                 :id "$bin" :type "m.room.message"
                                 :sender (make-leman-user :id "@u:example.org")
                                 :origin-server-ts (leman-inbox-tests--ts 60)
                                 :content (list (cons 'msgtype "m.text")
                                                (cons 'body (concat "\377\330\377\340"
                                                                    "JFIF\0ICC_PROFILE\0"
                                                                    "raw-bytes")))))
                :latest-ts (leman-inbox-tests--ts 60))))
    (setf (leman-session-rooms session) (list room)
          leman-sessions (list (cons "@me:example.org" session))
          leman-inbox-buffer-name "*leman-inbox-tests*")
    (leman-inbox :display-buffer-action nil)
    (with-current-buffer "*leman-inbox-tests*"
      (leman-inbox-debug)
      (with-current-buffer "*Leman Inbox Debug*"
        (let ((report (buffer-substring-no-properties (point-min) (point-max))))
          ;; The binary body is caught in the session scan, with the
          ;; room and event identified, and the rendered preview is
          ;; replaced by a placeholder.
          (should (string-match-p "Binary or control characters" report))
          (should (string-match-p "Binary-looking data in the live sessions" report))
          (should (string-match-p "event 1 (m.room.message) has a binary body of 30 chars" report))
          (should (string-match-p "room \\\"Binary Room\\\"" report))
          (should-not (string-match-p "the display name contains binary" report))
          (should (string-match-p "JFIF" report))
          (should (string-match-p "ICC_PROFILE" report))
          ;; The inbox buffer itself is clean after the placeholder fix.
          (with-current-buffer "*leman-inbox-tests*"
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "\\[binary data\\]" text))
              (should-not (string-match-p "JFIF" text)))))))))

(ert-deftest leman-inbox-icons ()
  ;; In a GUI, `leman-inbox--icon' returns a one-character string
  ;; displaying an SVG image, sized to the frame's line height.
  (let ((icon (cl-letf (((symbol-function 'display-images-p)
                         (lambda (&rest _) t)))
                (leman-inbox--icon "lock" "encrypted" (selected-frame)))))
    (should (equal (length icon) 1))
    (let ((display (get-text-property 0 'display icon)))
      (should (eq (car display) 'image))
      (should (string-match-p "stroke-width=\\\"2\\\"" (plist-get (cdr display) :data))))
    ;; And its display width is computed with `image-size'.
    (should (= (cl-letf (((symbol-function 'display-images-p)
                          (lambda (&rest _) t))
                         ((symbol-function 'image-size)
                          (lambda (&rest _) '(18 . 18))))
                 (leman-inbox--image-columns
                  (get-text-property 0 'display icon) (selected-frame)))
               18))
    ;; Non-image displays yield nil.
    (should (null (leman-inbox--image-columns "text" (selected-frame))))))

(ert-deftest leman-inbox-timestamps ()
  (leman-inbox-tests--with-inbox
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      ;; Today's messages show a clock time.
      (should (string-match-p "[0-9][0-9]:[0-9][0-9]" text))
      ;; Older messages show a weekday or date.
      (should (string-match-p "[A-Z][a-z][a-z]" text)))))

(ert-deftest leman-inbox-movement ()
  (leman-inbox-tests--with-inbox
    (goto-char (point-min))
    ;; From the top, next-row reaches the first row (the invite).
    (leman-inbox-next-row)
    (should (equal "!invite:example.org"
                   (leman-room-id (elt (leman-inbox--item-at-point) 0))))
    (leman-inbox-next-row)
    (should (equal "!fav:example.org"
                   (leman-room-id (elt (leman-inbox--item-at-point) 0))))
    ;; And previous-row comes back.
    (leman-inbox-previous-row)
    (should (equal "!invite:example.org"
                   (leman-room-id (elt (leman-inbox--item-at-point) 0))))
    ;; The first row is the boundary: point stays on it.
    (leman-inbox-previous-row)
    (should (equal "!invite:example.org"
                   (leman-room-id (elt (leman-inbox--item-at-point) 0))))
    ;; From after the last row, previous-row reaches the last row.
    (goto-char (point-max))
    (leman-inbox-previous-row)
    (should (equal "!space:example.org"
                   (leman-room-id (elt (leman-inbox--item-at-point) 0))))
    ;; The last row is the boundary: point stays on it.
    (leman-inbox-next-row)
    (should (equal "!space:example.org"
                   (leman-room-id (elt (leman-inbox--item-at-point) 0)))))
  ;; Point at a row spans both of its lines.
  (leman-inbox-tests--with-inbox
    (leman-inbox-tests--at-row 0
      (let ((bounds (leman-inbox--row-bounds)))
        (should (and bounds (< (car bounds) (cdr bounds))))
        (should (equal bounds (leman-inbox--hl-line-range)))))))

(ert-deftest leman-inbox-filter ()
  (leman-inbox-tests--with-inbox
    (leman-inbox-filter "Chat")
    (should (equal (leman-inbox-tests--room-ids) '("!group:example.org")))
    (leman-inbox-filter "")
    (should (> (length (leman-inbox-tests--room-ids)) 1))))

(ert-deftest leman-inbox-mark-read ()
  (leman-inbox-tests--with-inbox
    (leman-inbox-tests--at-row 2  ; The mention room.
      (let (marked-room marked-event)
        (cl-letf (((symbol-function 'leman-room-mark-read)
                   (lambda (room _session &key read-event &rest _)
                     (setf marked-room (leman-room-id room)
                           marked-event read-event))))
          (leman-inbox-mark-read)
          (should (equal marked-room "!mention:example.org"))
          ;; The read receipt is up to the room's latest event.
          (should (equal (leman-event-id marked-event) "$mention-msg")))))))

(ert-deftest leman-inbox-open ()
  (leman-inbox-tests--with-inbox
    (let (opened marker-called space-opened)
      (cl-letf (((symbol-function 'leman-view-room)
                 (lambda (room _session) (setf opened (leman-room-id room))))
                ((symbol-function 'leman-room-goto-fully-read-marker)
                 (lambda () (setf marker-called t)))
                ((symbol-function 'leman-view-space)
                 (lambda (room _session)
                   (setf space-opened (leman-room-id room)))))
        ;; Opening an unread room jumps to its first unread message.
        (leman-inbox-tests--at-row 2
          (leman-inbox-open)
          (should (equal opened "!mention:example.org"))
          (should marker-called))
        ;; Opening a read room does not.
        (leman-inbox-tests--at-row 7
          (setf opened nil marker-called nil)
          (leman-inbox-open)
          (should (equal opened "!dm:example.org"))
          (should-not marker-called))
        ;; Spaces are opened with `leman-view-space'.
        (leman-inbox-tests--at-row 8
          (leman-inbox-open)
          (should (equal space-opened "!space:example.org"))))))

  (leman-inbox-tests--with-inbox
    (should-error (leman-inbox-open))))  ; Point on the heading.

(ert-deftest leman-inbox-next-unread ()
  (leman-inbox-tests--with-inbox
    (let (opened marker-called)
      (cl-letf (((symbol-function 'leman-view-room)
                 (lambda (room _session) (setf opened (leman-room-id room))))
                ((symbol-function 'leman-room-goto-fully-read-marker)
                 (lambda () (setf marker-called t))))
        ;; From the top, the next unread room is the mention room.
        (goto-char (point-min))
        (leman-inbox-next-unread)
        (should (equal opened "!mention:example.org"))
        (should marker-called))))
  (leman-inbox-tests--with-inbox
    (let (opened)
      (cl-letf (((symbol-function 'leman-view-room)
                 (lambda (room _session) (setf opened (leman-room-id room))))
                ((symbol-function 'leman-room-goto-fully-read-marker)
                 #'ignore))
        ;; With no unread rooms, say so.
        (setf leman-inbox-filter-regexp nil)
        ;; Mark every room read by clearing notifications and skipping
        ;; the timeline check.
        (let ((leman-room-unread-only-counts-notifications t))
          (cl-loop for (_id . session) in leman-sessions
                   do (cl-loop for room in (leman-session-rooms session)
                               do (setf (leman-room-unread-notifications room) nil)))
          (leman-inbox--render)
          (goto-char (point-min))
          (leman-inbox-next-unread)
          (should (null opened)))))))

(ert-deftest leman-inbox-kill-room-buffer ()
  (leman-inbox-tests--with-inbox
    (leman-inbox-tests--at-row 2
      (let* ((item (leman-inbox--item-at-point))
             (room (elt item 0))
             (room-buffer (get-buffer-create "*leman-inbox-tests-room*")))
        (setf (alist-get 'buffer (leman-room-local room)) room-buffer)
        (leman-inbox-kill-room-buffer)
        (should-not (buffer-live-p room-buffer))))))

(ert-deftest leman-inbox-room-at-point ()
  (leman-inbox-tests--with-inbox
    (leman-inbox-tests--at-row 2
      (should (equal "!mention:example.org"
                     (leman-room-id (leman--room-at-point)))))))

(ert-deftest leman-inbox-empty ()
  (setf leman-sessions
        (list (cons "@me:example.org"
                    (make-leman-session
                     :user (make-leman-user :id "@me:example.org")))))
  (let ((leman-inbox-buffer-name "*leman-inbox-tests-empty*"))
    (leman-inbox :display-buffer-action nil)
    (with-current-buffer leman-inbox-buffer-name
      (should (string-match-p "No chats" (buffer-substring-no-properties
                                          (point-min) (point-max)))))))

(ert-deftest leman-inbox--display-room-reuses-or-splits-windows ()
  "The default display action reuses a window already showing a
Leman room buffer, or splits off a new one; the inbox's window is
never used."
  (let* ((room (make-leman-room :id "!room:example.org"))
         (inbox (get-buffer-create " *leman-inbox-display-test-inbox*"))
         (room-buffer (get-buffer-create " *leman-inbox-display-test-room*"))
         (new-room (get-buffer-create " *leman-inbox-display-test-new*")))
    (unwind-protect
        (with-current-buffer room-buffer
          (setf leman-room room)
          (with-current-buffer inbox
            ;; A window already showing a room buffer is reused.
            (delete-other-windows)
            (let* ((inbox-window (selected-window))
                   (room-window (split-window inbox-window)))
              (set-window-buffer room-window room-buffer)
              (set-window-buffer inbox-window inbox)
              (let ((displayed (display-buffer new-room
                                               '((leman-inbox--display-room)))))
                (should (eq displayed room-window))
                (should (eq (window-buffer room-window) new-room))
                ;; The inbox keeps its window and its buffer.
                (should (eq (window-buffer inbox-window) inbox))))
            ;; Without a room window, a new one is split off.
            (delete-other-windows)
            (let* ((inbox-window (selected-window))
                   (displayed (display-buffer new-room
                                              '((leman-inbox--display-room)))))
              (should (window-live-p displayed))
              (should-not (eq displayed inbox-window))
              (should (eq (window-buffer displayed) new-room))
              (should (eq (window-buffer inbox-window) inbox)))))
      (dolist (buffer (list inbox room-buffer new-room))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest leman-inbox-typing-preview ()
  "Rooms with typing members show it in the preview."
  (should (equal (leman-inbox--typing-preview '("Alice"))
                 "Alice is typing…"))
  (should (equal (leman-inbox--typing-preview '("Alice" "Bob"))
                 "Alice and Bob are typing…"))
  (should (equal (leman-inbox--typing-preview '("Alice" "Bob" "Carol"))
                 "Several people are typing…"))
  ;; No typing members: no typing preview (nil falls through to the
  ;; latest message).
  (should-not (leman-inbox--typing-preview nil))
  (leman-inbox-tests--with-inbox
    (let* ((session (cdr (car leman-sessions)))
           (room (cl-find "!dm:example.org" (leman-session-rooms session)
                          :key #'leman-room-id :test #'equal)))
      (setf (map-elt (leman-room-local room) 'typing) (list "Alice Chen"))
      (leman-inbox--render)
      (should (string-match-p "Alice Chen is typing…" (buffer-string))))))

(ert-deftest leman-inbox-highlight-mentions ()
  "Previews highlight mentions of the local user."
  (let* ((room (make-leman-room :id "!room:example.org" :display-name "Room"))
         (session (make-leman-session
                   :user (make-leman-user :id "@me:x.org" :displayname "Me")))
         (string (leman-inbox--highlight-mentions
                  "ping @room and Me please" room session)))
    (should (equal (get-text-property (string-match-p "@room" string) 'face string)
                   'leman-inbox-preview-mention))
    (should (equal (get-text-property (string-match-p "Me" string) 'face string)
                   'leman-inbox-preview-mention))
    (should-not (get-text-property (string-match-p "please" string) 'face string))))

;;;; Footer

(provide 'leman-inbox-tests)

;;; leman-inbox-tests.el ends here
