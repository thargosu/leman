;;; leman-tests.el --- Tests for leman                  -*- lexical-binding: t; -*-

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

(require 'ert)
(require 'map)

(require 'leman-lib)
(require 'leman-room)
(require 'leman-room-list)
(require 'leman-tabulated-room-list)

;;;; Helpers

(defun leman-tests--member-event (id state-key old new &optional kicked-p avatar-differs-p)
  "Return a membership event for testing.
OLD and NEW are the previous and new membership strings.  KICKED-P
means the sender differs from the state-key (i.e. another user
did the action).  AVATAR-DIFFERS-P makes the event's new avatar
URL differ from the previous one."
  (make-leman-event
   :id (format "%s-%s" id state-key)
   :state-key state-key
   :sender (make-leman-user :id (if kicked-p "@admin:example.com" state-key))
   :origin-server-ts id
   :type "m.room.member"
   :content `((membership . ,new)
              (avatar_url . ,(if avatar-differs-p "avatar-new" "avatar-same"))
              (displayname . nil))
   :unsigned `((prev_content . ((membership . ,old)
                                (avatar_url . "avatar-same")
                                (displayname . nil))))))

;;;; Tests

(ert-deftest leman-api--query-string-from-json-params ()
  ;; Params decoded from JSON objects are dotted pairs, which
  ;; `url-build-query-string' (Emacs >=29) rejects; `leman-api' must
  ;; rewrite them as one-element lists (and keep proper lists as-is).
  (let* ((urls nil)
         (session (make-leman-session
                   :server (make-leman-server :uri-prefix "https://example.org"))))
    (cl-letf (((symbol-function #'plz)
               (lambda (_method url &rest _args) (push url urls) nil)))
      (leman-api session "room_keys/keys"
        :version "v3"
        :params '((version . "12132284")))
      (leman-api session "messages"
        :version "v3"
        :params '(("dir" "f") ("limit" "200"))))
    (should (equal (nreverse urls)
                   (list "https://example.org/_matrix/client/v3/room_keys/keys?version=12132284"
                         "https://example.org/_matrix/client/v3/messages?dir=f&limit=200")))))

(ert-deftest leman--format-body-mentions ()
  (let ((room (make-leman-room
               :members (map-into
                         `(("@foo:matrix.org" . ,(make-leman-user :id "@foo:matrix.org"
                                                                  :displayname "foo"))
                           ("@bar:matrix.org" . ,(make-leman-user :id "@bar:matrix.org"
                                                                  :displayname "bar")))
                         '(hash-table :test equal)))))
    (should (equal (leman--format-body-mentions "@foo: hi" room)
                   "<a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a>: hi"))
    (should (equal (leman--format-body-mentions "@foo:matrix.org: hi" room)
                   "<a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a>: hi"))
    (should (equal (leman--format-body-mentions "foo: hi" room)
                   "<a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a>: hi"))
    (should (equal (leman--format-body-mentions "@foo and @bar:matrix.org: hi" room)
                   "<a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a> and <a href=\"https://matrix.to/#/@bar:matrix.org\">bar</a>: hi"))
    (should (equal (leman--format-body-mentions "foo: how about you and @bar ..." room)
                   "<a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a>: how about you and <a href=\"https://matrix.to/#/@bar:matrix.org\">bar</a> ..."))
    (should (equal (leman--format-body-mentions "Hello, @foo:matrix.org." room)
                   "Hello, <a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a>."))
    (should (equal (leman--format-body-mentions "Hello, @foo:matrix.org, how are you?" room)
                   "Hello, <a href=\"https://matrix.to/#/@foo:matrix.org\">foo</a>, how are you?"))))

(ert-deftest leman-room--pair-events ()
  "Test pairing of membership events by state-key."
  (let* ((join-alice (leman-tests--member-event 1 "@alice:example.com" nil "join"))
         (join-bob (leman-tests--member-event 2 "@bob:example.com" nil "join"))
         (leave-alice (leman-tests--member-event 3 "@alice:example.com" "join" "leave"))
         (leave-bob (leman-tests--member-event 4 "@bob:example.com" "join" "leave"))
         (leave-erin (leman-tests--member-event 5 "@erin:example.com" "join" "leave")))
    ;; Paired events are returned in the events' order.
    (should (equal (leman-room--pair-events (list join-alice join-bob)
                                            (list leave-alice leave-bob))
                   (list (list leave-alice leave-bob) nil nil)))
    ;; Unpaired events remain in their respective lists.
    (should (equal (leman-room--pair-events (list join-alice)
                                            (list leave-bob leave-erin))
                   (list nil (list join-alice) (list leave-bob leave-erin))))
    ;; An OTHERS event's state-key is consumed once, so later EVENTS
    ;; events having that state-key are dropped.
    (let ((join-alice-again (leman-tests--member-event 6 "@alice:example.com" "invite" "join")))
      (should (equal (leman-room--pair-events (list join-alice join-alice-again)
                                              (list leave-alice))
                     (list (list leave-alice) nil nil))))))

(ert-deftest leman-room--format-membership-events ()
  "Test membership events summary formatting."
  ;; Bind a fresh user table so the test does not depend on (or
  ;; leak into) leman's global `leman-users'.
  (let* ((leman-users (make-hash-table :test #'equal))
         (room (make-leman-room :id "!room:example.com"))
         (leman-room room)
        (format-summary (lambda (&rest events)
                          (substring-no-properties
                           (leman-room--format-membership-events
                            (make-leman-room-membership-events :events events)
                            room)))))
    ;; A single event is formatted by `leman-room--format-member-event'.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" nil "join"))
                   "@alice:example.com joined"))
    ;; Users are listed in events order within a category.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" nil "join")
                            (leman-tests--member-event 2 "@bob:example.com" nil "join")
                            (leman-tests--member-event 3 "@carol:example.com" nil "join"))
                   "Membership: 3 joined (@alice:example.com, @bob:example.com, @carol:example.com)."))
    ;; Categories are listed in a fixed order.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@carol:example.com" nil "join")
                            (leman-tests--member-event 2 "@bob:example.com" "join" "leave")
                            (leman-tests--member-event 3 "@alice:example.com" nil "join")
                            (leman-tests--member-event 4 "@dave:example.com" "join" "leave"))
                   "Membership: 2 joined (@carol:example.com, @alice:example.com); 2 left (@bob:example.com, @dave:example.com)."))
    ;; A join followed by a leave is counted as "joined and left".
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" nil "join")
                            (leman-tests--member-event 2 "@alice:example.com" "join" "leave"))
                   "Membership: 1 joined and left (@alice:example.com)."))
    ;; Events that are both joined and rejoined are counted as rejoined.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" nil "join")
                            (leman-tests--member-event 2 "@alice:example.com" "leave" "join"))
                   "Membership: 1 rejoined (@alice:example.com)."))
    ;; A kick followed by a rejoin is not also counted as leaving.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "join" "leave" 'kicked-p)
                            (leman-tests--member-event 2 "@alice:example.com" "leave" "join"))
                   "Membership: 1 was kicked and rejoined (@alice:example.com)."))
    ;; A single kick event is formatted by `leman-room--format-member-event'.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "join" "leave" 'kicked-p))
                   "@admin:example.com kicked @alice:example.com"))
    ;; A rejoin followed by a leave.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "leave" "join")
                            (leman-tests--member-event 2 "@alice:example.com" "join" "leave"))
                   "Membership: 1 rejoined and left (@alice:example.com)."))
    ;; Invitations.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "leave" "invite")
                            (leman-tests--member-event 2 "@bob:example.com" "leave" "invite"))
                   "Membership: 2 invited (@alice:example.com, @bob:example.com)."))
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "invite" "leave")
                            (leman-tests--member-event 2 "@bob:example.com" "invite" "leave"))
                   "Membership: 2 rejected invitation (@alice:example.com, @bob:example.com)."))
    ;; Bans and unbans.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "leave" "ban")
                            (leman-tests--member-event 2 "@bob:example.com" "invite" "ban"))
                   "Membership: 2 banned (@alice:example.com, @bob:example.com)."))
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "ban" "leave")
                            (leman-tests--member-event 2 "@bob:example.com" "ban" "leave"))
                   "Membership: 2 unbanned (@alice:example.com, @bob:example.com)."))
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "join" "ban" 'kicked-p)
                            (leman-tests--member-event 2 "@bob:example.com" "join" "ban" 'kicked-p))
                   "Membership: 2 kicked and banned (@alice:example.com, @bob:example.com)."))
    ;; Ban transitions which the summary does not classify are omitted.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" nil "ban")
                            (leman-tests--member-event 2 "@bob:example.com" "join" "leave"))
                   "Membership: 1 left (@bob:example.com)."))
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "ban" "ban")
                            (leman-tests--member-event 2 "@bob:example.com" "join" "leave"))
                   "Membership: 1 left (@bob:example.com)."))
    ;; Name and avatar changes.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "join" "join")
                            (leman-tests--member-event 2 "@bob:example.com" "join" "join"))
                   "Membership: 2 changed name (@alice:example.com, @bob:example.com)."))
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "join" "join" nil 'avatar-differs-p)
                            (leman-tests--member-event 2 "@bob:example.com" "join" "join" nil 'avatar-differs-p))
                   "Membership: 2 changed avatar (@alice:example.com, @bob:example.com)."))
    ;; Unclassifiable events are omitted.
    (should (equal (funcall format-summary
                            (leman-tests--member-event 1 "@alice:example.com" "leave" "knock")
                            (leman-tests--member-event 2 "@bob:example.com" nil "join"))
                   "Membership: 1 joined (@bob:example.com)."))
    ;; The summary type is propertized with the bold face.
    (let* ((raw (leman-room--format-membership-events
                 (make-leman-room-membership-events
                  :events (list (leman-tests--member-event 1 "@alice:example.com" nil "join")
                                (leman-tests--member-event 2 "@bob:example.com" nil "join")))
                 room))
           (pos (string-search "joined" raw)))
      (should (eq 'bold (get-text-property pos 'face raw))))))

(ert-deftest leman-room--format-single-unrecognized-membership ()
  ;; A single membership event with an unrecognized membership (e.g.
  ;; "knock", rooms v8+) must render instead of crashing: the
  ;; per-event formatter used `pcase-exhaustive', which signaled.
  (let ((leman-users (make-hash-table :test #'equal))
        (room (make-leman-room :id "!room:example.com"))
        (leman-room (make-leman-room :id "!room:example.com")))
    ;; "knock" (rooms v8+).
    (should (equal (substring-no-properties
                    (leman-room--format-member-event
                     (leman-tests--member-event 1 "@alice:example.com" nil "knock") room))
                   "@alice:example.com sent unrecognized membership event for @alice:example.com"))
    ;; A garbage (nil) membership too.
    (should (equal (substring-no-properties
                    (leman-room--format-member-event
                     (leman-tests--member-event 1 "@alice:example.com" nil nil) room))
                   "@alice:example.com sent unrecognized membership event for @alice:example.com"))))

(ert-deftest leman-room--initial-footer ()
  "Test initial room buffer footer."
  (let ((plain (make-leman-room :id "!room:example.com"))
        (invited (make-leman-room :id "!room:example.com" :status 'invite))
        (space (make-leman-room :id "!room:example.com" :type "m.space"))
        (invited-space (make-leman-room :id "!room:example.com" :status 'invite :type "m.space")))
    ;; A plain room has an empty footer.
    (should (string-empty-p (leman-room--initial-footer plain)))
    ;; An invited room's footer offers to join.
    (let ((footer (leman-room--initial-footer invited))
          (pos (string-search "[Join this room]" (leman-room--initial-footer invited))))
      (should (string-search "invited to this room" (substring-no-properties footer)))
      (should pos)
      (should (get-text-property pos 'button footer))
      (should (functionp (get-text-property pos 'action footer))))
    ;; A space's footer offers to view its rooms.
    (let ((footer (leman-room--initial-footer space))
          (pos (string-search "[View rooms in this space]" (leman-room--initial-footer space))))
      (should (string-search "grouping of other rooms" (substring-no-properties footer)))
      (should pos)
      (should (get-text-property pos 'button footer))
      (should (functionp (get-text-property pos 'action footer))))
    ;; For an invited space, the invitation takes precedence.
    (should (string-search "invited to this room"
                           (substring-no-properties (leman-room--initial-footer invited-space))))))

(defun leman-tests--taxy-items (taxy)
  "Return all of TAXY's items, including those in its sub-taxys."
  (append (taxy-items taxy)
          (cl-loop for sub-taxy in (taxy-taxys taxy)
                   append (leman-tests--taxy-items sub-taxy))))

(defun leman-tests--taxy-named (name taxy)
  "Return TAXY's descendant (or itself) named NAME, or nil."
  (if (equal name (substring-no-properties (taxy-name taxy)))
      taxy
    (cl-loop for sub-taxy in (taxy-taxys taxy)
             when (leman-tests--taxy-named name sub-taxy)
             return it)))

(ert-deftest leman-push-joined-room-events-account-data-keys-match-readers ()
  ;; Room account data is stored by the sync push and read back by
  ;; the room buffer's read-marker restoration and the room
  ;; display-name code with STRING keys (e.g. "m.fully_read"): the
  ;; stored keys must be strings, or read markers can never be
  ;; restored.
  (let* ((session (make-leman-session))
         (fully-read (list (cons 'type "m.fully_read")
                           (cons 'content (list (cons 'event_id "$read-up-to")))))
         (name-override (list (cons 'type "org.matrix.msc3015.m.room.name.override")
                              (cons 'content (list (cons 'name "Override")))))
         (room (progn
                 (setf (leman-session-events session) (make-hash-table :test #'equal))
                 (leman--push-joined-room-events
                  session
                  (cons (intern "!room:x.org")
                        (list (cons 'account_data
                                    (list (cons 'events
                                                (vector fully-read name-override)))))))
                 (car (leman-session-rooms session))))
         ;; The exact lookups the room buffer and display-name code
         ;; use.
         (fully-read-lookup (alist-get "m.fully_read"
                                       (leman-room-account-data room)
                                       nil nil #'equal))
         (override-lookup (alist-get "org.matrix.msc3015.m.room.name.override"
                                     (leman-room-account-data room)
                                     nil nil #'equal)))
    (should (equal (map-nested-elt fully-read-lookup '(content event_id))
                   "$read-up-to"))
    (should (equal (map-nested-elt override-lookup '(content name))
                   "Override"))))

(ert-deftest leman-room-list--build-taxy ()
  "Test building the room list taxy."
  (let* ((room-old (make-leman-room :id "!old:example.com" :latest-ts 100))
         (room-new (make-leman-room :id "!new:example.com" :latest-ts 200))
         (room-invited (make-leman-room :id "!invited:example.com"
                                        :latest-ts 300 :status 'invite))
         (room-left (make-leman-room :id "!left:example.com"
                                     :latest-ts 50 :status 'leave))
         (room-buffered (make-leman-room :id "!buffered:example.com" :latest-ts 400
                                         :local (list (cons 'buffer
                                                            (get-buffer-create
                                                             " *leman-test-room*")))))
         (session (make-leman-session :user (make-leman-user :id "@me:example.com")))
         (taxy (leman-room-list--build-taxy
                (list (vector room-old session) (vector room-new session)
                      (vector room-invited session) (vector room-left session)
                      (vector room-buffered session))
                leman-room-list-default-keys
                #'identity))
         (items (mapcar (lambda (item) (elt item 0))
                        (leman-tests--taxy-items taxy))))
    (should (equal "Leman Rooms" (taxy-name taxy)))
    (dolist (room (list room-old room-new room-invited room-left room-buffered))
      (should (member room items)))
    ;; Rooms are sorted latest-first.
    (should (< (cl-position room-new items :test #'equal)
               (cl-position room-old items :test #'equal)))
    ;; Grouping: invitations, left rooms, and rooms with open
    ;; buffers land in their own groups (and only there).
    (dolist (group (list (list "Invited" room-invited room-old)
                         (list "[Left]" room-left room-old)
                         (list "Buffers" room-buffered room-old)))
      (pcase-let* ((`(,name ,in ,out) group)
                   (sub-taxy (leman-tests--taxy-named name taxy))
                   (sub-items (when sub-taxy
                                (mapcar (lambda (item) (elt item 0))
                                        (leman-tests--taxy-items sub-taxy)))))
        (should sub-taxy)
        (should (member in sub-items))
        (should-not (member out sub-items))))))

(ert-deftest leman-tabulated-room-list--entry ()
  "Test building a tabulated room list entry."
  (let* ((session (make-leman-session :user (make-leman-user :id "@me:example.com")))
         (room (make-leman-room :id "!room:example.com"))
         (entry (leman-tabulated-room-list--entry session room))
         (name (elt (elt entry 1) 5)))
    ;; The entry identifies the room and has one column per format.
    (should (eq room (car entry)))
    (should (= (length (elt entry 1)) 10))
    ;; A plain room's name has only the base face.
    (should (equal '(:inherit (leman-tabulated-room-list-name))
                   (get-text-property 0 'face (car name))))))

(ert-deftest leman-tabulated-room-list--entry-membership-faces ()
  "Test that invited and left rooms are face-modified.
This checks the 'leave branch, which formerly passed its
arguments to `cons' in reverse, and the `leman-room-status'
slot, which was read from the wrong slot, so these branches
never applied."
  (let* ((session (make-leman-session :user (make-leman-user :id "@me:example.com")))
         (invited-room (make-leman-room :id "!invited:example.com" :status 'invite))
         (left-room (make-leman-room :id "!left:example.com" :status 'leave))
         (invited-entry (leman-tabulated-room-list--entry session invited-room))
         (left-entry (leman-tabulated-room-list--entry session left-room))
         (invited-name (car (elt (elt invited-entry 1) 5)))
         (left-name (car (elt (elt left-entry 1) 5))))
    ;; Topics are prefixed, and the name's face inherits the membership face.
    (should (string-search "[invited]"
                           (substring-no-properties (elt (elt invited-entry 1) 6))))
    (should (member 'leman-tabulated-room-list-invited
                    (map-elt (get-text-property 0 'face invited-name) :inherit)))
    (should (string-search "[left]"
                           (substring-no-properties (elt (elt left-entry 1) 6))))
    (should (member 'leman-tabulated-room-list-left
                    (map-elt (get-text-property 0 'face left-name) :inherit)))))

(ert-deftest leman-room--render-html-spoilers ()
  "Test that Matrix spoilers are rendered and hidden.
Both the valueless attribute form (as sent by, e.g. Element's
/spoiler command) and the reasoned form are covered."
  (let ((string (let ((leman-room-use-variable-pitch nil))
                  (leman-room--render-html
                   "Before <span data-mx-spoiler>the secret</span> mid <span data-mx-spoiler=\"plot twist\">snape did it</span>"
                   nil))))
    ;; Labels and content are rendered in order.
    (should (string-match "\\[spoiler\\] the secret" string))
    (should (string-match "\\[spoiler: plot twist\\] snape did it" string))
    ;; Content is hidden and toggleable.
    (let ((cbeg (next-single-property-change 0 'leman-spoiler-content string)))
      (should cbeg)
      (should (eq (get-text-property cbeg 'invisible string) 'leman-spoiler))
      (should (get-text-property cbeg 'keymap string))
      (should (equal (substring-no-properties string cbeg
                                               (next-single-property-change cbeg 'leman-spoiler-content string))
                     "the secret"))
      ;; Labels are clickable too.
      (let ((label-beg (string-match "\\[spoiler\\]" string)))
        (should (get-text-property label-beg 'keymap string))
        (should (get-text-property label-beg 'face string))))))

(ert-deftest leman-room--toggle-spoiler-at-point ()
  "Test that toggling reveals and hides spoiler content.
Toggling works both from within the content and from its label."
  (let ((string (let ((leman-room-use-variable-pitch nil))
                  (leman-room--render-html
                   "<span data-mx-spoiler>hidden words</span>" nil))))
    (with-temp-buffer
      (insert string)
      ;; From within the content.
      (goto-char (next-single-property-change (point-min) 'leman-spoiler-content))
      (should (get-text-property (point) 'invisible))
      (leman-room--toggle-spoiler-at-point)
      (should-not (get-text-property (point) 'invisible))
      (leman-room--toggle-spoiler-at-point)
      (should (eq (get-text-property (point) 'invisible) 'leman-spoiler))
      ;; From the label.
      (goto-char (point-min))
      (search-forward "[spoiler]")
      (leman-room--toggle-spoiler-at-point)
      (should-not (get-text-property (point) 'invisible)))))

(ert-deftest leman-room--spoiler-keymap-and-RET ()
  "Test that the spoiler keymap works with both mouse and keyboard.
RET must invoke the command without the \"e\" interactive spec's
\"must be bound to an event with parameters\" error, and mouse-1
must not be bound, because the `follow-link' property translates
quick mouse-1 clicks to mouse-2 clicks before key lookup."
  (should (eq (lookup-key leman-room-spoiler-keymap (kbd "RET"))
              #'leman-room-toggle-spoiler))
  (should (eq (lookup-key leman-room-spoiler-keymap [mouse-2])
              #'leman-room-toggle-spoiler))
  (should-not (lookup-key leman-room-spoiler-keymap [mouse-1]))
  (let ((string (let ((leman-room-use-variable-pitch nil))
                  (leman-room--render-html
                   "<span data-mx-spoiler>hidden words</span>" nil))))
    (with-temp-buffer
      (insert string)
      ;; Pressing RET on the label toggles the spoiler.
      (goto-char (point-min))
      (search-forward "[spoiler]")
      (backward-char 3)
      (let ((last-command-event ?\r))
        (call-interactively #'leman-room-toggle-spoiler))
      (let ((cbeg (next-single-property-change (point-min) 'leman-spoiler-content)))
        (should-not (get-text-property cbeg 'invisible))))))

(ert-deftest leman--unread-room-names-escape-percent ()
  "Test that unread room names are escaped for the mode line.
The indicator string is interpreted as a mode-line format string,
so an unescaped \"%\" in a room name would be treated as a
%-construct (invalid ones are displayed as \"*invalid*\")."
  (let* ((room (make-leman-room :id "!room:example.com"
                                :status 'join
                                :display-name "100% done"
                                :unread-notifications '((notification_count . 3)
                                                        (highlight_count . 0))))
         (session (make-leman-session :rooms (list room))))
    (let ((leman-sessions (list (cons "!session:example.com" session))))
      (should (equal (leman--unread-room-names 1)
                     '("100%% done 3"))))))

(defun leman-tests--reacted-event (reaction-key &optional sender-id)
  "Return an event with a reaction of REACTION-KEY by SENDER-ID.
The reaction is stored in the event's local reactions list, as
`leman-room--format-reactions' expects."
  (let ((event (make-leman-event :id "$event"))
        (sender (make-leman-user :id (or sender-id "@other:example.com"))))
    (setf (map-elt (leman-event-local event) 'reactions)
          (list (make-leman-event
                 :sender sender
                 :content `((m.relates_to . ((rel_type . "m.annotation")
                                             (event_id . "$event")
                                             (key . ,reaction-key)))))))
    event))

(defun leman-tests--thread-reply-event (id root-id &optional ts)
  "Return a thread reply event with ID relating to ROOT-ID."
  (make-leman-event :id id
                    :origin-server-ts (or ts 1000)
                    :sender (make-leman-user :id "@other:example.com")
                    :content `((msgtype . "m.text")
                               (body . ,(format "reply %s" id))
                               (m.relates_to . ((rel_type . "m.thread")
                                                (event_id . ,root-id))))))

(defun leman-tests--format-reactions ()
  "Return formatted reactions of a reacted event with test data."
  (let* ((room (make-leman-room :id "!room:example.com"))
         (event (leman-tests--reacted-event "👍")))
    (leman-room--format-reactions event room)))

(ert-deftest leman-room--format-reactions-store-key-property ()
  "Test that reactions store their raw key in a text property.
The property is used to recover the key when the button is
pushed (necessary for custom-emoji reactions, whose keys do not
appear in the buffer text)."
  (let ((leman-session (make-leman-session
                        :user (make-leman-user :id "@me:example.com"))))
    (let ((string (leman-tests--format-reactions)))
      (should (equal (get-text-property (string-match "👍" string) 'leman-reaction-key string)
                     "👍")))))

(ert-deftest leman-room--format-reactions-show-count-not-names ()
  "Reactions show the number of senders, not their names.
Names are available in the reaction's tooltip instead."
  (let ((leman-session (make-leman-session
                        :user (make-leman-user :id "@me:example.com"))))
    (let ((string (leman-tests--format-reactions)))
      (should (string-match-p "👍 (1)" string))
      (should-not (string-match-p "Alice\\|Bob\\|@other" string))
      ;; The names are delivered through the reaction's tooltip.
      (should (functionp (get-text-property (string-match "👍" string)
                                            'help-echo string))))))

(ert-deftest leman-room--format-reactions-tooltip-shows-shortcode ()
  "Reaction tooltips show Unicode and custom-emoji shortcodes."
  (let ((leman-session (make-leman-session
                        :user (make-leman-user :id "@me:example.com")))
        (leman-room-images nil))
    (dolist (data `(("👍" . ":thumbs_up_sign:")
                    ("mxc://example.com/party" . ":party_parrot:")))
      (let* ((key (car data))
             (shortcode (cdr data))
             (room (make-leman-room
                    :id "!room:example.com"
                    :state (when (string-prefix-p "mxc://" key)
                             (list (make-leman-event
                                    :type "im.ponies.room_emotes"
                                    :content '((images . ((party_parrot . ((url . "mxc://example.com/party")))))))))))
             (string (leman-room--format-reactions (leman-tests--reacted-event key) room))
             (pos (or (string-match (regexp-quote key) string)
                      (next-single-property-change 0 'leman-reaction-key string)))
             (tooltip (get-text-property pos 'help-echo string)))
        (with-temp-buffer
          (setq-local leman-room room)
          (should (equal (funcall tooltip nil (current-buffer) pos)
                         (concat shortcode ": @other:example.com"))))))))

(ert-deftest leman-room--format-reactions-custom-emoji ()
  "Test that custom-emoji reaction keys (mxc URIs) are handled.
When the emoji's image is available (from the URL cache), it is
rendered in place of the key; otherwise, the mxc URI is shown as
the key.  In either case, the raw key is stored in a text
property for toggling."
  (let ((leman-session (make-leman-session
                        :user (make-leman-user :id "@me:example.com")
                        :server (make-leman-server :uri-prefix "https://matrix.example.com"))))
    ;; Without the image, the key is shown as the mxc URI.  Bind
    ;; `leman-room-images' explicitly: when it defaults to t (on an
    ;; ImageMagick-capable build), the formatter would try to fetch
    ;; the mxc URI for real.
    (let* ((leman-room-images nil)
           (event (leman-tests--reacted-event "mxc://example.com/emoji"))
           (room (make-leman-room :id "!room:example.com"))
           (string (leman-room--format-reactions event room)))
      ;; Without the image, the key is shown as the mxc URI.
      (should (string-match-p "mxc://example.com/emoji" string))
      (should (equal (get-text-property (string-match "mxc://" string) 'leman-reaction-key string)
                     "mxc://example.com/emoji")))
    ;; With the image available, the key is an image, not the mxc
    ;; URI.  Test both shapes of `shr-get-image-data' return value:
    ;; a (DATA CONTENT-TYPE) list in Emacs 30+, and a DATA string
    ;; in older versions.
    (dolist (shr-data '("mock-image-data" ("mock-image-data" image/gif)))
      (let* ((event (leman-tests--reacted-event "mxc://example.com/emoji"))
             (room (make-leman-room :id "!room:example.com"))
             (leman-room-images t)
             (string (cl-letf (((symbol-function 'leman-room--shr-image-data)
                                (lambda (_url) shr-data))
                               ((symbol-function 'leman-room--fetch-html-image) #'ignore)
                               ((symbol-function 'create-image)
                                (lambda (&rest _) 'mock-image)))
                       (leman-room--format-reactions event room))))
        ;; With the image available, the key is an image, not the mxc URI.
        (should-not (string-match-p "mxc://example.com/emoji" string))
        (should (eq (get-text-property (next-single-property-change 0 'display string)
                                       'display string)
                    'mock-image))
        (should (equal (get-text-property (next-single-property-change 0 'leman-reaction-key string)
                                          'leman-reaction-key string)
                       "mxc://example.com/emoji"))))))

(ert-deftest leman-room--thread-data ()
  "Test storing, deduplicating, and replacing thread events."
  (let ((room (make-leman-room :id "!room:example.com"))
        (root-id "$root"))
    ;; Replies are stored and deduplicated.
    (leman-room--add-thread-event (leman-tests--thread-reply-event "$reply1" root-id) room)
    (leman-room--add-thread-event (leman-tests--thread-reply-event "$reply1" root-id) room)
    (leman-room--add-thread-event (leman-tests--thread-reply-event "$reply2" root-id 2000) room)
    (should (= 2 (length (leman-room--thread-events room root-id))))
    (should-not (leman-room--thread-events room "$other-root"))
    ;; Lookup by event ID works.
    (should (equal "$reply2"
                   (leman-event-id (leman-room--thread-event-for-id "$reply2" room))))
    ;; Edits replace the stored event.
    (let ((edit (make-leman-event :id "$edit"
                                  :content `((m.new_content . ((body . "edited")))
                                             (m.relates_to . ((rel_type . "m.replace")
                                                              (event_id . "$reply1")))))))
      (should (leman-room--replace-thread-event edit room))
      ;; The replaced event keeps its original ID; its body is the
      ;; edit's "m.new_content".
      (should (equal "edited"
                     (map-elt (leman-event-content (leman-room--thread-event-for-id "$reply1" room))
                              'body)))
      ;; An edit of a non-thread event does nothing.
      (should-not (leman-room--replace-thread-event
                   (make-leman-event :id "$edit2"
                                     :content '((m.relates_to . ((rel_type . "m.replace")
                                                                 (event_id . "$not-in-thread")))))
                   room)))))

(ert-deftest leman-room--replace-event-carries-reactions ()
  ;; Reactions are attached to the original event's local data: the
  ;; edit event that replaces it in the buffer must carry them over,
  ;; else reactions to a message are lost the moment it is edited
  ;; (and new ones never render).
  (let* ((room (make-leman-room :id "!room:example.com"))
         (printer (lambda (data)
                    (if (leman-event-p data)
                        (format "%s " (leman-event-id data))
                      " ")))
         (message (make-leman-event :id "$msg" :type "m.room.message"
                                    :content '((body . "hi"))))
         (reaction (make-leman-event :id "$react" :type "m.reaction"
                                     :content '((m.relates_to (event_id . "$msg")))))
         (edit (make-leman-event :id "$edit" :type "m.room.message"
                                 :content '((body . "hi*")
                                            (m.new_content (body . "hi!"))
                                            (m.relates_to (rel_type . "m.replace")
                                                          (event_id . "$msg"))))))
    (setf (map-elt (leman-event-local message) 'reactions) (list reaction))
    (with-temp-buffer
      (setf (map-elt (leman-room-local room) 'buffer) (current-buffer)
            leman-ewoc (ewoc-create printer))
      (ewoc-enter-last leman-ewoc message)
      ;; Replacing with the edit event carries the reactions over.
      (should (leman-room--replace-event edit))
      (let ((node (ewoc-nth leman-ewoc 0)))
        (should (eq (ewoc-data node) edit))
        (should (equal (map-elt (leman-event-local edit) 'reactions)
                       (list reaction)))))))

(ert-deftest leman-room--invalidate-event-node-by-id ()
  "Event nodes are found by event ID, not struct identity.
The event struct whose image was downloaded (captured when the
event was formatted) may be a different object from the one the
node holds, e.g. when a thread event was edited (the thread data
stores a copy) or when the room buffer was re-created."
  (let* ((room (make-leman-room :id "!room:example.com"))
         (calls nil)
         (printer (lambda (data)
                    (push data calls)
                    (format "%s " (leman-event-id data))))
         (event (make-leman-event :id "$img"))
         (other (make-leman-event :id "$img"))
         (missing (make-leman-event :id "$missing")))
    (with-temp-buffer
      (setf (map-elt (leman-room-local room) 'buffer) (current-buffer)
            leman-ewoc (ewoc-create printer))
      (ewoc-enter-last leman-ewoc event)
      (should (= (length calls) 1))
      ;; Invalidating another struct with the same event ID
      ;; invalidates the node.
      (leman-room--invalidate-event-node other room)
      (should (= (length calls) 2))
      ;; An event with no node in the buffer is ignored without error.
      (leman-room--invalidate-event-node missing room)
      (should (= (length calls) 2)))))

(ert-deftest leman-room--fetch-html-image-dedups-in-flight ()
  ;; Re-renders while an image fetch is in flight (e.g. caused by
  ;; receipts or typing notifications) must not queue a duplicate
  ;; fetch for the same URL: the event is registered as waiting, and
  ;; all waiting events are re-rendered when the image arrives.
  (let* ((leman-room--fetching-html-images nil)
         (rerenders 0)
         (fetches 0)
         ;; Queued requests as (THEN . ELSE), most recent first.
         (closers nil)
         (event1 (make-leman-event :id "$e1"))
         (event2 (make-leman-event :id "$e2"))
         (room (make-leman-room :id "!room:x.org")))
    (cl-letf (((symbol-function #'url-is-cached) #'ignore)
              ((symbol-function #'plz-queue)
               (lambda (_queue &rest args)
                 (cl-incf fetches)
                 (push (cons (plist-get args :then) (plist-get args :else)) closers)
                 nil))
              ((symbol-function #'plz-run) #'ignore)
              ((symbol-function #'leman-room--store-image-in-url-cache) #'ignore)
              ((symbol-function #'leman-room--invalidate-event-node)
               (lambda (_event _room) (cl-incf rerenders))))
      ;; Two events with the same image URL: one request, both waiting.
      (leman-room--fetch-html-image "https://x.org/img.png" event1 room)
      (leman-room--fetch-html-image "https://x.org/img.png" event2 room)
      ;; A different URL still fetches.
      (leman-room--fetch-html-image "https://x.org/other.png" event2 room)
      (should (= fetches 2))
      (let ((waiting (cdr (assoc "https://x.org/img.png"
                                 leman-room--fetching-html-images))))
        (should (member event1 (mapcar #'car waiting)))
        (should (member event2 (mapcar #'car waiting))))
      ;; When the img.png fetch completes, every waiting event is
      ;; re-rendered and the URL is no longer tracked.
      (funcall (car (cadr closers)) "binary-data")
      (should (= rerenders 2))
      (should-not (assoc "https://x.org/img.png"
                         leman-room--fetching-html-images))
      ;; After completion, the same URL may be fetched again (e.g. the
      ;; cache entry expired).
      (leman-room--fetch-html-image "https://x.org/img.png" event1 room)
      (should (= fetches 3))
      ;; A failed fetch (other.png) stops being tracked without
      ;; re-rendering anything.
      (funcall (cdr (cadr closers)) nil)
      (should (= rerenders 2))
      (should-not (assoc "https://x.org/other.png"
                         leman-room--fetching-html-images)))))

(ert-deftest leman-room--m.image-callback-shows-thread-reply-image ()
  "A downloaded image for a thread reply is shown in the thread view.
Thread replies are not inserted into the room buffer's main
timeline, so the callback re-renders the open thread view
instead; it formerly warned that the event was \"not found in
room\" (an \"as-yet unexplained bug\") and the image was never
displayed."
  (let* ((room (make-leman-room :id "!room:example.com"))
         (session (make-leman-session :user (make-leman-user :id "@me:example.com")
                                      :events (make-hash-table :test #'equal)))
         (root (make-leman-event :id "$root" :origin-server-ts 1000
                                 :type "m.room.message"
                                 :sender (make-leman-user :id "@me:example.com")
                                 :content '((msgtype . "m.text") (body . "root"))))
         (reply (make-leman-event :id "$reply" :origin-server-ts 2000
                                  :sender (make-leman-user :id "@other:example.com")
                                  :content '((msgtype . "m.image")
                                             (body . "photo.jpg")
                                             (m.relates_to . ((rel_type . "m.thread")
                                                              (event_id . "$root"))))))
         (thread-buffer (get-buffer-create "*Leman Thread: room*")))
    (puthash "$root" root (leman-session-events session))
    (leman-room--add-thread-event reply room)
    (with-current-buffer thread-buffer
      (leman-thread-mode)
      (setf leman-thread-root-id "$root"
            leman-room room
            leman-session session)
      (let ((leman-room-images nil))
        (leman-room--render-thread thread-buffer root))
      (should (string-search "root" (buffer-string))))
    ;; The image arrives (via a struct captured when the reply was
    ;; first formatted); the callback must re-render the thread view,
    ;; which then uses the image data.  (The fake image data makes the
    ;; formatter insert its error string, proving the re-render used
    ;; the image data.)
    (let ((leman-room-images t))
      (leman-room--m.image-callback reply room "not-really-an-image"))
    (with-current-buffer thread-buffer
      (should (string-search "[error inserting image" (buffer-string))))
    (kill-buffer thread-buffer)))

(ert-deftest leman-room--format-m.image-no-duplicate-downloads ()
  "Re-rendering an event does not start a second image download.
The event may be re-rendered while its image is being downloaded
(e.g. when other events arrive or the room buffer is
re-created); each re-render must not start another download."
  (let* ((room (make-leman-room :id "!room:example.com"))
         (event (make-leman-event :id "$img"
                                  :content '((msgtype . "m.image")
                                             (body . "photo.jpg")
                                             (url . "mxc://example.com/photo"))))
         (downloads 0))
    (cl-letf (((symbol-function 'leman-room--image-download)
               (lambda (&rest _args) (cl-incf downloads))))
      (let ((leman-room-images t)
            ;; The formatter reads the room from the dynamic
            ;; `leman-room' variable.
            (leman-room room))
        (leman-room--format-m.image event nil)
        (leman-room--format-m.image event nil)
        (should (= downloads 1))
        ;; Once the image is available, re-rendering shows it
        ;; without downloading.
        (setf (map-elt (leman-event-local event) 'image) "image-data")
        (should (string-match-p "error inserting image"
                                (leman-room--format-m.image event nil)))
        (should (= downloads 1))
        ;; After the download completes or fails, re-rendering may
        ;; download again.
        (setf (map-elt (leman-event-local event) 'image) nil
              (map-elt (leman-event-local event) 'image-downloading) nil)
        (leman-room--format-m.image event nil)
        (should (= downloads 2))))))

(ert-deftest leman-room--format-thread-chip ()
  "Test that thread roots show a summary chip linking to the view."
  (let* ((room (make-leman-room :id "!room:example.com"))
         (root (make-leman-event :id "$root"))
         (chip (progn
                 (leman-room--add-thread-event (leman-tests--thread-reply-event "$reply1" "$root") room)
                 (leman-room--add-thread-event (leman-tests--thread-reply-event "$reply2" "$root") room)
                 (leman-room--format-thread-chip root room))))
    (should (string-match-p "2 replies" chip))
    ;; No replies: no chip.
    (should (string-empty-p (leman-room--format-thread-chip
                             (make-leman-event :id "$non-root") room))))
  ;; Server-side summary is used when local events are unknown.
  (let* ((room (make-leman-room :id "!room:example.com"))
         (root (make-leman-event :id "$root2"
                                 :unsigned '((m.relations . ((m.thread . ((count . 5))))))))
         (chip (leman-room--format-thread-chip root room)))
    (should (string-match-p "5 replies" chip)))
  ;; A summary's "latest_event" is a raw event alist, not an event
  ;; struct; the chip must show its body without signaling an error.
  (let* ((room (make-leman-room :id "!room:example.com"))
         (latest-event '((type . "m.room.message")
                         (content . ((body . "Latest reply")))))
         (root (make-leman-event :id "$root3"
                                 :unsigned `((m.relations . ((m.thread . ((count . 1)
                                                                          (latest_event . ,latest-event))))))))
         (chip (leman-room--format-thread-chip root room)))
    (should (string-match-p "1 reply" chip))
    ;; The snippet is rendered as a display property; this proves the
    ;; raw "latest_event" was converted to an event struct (and its
    ;; body extracted) without signaling an error.
    (should (text-property-not-all 0 (length chip) 'display nil chip))))

(ert-deftest leman-room--format-thread-chip-svg ()
  "Test that SVG thread chips remain strings which can be buttonized."
  (let* ((room (make-leman-room :id "!room:example.com"))
         (root (make-leman-event :id "$root"))
         (image '(image :type svg :data "mock-svg")))
    (leman-room--add-thread-event
     (leman-tests--thread-reply-event "$reply" "$root") room)
    (cl-letf (((symbol-function 'leman-room--svg-rendering-p) (lambda (_) t))
              ((symbol-function 'svg-lib-tag) (lambda (&rest _) image)))
      (let ((chip (leman-room--format-thread-chip root room)))
        (should (stringp chip))
        (should (equal (get-text-property 0 'display chip) image))))))

(ert-deftest leman-room--typing-footer-svg ()
  "Test that SVG typing tags are inserted through a display property."
  (let ((image '(image :type svg :data "mock-svg")))
    (cl-letf (((symbol-function 'leman-room--svg-rendering-p) (lambda (_) t))
              ((symbol-function 'svg-lib-tag) (lambda (&rest _) image)))
      (let ((footer (leman-room--typing-footer '("Ada") nil)))
        (should (stringp footer))
        (should (equal (get-text-property 0 'display footer) image))))))

(ert-deftest leman-room--format-event-undecryptable ()
  "Undecryptable encrypted events show a placeholder, not raw content."
  (let* ((room (make-leman-room :id "!room:example.com"))
         (event (make-leman-event :id "$enc"
                                  :sender (make-leman-user :id "@other:example.com")
                                  :type "m.room.encrypted"
                                  :content '((algorithm . "m.megolm.v1.aes-sha2")
                                             (ciphertext . "secret")))))
    (let ((formatted (leman-room--format-event event room nil)))
      (should (string-match-p "unable to decrypt" formatted))
      (should-not (string-match-p "secret" formatted)))))

(ert-deftest leman-room--format-shield ()
  "Authenticity markers distinguish verified, unverified, and plaintext.
On non-graphical displays (as in these batch tests), emojis are
used, and plaintext messages get no marker."
  (let ((room (make-leman-room :id "!room:example.com")))
    (pcase-dolist (`(,shield ,expected ,face)
                   '(((none) "🛡" nil)
                     ((red "UnverifiedIdentity" "reason") "⚠️" error)
                     ((grey "UnverifiedIdentity" "reason") "⚠️" shadow)
                     (nil "" nil)))
      (let* ((event (make-leman-event :id "$msg"
                                      :type "m.room.message"
                                      :local (when shield
                                               (list (cons 'shield shield)))
                                      :content '((msgtype . "m.text")
                                                 (body . "hello"))))
             (marker (leman-room--format-shield event room)))
        (should (string-match-p (regexp-quote expected) marker))
        (when face
          (should (memq face (ensure-list (get-text-property 0 'face marker))))))))
  ;; Encrypted messages carry a marker whatever their decrypted type.
  (should (leman-room--shield-p
           (make-leman-event :id "$enc" :type "m.room.encrypted"
                             :local '((shield . (red "Code" "why"))))))
  ;; Plaintext messages of a message-like type get one; others don't.
  (should (leman-room--shield-p
           (make-leman-event :id "$msg" :type "m.room.message")))
  (should-not (leman-room--shield-p
               (make-leman-event :id "$img" :type "m.image"
                                 :content '((body . "photo"))))))

(ert-deftest leman-room--format-event-shield-leads ()
  "The authenticity marker is rendered at the start of the line."
  (let* ((room (make-leman-room :id "!room:example.com"))
         (session (make-leman-session :user (make-leman-user :id "@other:x.org")))
         (event (make-leman-event :id "$msg"
                                  :sender (make-leman-user :id "@vv:x.org"
                                                           :displayname "Vincent")
                                  :type "m.room.message"
                                  :origin-server-ts 1694000000000
                                  :local '((shield . (none)))
                                  :content '((msgtype . "m.text")
                                             (body . "hello world"))))
         ;; A margin-less format: the shield stays as plain text, so
         ;; its position can be asserted.
         (formatted (let ((leman-room-message-format-spec "[%t] %S> %B%r%T"))
                      (leman-room--format-event event room session))))
    (should (< (string-match-p "🛡" formatted)
               (string-match-p "hello world" formatted)))))

(ert-deftest leman-room--shield-image-cache-uses-visual-parameters ()
  "Shield images are not reused across font sizes or theme colors."
  (let ((leman-room--shield-images (make-hash-table :test #'equal))
        (height 10)
        (color "green"))
    (cl-letf (((symbol-function 'window-font-height) (lambda (&rest _) height))
              ((symbol-function 'leman-room--shield-color) (lambda (&rest _) color))
              ((symbol-function 'svg-image) (lambda (&rest args) args)))
      (let ((small (leman-room--shield-image "verified")))
        (setq height 20)
        (let ((large (leman-room--shield-image "verified")))
          (should-not (eq small large))
          (should (= (plist-get (cdr small) :max-height) 9))
          (should (= (plist-get (cdr large) :max-height) 18))
          (should (eq large (leman-room--shield-image "verified")))
          (setq color "blue")
          (should-not (eq large (leman-room--shield-image "verified"))))))))

(ert-deftest leman-room--shield-color-is-hex ()
  "Shield colors are normalized to `#rrggbb'.
librsvg does not know the X11 color names theme faces return
\(e.g. \"Green1\"): such a name silently renders no stroke on the
outlined shield."
  (cl-letf (((symbol-function 'face-foreground) (lambda (&rest _) "Green1")))
    (should (equal (leman-room--shield-color 'success "green") "#00ff00")))
  (cl-letf (((symbol-function 'face-foreground) (lambda (&rest _) "grey70")))
    (should (string-match-p "\\`#[0-9a-f]\\{6\\}\\'"
                            (leman-room--shield-color 'shadow "gray"))))
  ;; Hex colors stay within the `#rrggbb' shape (exact value depends
  ;; on the frame's color resolution; in these batch tests it goes
  ;; through the TTY color table).
  (cl-letf (((symbol-function 'face-foreground) (lambda (&rest _) "#a08979")))
    (should (string-match-p "\\`#[0-9a-f]\\{6\\}\\'"
                            (leman-room--shield-color 'success "green"))))
  ;; An unresolvable color (no frame to query) is left as is.
  (cl-letf (((symbol-function 'face-foreground) (lambda (&rest _) "not-a-color"))
            ((symbol-function 'color-name-to-rgb) (lambda (&rest _) nil)))
    (should (equal (leman-room--shield-color 'shadow "gray") "not-a-color"))))

(ert-deftest leman-room--sender-margin-width ()
  "The left margin fits senders' complete display names, capped."
  (let ((leman-room-sender-in-left-margin t)
        (leman-room-left-margin-width 12)
        (leman-room-left-margin-max-width 24)
        (room (make-leman-room :id "!room:example.com")))
    ;; No members: fall back to the configured width.
    (should (= (leman-room--sender-margin-width room) 12))
    ;; A short name: the configured width is the floor.
    (puthash "@short:x.org" (make-leman-user :id "@short:x.org"
                                             :displayname "Al")
             (leman-room-members room))
    (should (= (leman-room--sender-margin-width room) 12))
    ;; A long name: widen to fit it (shield marker plus one space),
    ;; capped by `leman-room-left-margin-max-width'.
    (puthash "@long:x.org" (make-leman-user :id "@long:x.org"
                                            :displayname
                                            "Christopher Alexander III")
             (leman-room-members room))
    (should (= (leman-room--sender-margin-width room) 24))))

(ert-deftest leman-room--typing-footer ()
  "The typing footer lists the typing users, or is empty."
  (let ((room (make-leman-room :id "!room:example.com")))
    (should (string-empty-p (leman-room--typing-footer nil room)))
    (should (string-match-p "Alice" (leman-room--typing-footer '("Alice") room)))
  (should (string-match-p "Alice, Bob" (leman-room--typing-footer
                                        '("Alice" "Bob") room)))))

(ert-deftest leman-room--svg-rendering-p ()
  "SVG rendering follows the room buffer's display, not the selected frame."
  (let* ((room (make-leman-room :id "!room:example.com"))
         (buffer (generate-new-buffer " *leman-svg-test*")))
    (unwind-protect
        (progn
          (setf (map-elt (leman-room-local room) 'buffer) buffer)
          (with-current-buffer buffer
            (setq-local leman-room--svg-enabled-p t))
          (should (leman-room--svg-rendering-p room))
          (with-current-buffer buffer
            (setq-local leman-room--svg-enabled-p nil))
          (should-not (leman-room--svg-rendering-p room)))
      (kill-buffer buffer))))

(ert-deftest leman--sessions-round-trip-device-id ()
  "The device ID round-trips through the saved sessions file.
Restoring it lets E2EE skip its whoami call."
  (let* ((leman-sessions-file (make-temp-file "leman-sessions-test-"))
         (session (make-leman-session
                   :user (make-leman-user :id "@vv:x.org" :username "vv")
                   :server (make-leman-server :name "x.org" :uri-prefix "https://x.org")
                   :token "tok"
                   :transaction-id 42
                   :device-id "ABC"))
         (leman-sessions (list (cons "@vv:x.org" session))))
    (leman--write-sessions leman-sessions)
    (let ((restored (cdr (car (leman--read-sessions)))))
      (should (equal (leman-session-device-id restored) "ABC"))
      (should (equal (leman-session-token restored) "tok"))
      (should (equal (leman-user-id (leman-session-user restored)) "@vv:x.org")))))

(ert-deftest leman--write-sessions-uses-session-directory-for-temp-file ()
  "The temporary file must share the destination filesystem for rename."
  (let* ((directory (make-temp-file "leman-sessions-dir-" 'dir))
         (leman-sessions-file (expand-file-name "sessions" directory))
         (session (make-leman-session
                   :user (make-leman-user :id "@vv:x.org" :username "vv")
                   :server (make-leman-server :name "x.org" :uri-prefix "https://x.org")
                   :token "tok"))
         (arguments nil)
         (real-make-temp-file (symbol-function #'make-temp-file)))
    (unwind-protect
        (cl-letf (((symbol-function #'make-temp-file)
                   (lambda (&rest args)
                     (setf arguments args)
                     (apply real-make-temp-file args))))
          (leman--write-sessions (list (cons "@vv:x.org" session))))
      (delete-directory directory 'recursive))
    (should (equal (file-name-directory (car arguments))
                   (file-name-as-directory directory)))
    (should (equal (nth 1 arguments) nil))
    (should (equal (nth 2 arguments) ".tmp"))
    (should-not (nth 3 arguments))))

(ert-deftest leman-room-send-file-omits-unknown-mimetype ()
  ;; A nil mimetype (unknown file extension) must be omitted from the
  ;; message content: keeping the key would encode a JSON array of the
  ;; key (AGENTS.md, the nil-overload class).
  (let* ((file (make-temp-file "leman-send-file-test." nil ".unknownext"))
         (requests nil))
    (cl-letf (((symbol-function #'yes-or-no-p) #'always)
              ((symbol-function #'leman-message) #'ignore)
              ((symbol-function #'leman-upload)
               (lambda (_session &rest args)
                 (funcall (plist-get args :then) '((content_uri . "mxc://x.org/abc")))))
              ((symbol-function #'leman-api)
               (lambda (_session _endpoint &rest args)
                 (push (plist-get args :data) requests))))
      (leman-room-send-file file "my file" (make-leman-room :id "!room:x.org")
                            (make-leman-session :transaction-id 1)))
    (let ((data (car requests)))
      (should data)
      (should (string-match-p "mxc://x.org/abc" data))
      (should-not (string-match-p "mimetype" data))
      (should (string-match-p "\"size\"" data)))))

(ert-deftest leman-room-send-file-encrypts-in-encrypted-rooms ()
  ;; In an encrypted room, the file is encrypted before upload (the
  ;; homeserver must never see the plaintext), and the event carries
  ;; the ciphertext's URL with the key/IV/hash ("file" replacing
  ;; "url", spec: "Sending encrypted attachments"), Megolm-encrypted
  ;; like any other message content.
  (let* ((original (make-temp-file "leman-plain-"))
         (uploads nil)
         (requests nil)
         (captured-content nil)
         (fake (leman-e2ee-tests--fake-agent
                (list (cons 'encrypt_file
                            (list (cons 'key "k-url-safe")
                                  (cons 'iv "iv-b64")
                                  (cons 'sha256 "hash-b64"))))))
         (session (make-leman-session :transaction-id (leman--initial-transaction-id)))
         (room (make-leman-room :id "!room:x.org"
                                :members (make-hash-table :test #'equal))))
    (setf (leman-session-e2ee session) (car fake))
    (setf (leman-room-state room)
          (list (make-leman-event :id "$enc-state" :type "m.room.encryption"
                                  :content '((algorithm . "m.megolm.v1.aes-sha2")))))
    (cl-letf (((symbol-function #'yes-or-no-p) #'always)
              ((symbol-function #'leman-message) #'ignore)
              ((symbol-function #'leman-upload)
               (lambda (_session &rest args)
                 (push (list (nth 1 (plist-get args :data))
                             (plist-get args :content-type)
                             (plist-get args :filename))
                       uploads)
                 (funcall (plist-get args :then) '((content_uri . "mxc://x.org/cipher")))))
              ((symbol-function #'leman-api)
               (lambda (_session endpoint &rest args)
                 (push (cons endpoint (plist-get args :data)) requests))))
      (let ((leman-encrypt-send-content-function
             (lambda (_session _room content)
               (push content captured-content)
               (cons (list (cons 'ciphertext "opaque")) "m.room.encrypted"))))
        (leman-room-send-file original "my file" room session)))
    ;; The ciphertext was uploaded, not the plaintext, as
    ;; application/octet-stream (a mimetype would leak the type).
    (let ((upload (car uploads)))
      (should-not (equal (car upload) original))
      (should (string-prefix-p (file-name-as-directory temporary-file-directory)
                               (car upload)))
      (should (equal (nth 1 upload) "application/octet-stream"))
      ;; The original filename must stay inside the encrypted event,
      ;; not in the media-upload query parameter.
      (should-not (nth 2 upload))
      ;; The ciphertext temp file is removed after the upload callback.
      (should-not (file-exists-p (car upload))))
    ;; The message is sent Megolm-encrypted...
    (let ((request (car requests)))
      (should (string-match-p "/send/m.room.encrypted/" (car request)))
      (should (string-match-p "opaque" (cdr request)))
      ;; ...so the file key is not visible to the homeserver.
      (should-not (string-match-p "A256CTR" (cdr request))))
    ;; The content handed to encryption has the "file" object.
    (let ((content (car captured-content)))
      (should-not (assoc "url" content))
      (let ((file (cdr (assoc "file" content))))
        (should (equal (alist-get "url" file nil nil #'string=)
                       "mxc://x.org/cipher"))
        (should (equal (alist-get "v" file nil nil #'string=) "v2"))
        (should (equal (alist-get "iv" file nil nil #'string=) "iv-b64"))
        (should (equal (alist-get "sha256"
                                  (cdr (assoc "hashes" file)) nil nil #'string=)
                       "hash-b64"))
        (let ((key (cdr (assoc "key" file))))
          (should (equal (alist-get "k" key nil nil #'string=) "k-url-safe"))
          (should (equal (alist-get "alg" key nil nil #'string=) "A256CTR"))
          (should (eq (alist-get "ext" key nil nil #'string=) t))
          (should (equal (alist-get "key_ops" key nil nil #'string=)
                         '("encrypt" "decrypt"))))))))

(ert-deftest leman-room-send-file-fails-closed-without-agent ()
  ;; An encrypted room never receives a plaintext upload: with no
  ;; agent, sending a file must signal before anything is uploaded.
  (let* ((original (make-temp-file "leman-plain-"))
         (upload-called nil)
         (room (make-leman-room :id "!room:x.org"
                                :members (make-hash-table :test #'equal))))
    (setf (leman-room-state room)
          (list (make-leman-event :id "$enc" :type "m.room.encryption"
                                  :content '((algorithm . "m.megolm.v1.aes-sha2")))))
    (cl-letf (((symbol-function #'yes-or-no-p) #'always)
              ((symbol-function #'leman-upload)
               (lambda (&rest _) (setf upload-called t))))
      (let ((leman-encrypt-send-content-function #'leman-e2ee--encrypt-content))
        (should-error (leman-room-send-file original "my file" room
                                            (make-leman-session :transaction-id 1))
                      :type 'leman-e2ee-error)))
    (should-not upload-called)))

(ert-deftest leman-room-download-file-decrypts-encrypted-attachments ()
  ;; An encrypted attachment's blob is ciphertext: it is downloaded,
  ;; hash-verified, and decrypted with the agent before being written
  ;; to the destination.
  (let* ((leman-session (make-leman-session
                         :server (make-leman-server :name "x.org" :uri-prefix "https://x.org")))
         (room (make-leman-room :id "!room:x.org"))
         (dir (make-temp-file "leman-download-test-" t))
         (destination (expand-file-name "file.bin" dir))
         (downloaded nil)
         (decrypt-calls nil)
         (fake (leman-e2ee-tests--fake-agent
                (list (cons 'decrypt_file nil)))))
    (setf (leman-session-e2ee leman-session) (car fake))
    (let ((event (make-leman-event :id "$f" :type "m.room.message"
                                   :content (list 'msgtype "m.file"
                                                  'body "file.bin"
                                                  'file (list '(url . "mxc://x.org/enc")
                                                              '(v . "v2")
                                                              '(key . ((k . "k") (alg . "A256CTR")
                                                                       (kty . "oct") (ext . t)
                                                                       (key_ops . ("encrypt" "decrypt"))))
                                                              '(iv . "iv")
                                                              '(hashes . ((sha256 . "hash"))))))))
      (cl-letf (((symbol-function #'leman--media-request)
                 (lambda (_url _session &rest args)
                   (setf downloaded (nth 1 (plist-get args :as)))
                   ;; The "download" writes the ciphertext blob.
                   (write-region "" nil (nth 1 (plist-get args :as)))
                   (funcall (plist-get args :then))))
                ((symbol-function #'leman-e2ee-decrypt-file)
                 (lambda (_agent input output key iv sha256)
                   (push (list input output key iv sha256) decrypt-calls)
                   (setf downloaded nil)
                   ;; The "decryption" writes the plaintext.
                   (write-region "" nil output))))
        (leman-room-download-file event destination)))
    ;; The ciphertext went to a temp file, which was decrypted to the
    ;; destination and removed.
    (let ((call (car decrypt-calls)))
      (should (string-prefix-p (file-name-as-directory temporary-file-directory)
                               (nth 0 call)))
      (should (equal (nth 1 call) destination))
      (should (equal (nth 2 call) "k"))
      (should (equal (nth 3 call) "iv"))
      (should (equal (nth 4 call) "hash"))
      (should-not (file-exists-p (nth 0 call))))
    (should-not downloaded)))

(ert-deftest leman-room-download-file-sanitizes-remote-filename ()
  ;; Remote senders control the attachment filename: a crafted name
  ;; must not escape the download directory.
  (let* ((leman-session (make-leman-session
                         :server (make-leman-server :name "x.org" :uri-prefix "https://x.org")))
         (room (make-leman-room :id "!room:x.org"))
         (dir (make-temp-file "leman-download-test-" t))
         (downloaded nil))
    (cl-letf (((symbol-function #'leman--media-request)
               (lambda (_url _session &rest args)
                 (setf downloaded (nth 1 (plist-get args :as))))))
      (dolist (name '("../../.bashrc" "sub/../../evil.txt" ".." "."))
        (let ((event (make-leman-event :id "$f" :type "m.room.message"
                                       :content (list 'msgtype "m.file"
                                                      'url "mxc://x.org/abc"
                                                      'filename name
                                                      'body "whatever"))))
          (setf downloaded nil)
          (leman-room-download-file event dir)
          (should downloaded)
          (should (equal (file-name-directory
                          (directory-file-name downloaded))
                         (file-name-as-directory
                          (directory-file-name dir)))))))))

(ert-deftest leman-ignore-user-sends-empty-object-values ()
  ;; The spec requires each ignored user's value to be an empty
  ;; object; elisp's nil would encode as null (AGENTS.md), so the
  ;; body is built by hand and sent verbatim.
  ;; NOTE: Session account-data holds raw decoded events (sync
  ;; responses), not event structs; empty map values decode to nil
  ;; cdrs (e.g. IGNORED = ((@bad:x.org))).
  (let* ((ignored-alist (list (cons '@bad:x.org nil)))
         (content-alist (list (cons 'ignored_users ignored-alist)))
         (session (make-leman-session
                   :user (make-leman-user :id "@me:x.org")
                   :server (make-leman-server :name "x.org" :uri-prefix "https://x.org")
                   :account-data
                   (list (list (cons 'type "m.ignored_user_list")
                               (cons 'content content-alist)))))
         (requests nil))
    (cl-letf (((symbol-function #'leman-api)
               (lambda (_session _endpoint &rest args)
                 (push (plist-get args :data) requests))))
      (leman-ignore-user "@other:x.org" session)
      (should (equal (car requests)
                     "{\"ignored_users\":{\"@other:x.org\":{},\"@bad:x.org\":{}}}"))
      ;; Unignoring removes the entry without corrupting the rest.
      (leman-ignore-user "@other:x.org" session 'unignore)
      (should (equal (car requests)
                     "{\"ignored_users\":{\"@bad:x.org\":{}}}")))))

(provide 'leman-tests)

;;; leman-tests.el ends here
