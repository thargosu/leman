;;; leman-threads.el --- Unread threads for Leman  -*- lexical-binding: t; -*-

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

;; This library tracks which threads (MSC3440: replies relating to a
;; root with an "m.thread" relationship) have unread replies, apart
;; from a room's main timeline.  A room whose only new messages are
;; thread replies is then not shown as unread itself; its threads are
;; listed separately (see `leman-inbox').
;;
;; Servers that implement threaded notifications (MSC3773) report
;; per-thread counts, but some, like Conduit, do not: they count
;; thread replies in the room's counts, and reset them on any read
;; receipt.  So unread threads come from, in order of preference:
;;
;; * Room account data published by a bridge (see
;;   `leman-threads-bridge-account-data-type'), which knows the
;;   authoritative read state of the bridged network's threads (e.g.
;;   Mattermost's collapsed reply threads).  It looks like:
;;
;;     {"main": {"unread": 6, "mentions": 0},
;;      "threads": {"$root": {"unread": 42, "mentions": 1,
;;                            "latest_ts": 1761818000000,
;;                            "latest_event_id": "$reply"}}}
;;
;;   where "main" counts the room's messages outside threads, and
;;   "threads" lists only the threads with unread replies.
;;
;; * Otherwise, the thread replies Leman has seen, compared to the
;;   threads' read positions, which Leman stores in room account data
;;   (see `leman-threads-read-account-data-type') so they persist and
;;   are shared between sessions.  Thread replies older than the
;;   room's read position when Leman first tracked it are considered
;;   read.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'subr-x)

(require 'leman-lib)

;;;; Customization

(defgroup leman-threads nil
  "Options for unread threads."
  :group 'leman)

(defcustom leman-threads-bridge-account-data-type "fi.mau.kchat.thread_unreads"
  "Type of room account data in which a bridge publishes unread threads.
When a room has such data, it is used instead of what Leman
computes from the thread replies it has seen.  See Commentary in
library `leman-threads' for its format."
  :type 'string)

(defcustom leman-threads-read-account-data-type
  "io.github.thargosu.leman.thread_read"
  "Type of room account data in which Leman stores threads' read positions."
  :type 'string)

(defcustom leman-threads-send-receipts 'bridged
  "Whether to send read receipts for threads read in Leman.
Receipts carry the thread's ID (MSC3771), so a bridge can mark
the thread read on the bridged network.  Note that some servers
(like Conduit) ignore the thread ID, and reset the room's unread
counts on any receipt.  When `bridged', receipts are sent only in
rooms with bridge-published unread threads (see
`leman-threads-bridge-account-data-type'), whose counts don't
depend on the server's."
  :type '(choice (const :tag "Only in bridged rooms" bridged)
                 (const :tag "Always" t)
                 (const :tag "Never" nil)))

(defcustom leman-threads-read-save-delay 2
  "Seconds to wait before saving threads' read positions.
Saves are coalesced per room."
  :type 'number)

(defcustom leman-threads-max-saved 300
  "Maximum number of threads whose read positions are saved per room.
The most recently read are kept."
  :type 'natnum)

;;;; Variables

(defvar leman-room-unread-only-counts-notifications)

(defvar leman-threads-read-functions nil
  "Functions called when a thread is marked read.
Each is called with three arguments: the room, the session, and
the thread root's event ID.")

(defvar leman-threads-event-functions nil
  "Functions called when an event fetched for threads arrives.
Each is called with three arguments: the event, the room, and the
session.  Used, e.g. to show a thread root's text once known.")

(defvar leman-threads--tick 0
  "Counter incremented whenever local thread data changes.
Invalidates cached thread scans.")

(defvar leman-threads--fetching (make-hash-table :test #'equal)
  "Event IDs being fetched, or which failed to be fetched.")

;;;; Structs

(cl-defstruct leman-thread
  "A thread with unread replies in a room."
  room session root-id
  ;; Number of unread replies, and how many of those ping the user.
  (count 0) (mentions 0)
  ;; Timestamp of the latest reply, in milliseconds.
  latest-ts
  ;; The latest reply's event, if known, and its ID.
  latest-event latest-event-id)

;;;; Events

(defun leman-threads--event (session id)
  "Return the event ID known to SESSION, or nil."
  (when-let ((events (leman-session-events session)))
    (gethash id events)))

(defun leman-threads--reply-p (event)
  "Return non-nil if EVENT is a message counting as a thread reply.
That is, a message, sticker, or encrypted event relating to a
thread root, which is not an edit."
  (and (member (leman-event-type event)
               '("m.room.message" "m.sticker" "m.room.encrypted"))
       (equal "m.thread" (map-nested-elt (leman-event-content event)
                                         '(m.relates_to rel_type)))))

(defun leman-threads--main-message-p (event session)
  "Return non-nil if EVENT is a message of its room's main timeline.
On SESSION, edits of thread replies are part of their thread."
  (and (member (leman-event-type event)
               '("m.room.message" "m.sticker" "m.room.encrypted"))
       (not (leman--event-thread-root-id event session))))

(defun leman-threads-latest-main-event (room session)
  "Return ROOM's latest event outside threads on SESSION, or nil.
Read markers set for the main timeline should be at such events:
a bridge may take a receipt at a thread reply to mean that only
the thread was read."
  (cl-loop for event in (leman-room-timeline room)
           unless (leman--event-thread-root-id event session)
           return event))

(defun leman-threads-latest-main-ts (room session)
  "Return the timestamp of ROOM's latest main-timeline message on SESSION.
Fall back to ROOM's latest timestamp."
  (or (cl-loop for event in (leman-room-timeline room)
               when (leman-threads--main-message-p event session)
               return (leman-event-origin-server-ts event))
      (leman-room-latest-ts room)))

;;;; Account data

(defun leman-threads--account-data-content (room type)
  "Return the content of ROOM's account data of TYPE, or nil."
  (map-elt (alist-get type (leman-room-account-data room) nil nil #'equal)
           'content))

(defun leman-threads--key-name (key)
  "Return KEY, a JSON object key, as a string.
`json-read' makes keys into symbols, while locally made alists use
strings."
  (if (symbolp key) (symbol-name key) key))

(defun leman-threads--map-get (map key)
  "Return the value of string KEY in MAP, a JSON object as an alist."
  (cl-loop for (k . value) in map
           when (equal key (leman-threads--key-name k))
           return value))

(defun leman-threads-bridged-p (room)
  "Return non-nil if a bridge publishes ROOM's unread threads."
  (and (alist-get leman-threads-bridge-account-data-type
                  (leman-room-account-data room) nil nil #'equal)
       t))

;;;; Read positions

(defun leman-threads--local-reads (room)
  "Return ROOM's table of threads read in this session.
Maps root IDs to the timestamp of the latest reply read."
  (or (map-elt (leman-room-local room) 'thread-reads)
      (setf (map-elt (leman-room-local room) 'thread-reads)
            (make-hash-table :test #'equal))))

(defun leman-threads--room-read-ts (room session)
  "Return the timestamp up to which ROOM on SESSION was read, or nil.
That is, the time of the fully-read marker's event, or of the
user's read receipt's."
  (let* ((fully-read-id (map-nested-elt
                         (alist-get "m.fully_read" (leman-room-account-data room)
                                    nil nil #'equal)
                         '(content event_id)))
         (receipt (gethash (leman-user-id (leman-session-user session))
                           (leman-room-receipts room))))
    (or (when-let ((event (and fully-read-id (leman-threads--event session fully-read-id))))
          (leman-event-origin-server-ts event))
        (when-let ((event (and (car receipt) (leman-threads--event session (car receipt)))))
          (leman-event-origin-server-ts event))
        (cdr receipt))))

(defun leman-threads--since (room session)
  "Return the timestamp before which ROOM's thread replies are read.
On SESSION.  It is the room's read position when Leman first
tracked its threads, saved in its account data; afterward, reading
the main timeline does not mark threads read.  Return nil if
unknown yet."
  (or (map-elt (leman-threads--account-data-content
                room leman-threads-read-account-data-type)
               'since)
      (map-elt (leman-room-local room) 'thread-since)
      (when-let ((ts (leman-threads--room-read-ts room session)))
        ;; NOTE: Saved with threads' read positions, so that it
        ;; outlives this session (see `leman-threads--scan').
        (setf (map-elt (leman-room-local room) 'thread-since) ts))))

(defun leman-threads-read-ts (room session root-id)
  "Return the timestamp up to which thread ROOT-ID in ROOM was read.
On SESSION.  Return 0 when nothing is known to be read."
  (max (or (leman-threads--since room session) 0)
       (or (gethash root-id (leman-threads--local-reads room)) 0)
       (or (leman-threads--map-get
            (map-elt (leman-threads--account-data-content
                      room leman-threads-read-account-data-type)
                     'threads)
            root-id)
           0)))

(defun leman-threads--save (room session)
  "Save ROOM's threads' read positions in its account data on SESSION."
  (setf (map-elt (leman-room-local room) 'thread-save-timer) nil)
  (let* ((saved (map-elt (leman-threads--account-data-content
                          room leman-threads-read-account-data-type)
                         'threads))
         (reads (make-hash-table :test #'equal)))
    (cl-loop for (key . ts) in saved
             do (puthash (leman-threads--key-name key) ts reads))
    (maphash (lambda (root-id ts)
               (when (> ts (gethash root-id reads 0))
                 (puthash root-id ts reads)))
             (leman-threads--local-reads room))
    (let* ((threads (seq-take (cl-sort (map-pairs reads) #'> :key #'cdr)
                              leman-threads-max-saved))
           (since (leman-threads--since room session))
           (content (append (when since (list (cons "since" since)))
                            (list (cons "threads" (or threads
                                                      ;; An empty object.
                                                      (make-hash-table)))))))
      (leman-put-account-data session leman-threads-read-account-data-type content
        :room room
        :then (lambda (_)
                ;; Keep the saved content, as the server may not
                ;; echo it in a sync response.
                (setf (alist-get leman-threads-read-account-data-type
                                 (leman-room-account-data room) nil nil #'equal)
                      (list (cons 'type leman-threads-read-account-data-type)
                            (cons 'content content)))
                (cl-incf leman-threads--tick))))))

(defun leman-threads--schedule-save (room session)
  "Save ROOM's threads' read positions on SESSION soon.
Saves are coalesced per `leman-threads-read-save-delay'."
  (unless (timerp (map-elt (leman-room-local room) 'thread-save-timer))
    (setf (map-elt (leman-room-local room) 'thread-save-timer)
          (run-with-timer leman-threads-read-save-delay nil
                          #'leman-threads--save room session))))

;;;; Scanning

(defun leman-threads--known-replies (room session)
  "Return a table of ROOM's known thread replies on SESSION.
Maps root IDs to lists of reply events, from the room's timeline
and its thread data (which includes replies fetched when viewing
a thread)."
  (let ((table (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal)))
    (cl-flet ((add (event)
                (when (and (leman-threads--reply-p event)
                           (not (gethash (leman-event-id event) seen)))
                  (puthash (leman-event-id event) t seen)
                  (push event (gethash (leman--event-thread-root-id event session)
                                       table)))))
      (mapc #'add (leman-room-timeline room))
      (when-let ((threads (map-elt (leman-room-local room) 'threads)))
        (maphash (lambda (_root-id events) (mapc #'add events)) threads)))
    table))

(defun leman-threads--ping-p (event room session)
  "Return non-nil if EVENT in ROOM pings SESSION's user."
  (declare (function leman-room--event-ping-p "leman-room"))
  (and (fboundp 'leman-room--event-ping-p)
       (leman-room--event-ping-p event room session)))

(defun leman-threads--scan (room session)
  "Return ROOM's unread threads on SESSION, computed from known replies.
Return a list of `leman-thread' structs."
  (let ((our-id (leman-user-id (leman-session-user session)))
        (known (leman-threads--known-replies room session))
        (threads nil))
    (when (and (> (hash-table-count known) 0)
               (not (map-elt (leman-threads--account-data-content
                              room leman-threads-read-account-data-type)
                             'since))
               (leman-threads--since room session))
      ;; Save the room's read position from which its threads are
      ;; tracked, once it has some.
      (leman-threads--schedule-save room session))
    (maphash
     (lambda (root-id replies)
       (let* ((replies (cl-sort replies #'> :key #'leman-event-origin-server-ts))
              (latest (car replies))
              ;; Replying in a thread implies having read it.
              (our-latest-ts (cl-loop for event in replies
                                      when (equal our-id (leman-user-id
                                                          (leman-event-sender event)))
                                      return (leman-event-origin-server-ts event)))
              (read-ts (max (leman-threads-read-ts room session root-id)
                            (or our-latest-ts 0)))
              (unread (cl-loop for event in replies
                               while (> (leman-event-origin-server-ts event) read-ts)
                               unless (equal our-id (leman-user-id
                                                     (leman-event-sender event)))
                               collect event)))
         (when unread
           (push (make-leman-thread
                  :room room :session session :root-id root-id
                  :count (length unread)
                  :mentions (cl-count-if (lambda (event)
                                           (leman-threads--ping-p event room session))
                                         unread)
                  :latest-ts (leman-event-origin-server-ts latest)
                  :latest-event latest
                  :latest-event-id (leman-event-id latest))
                 threads))))
     known)
    threads))

(defun leman-threads--from-bridge (room session content)
  "Return ROOM's unread threads on SESSION from bridge CONTENT.
CONTENT is the bridge's account data (see Commentary).  Threads
read in this session since it was published are omitted."
  (let ((reads (leman-threads--local-reads room))
        (known (leman-threads--known-replies room session)))
    (cl-loop for (key . thread) in (map-elt content 'threads)
             for root-id = (leman-threads--key-name key)
             for count = (or (map-elt thread 'unread) 0)
             for latest-id = (map-elt thread 'latest_event_id)
             for latest-event = (or (and latest-id
                                         (leman-threads--event session latest-id))
                                    (car (cl-sort (copy-sequence (gethash root-id known))
                                                  #'> :key #'leman-event-origin-server-ts)))
             for latest-ts = (or (map-elt thread 'latest_ts)
                                 (and latest-event
                                      (leman-event-origin-server-ts latest-event)))
             when (and (> count 0)
                       (not (and latest-ts
                                 (>= (gethash root-id reads 0) latest-ts))))
             collect (make-leman-thread
                      :room room :session session :root-id root-id
                      :count count :mentions (or (map-elt thread 'mentions) 0)
                      :latest-ts latest-ts
                      :latest-event latest-event
                      :latest-event-id (or latest-id
                                           (and latest-event
                                                (leman-event-id latest-event)))))))

(defun leman-threads-unread (room session)
  "Return the unread threads of ROOM on SESSION, most recent first.
Return a list of `leman-thread' structs.  The result is cached
until the room's timeline, account data, or thread data change."
  (if (not (equal 'join (leman-room-status room)))
      nil
    (let* ((bridge-event (alist-get leman-threads-bridge-account-data-type
                                    (leman-room-account-data room) nil nil #'equal))
           (read-event (alist-get leman-threads-read-account-data-type
                                  (leman-room-account-data room) nil nil #'equal))
           (key (list (car (leman-room-timeline room)) bridge-event read-event
                      leman-threads--tick
                      ;; Until the room's read position is known.
                      (and (leman-threads--since room session) t)))
           (cached (map-elt (leman-room-local room) 'unread-threads)))
      ;; NOTE: Compared with `eq': `equal' would compare events deeply.
      (if (and cached (cl-every #'eq key (car cached)))
          (cdr cached)
        (let ((threads (cl-sort (if bridge-event
                                    (leman-threads--from-bridge
                                     room session (map-elt bridge-event 'content))
                                  (leman-threads--scan room session))
                                #'> :key (lambda (thread)
                                           (or (leman-thread-latest-ts thread) 0)))))
          (setf (map-elt (leman-room-local room) 'unread-threads)
                (cons key threads))
          threads)))))

(defun leman-threads-unread-thread (room session root-id)
  "Return the `leman-thread' for unread thread ROOT-ID in ROOM, or nil."
  (cl-find root-id (leman-threads-unread room session)
           :key #'leman-thread-root-id :test #'equal))

;;;; Main timeline

(defun leman-threads-main-counts (room session)
  "Return (COUNT . MENTIONS) unread outside threads in ROOM on SESSION.
From the bridge's account data when available; otherwise from the
server's notification counts, less the thread replies they
include.  Return nil if the server reports no counts."
  (if-let ((main (map-elt (leman-threads--account-data-content
                           room leman-threads-bridge-account-data-type)
                          'main)))
      (cons (or (map-elt main 'unread) 0) (or (map-elt main 'mentions) 0))
    (when-let ((notifications (leman-room-unread-notifications room)))
      (let ((count (map-elt notifications 'notification_count 0))
            (mentions (map-elt notifications 'highlight_count 0)))
        (if (and (zerop count) (zerop mentions))
            (cons 0 0)
          (pcase-let ((`(,thread-count . ,thread-mentions)
                       (leman-threads--counted-replies room session)))
            (cons (max 0 (- count thread-count))
                  (max 0 (- mentions thread-mentions)))))))))

(defun leman-threads--counted-replies (room session)
  "Return (COUNT . MENTIONS) of thread replies in ROOM's server counts.
On SESSION.  That is, replies from others since the server last
reset its counts: at the user's latest receipt or message.  (A
server implementing MSC3773 counts them apart, but Leman does not
request that, as not all servers do.)"
  (let* ((our-id (leman-user-id (leman-session-user session)))
         (receipt-ts (or (cdr (gethash our-id (leman-room-receipts room))) 0))
         (reset-ts (max receipt-ts
                        (or (cl-loop for event in (leman-room-timeline room)
                                     when (equal our-id (leman-user-id
                                                         (leman-event-sender event)))
                                     return (leman-event-origin-server-ts event))
                            0)))
         (count 0) (mentions 0))
    (cl-loop for event in (leman-room-timeline room)
             while (> (leman-event-origin-server-ts event) reset-ts)
             when (and (leman-threads--reply-p event)
                       (not (equal our-id (leman-user-id (leman-event-sender event)))))
             do (cl-incf count)
             (when (leman-threads--ping-p event room session)
               (cl-incf mentions)))
    (cons count mentions)))

(defun leman-threads-room-unread-p (room session)
  "Return non-nil if ROOM on SESSION has unread messages outside threads."
  (pcase (leman-threads-main-counts room session)
    (`(,count . ,mentions)
     (or (> count 0) (> mentions 0)
         (leman-threads--timeline-unread-p room session)))
    (_ (leman--room-unread-p room session))))

(defun leman-threads--timeline-unread-p (room session)
  "Return non-nil if ROOM's main timeline is unread per its read markers.
On SESSION.  Only when `leman-room-unread-only-counts-notifications'
is nil."
  (and (not leman-room-unread-only-counts-notifications)
       (leman--timeline-unread-p
        (leman-room-timeline room) (leman-room-receipts room)
        (map-nested-elt (alist-get "m.fully_read" (leman-room-account-data room)
                                   nil nil #'equal)
                        '(content event_id))
        (leman-user-id (leman-session-user session)))))

;;;; Marking read

(defun leman-threads-mark-read (room session root-id)
  "Mark thread ROOT-ID in ROOM on SESSION as read up to its latest reply.
The read position is saved in the room's account data, and, per
`leman-threads-send-receipts', a threaded read receipt is sent."
  (let* ((replies (gethash root-id (leman-threads--known-replies room session)))
         (latest (car (cl-sort (copy-sequence replies) #'>
                               :key #'leman-event-origin-server-ts)))
         (bridge-thread (leman-threads--map-get
                         (map-elt (leman-threads--account-data-content
                                   room leman-threads-bridge-account-data-type)
                                  'threads)
                         root-id))
         (ts (max (if latest (leman-event-origin-server-ts latest) 0)
                  (or (map-elt bridge-thread 'latest_ts) 0)))
         (reads (leman-threads--local-reads room)))
    (when (and (> ts 0) (> ts (leman-threads-read-ts room session root-id)))
      (puthash root-id ts reads)
      (cl-incf leman-threads--tick)
      (leman-threads--schedule-save room session)
      (when-let ((event (or (when-let ((id (map-elt bridge-thread 'latest_event_id)))
                              (leman-threads--event session id))
                            latest)))
        (when (pcase leman-threads-send-receipts
                ('bridged (leman-threads-bridged-p room))
                (value value))
          (leman-threads--send-receipt room session event root-id)))
      (run-hook-with-args 'leman-threads-read-functions room session root-id))))

(defun leman-threads--send-receipt (room session event root-id)
  "Send a read receipt for EVENT in thread ROOT-ID of ROOM on SESSION."
  (leman-api session (format "rooms/%s/receipt/m.read/%s"
                             (url-hexify-string (leman-room-id room))
                             (url-hexify-string (leman-event-id event)))
    :method 'post :version "v3"
    :data (json-encode (list (cons "thread_id" root-id)))
    :then #'ignore))

;;;; Fetching events

(defun leman-threads-ensure-event (room session event-id)
  "Return the event EVENT-ID of ROOM on SESSION, or fetch it.
When not known, nil is returned, and the event is fetched; when
it arrives, `leman-threads-event-functions' are run.  An event is
fetched at most once per Emacs session."
  (declare (function leman--make-event "leman"))
  (or (leman-threads--event session event-id)
      (progn
        (unless (gethash event-id leman-threads--fetching)
          (puthash event-id t leman-threads--fetching)
          (leman-api session (format "rooms/%s/event/%s"
                                     (url-hexify-string (leman-room-id room))
                                     (url-hexify-string event-id))
            :version "v3"
            :then (lambda (data)
                    (let ((event (leman--make-event data)))
                      (puthash event-id event (leman-session-events session))
                      (cl-incf leman-threads--tick)
                      (run-hook-with-args 'leman-threads-event-functions
                                          event room session)))
            ;; Not retried: e.g. the event may have been redacted.
            :else #'ignore))
        nil)))

;;;; Footer

(provide 'leman-threads)

;;; leman-threads.el ends here
