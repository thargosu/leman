;;;; leman-e2ee-tests.el --- Tests for leman-e2ee.el        -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the E2EE agent protocol layer.  Pure-protocol tests use
;; a fake in-memory transport; tests tagged `e2ee-real' drive the
;; real agent binary (skipped when it hasn't been built).

;;; Code:

(require 'ert)
(require 'json)

(require 'leman-structs)
(require 'leman-e2ee)
(require 'leman-api)

(declare-function leman--initial-transaction-id "leman")
(declare-function leman--push-joined-room-events "leman")
(declare-function leman-e2ee--announce-requests "leman")
(declare-function leman-e2ee--discard-store "leman-e2ee")
(declare-function leman-e2ee--store-path "leman-e2ee")
(declare-function leman--response-revoked-p "leman-api")
(declare-function leman--response-soft-logout-p "leman-api")
(declare-function leman--session-revoked "leman-api")
(declare-function leman--session-revoked-cleanup "leman")
(declare-function leman-session-revoked-p "leman-api")
(declare-function leman--sync-failed "leman")
(declare-function leman--sync-callback "leman")
(declare-function leman-api-session-revoked-p "leman-api")
(declare-function leman-room--send-typing "leman-room")
(declare-function leman-e2ee--decrypt-event "leman")
(declare-function leman-e2ee--encrypt-content "leman")
(declare-function leman-e2ee--format-emoji "leman")
(declare-function leman-e2ee--perform-outgoing-request "leman")
(declare-function leman-e2ee--advance-verification "leman")
(declare-function leman-e2ee--prompt-sas "leman")
(declare-function leman-e2ee--device-annotation "leman")
(declare-function leman--password-login "leman")
(declare-function leman--sso-login-with-token "leman")
(declare-function leman--new-session "leman")
(declare-function leman-e2ee--process-outgoing-requests "leman")
(declare-function leman-e2ee--process-outgoing-requests-sync "leman")
(declare-function leman-e2ee--sync-changes "leman")
(declare-function leman-e2ee--backup-pump "leman")
(declare-function leman-e2ee--account-data-get "leman")
(declare-function leman-e2ee--account-data-put "leman")
(declare-function leman-e2ee--unlock-backup-secret "leman")
(declare-function leman-e2ee--re-store-backup-secret "leman")
(declare-function leman-e2ee-ssss-check-key "leman-e2ee")
(declare-function leman-e2ee-backup-dump "leman")
(declare-function leman-e2ee-backup-recovery-key "leman-e2ee")
(declare-function leman-e2ee--export-keys "leman-e2ee")
(declare-function leman-e2ee--import-keys "leman-e2ee")
(declare-function leman-e2ee--format-recovery-key "leman-e2ee")

;;;; Helpers

(defconst leman-e2ee-tests--root
  ;; Captured at load time: `load-file-name' is only bound then.
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name default-directory))))
  "Repository root directory.")

(defun leman-e2ee-tests--agent-program ()
  "Return the path to a built agent binary, or nil."
  (seq-find #'file-executable-p
            (list (expand-file-name "e2ee/agent/target/debug/leman-agent" leman-e2ee-tests--root)
                  (expand-file-name "e2ee/agent/target/release/leman-agent" leman-e2ee-tests--root))))

(defun leman-e2ee-tests--fake-agent (responses)
  "Return (AGENT . SENT-LINES) for a fake agent.
RESPONSES maps command symbols to OK values; unmatched commands
get an empty OK object, and a value of the form (err CODE
MESSAGE) produces an error response.  Request ids are echoed;
SENT-LINES is a dummy-headed list whose cdr holds the lines sent
to the agent, newest first."
  (let ((sent-lines (list nil)))
    (cons (leman-e2ee--create
           :pending (make-hash-table :test #'eql)
           :fake (lambda (agent line)
                   (setcdr sent-lines (cons line (cdr sent-lines)))
                   (let* ((request (leman-e2ee--decode line))
                          (id (alist-get 'id request))
                          (cmd (alist-get 'cmd request))
                          (value (and (stringp cmd)
                                      (alist-get (intern cmd) responses)))
                          (body (pcase value
                                  ((pred (lambda (v) (and (consp v) (eq (car v) 'err))))
                                   (let ((code (nth 1 value))
                                         (message (nth 2 value)))
                                     (list (cons 'err (list (cons 'code code)
                                                            (cons 'message message))))))
                                  (_ (list (cons 'ok (or value (list))))))))
                     (leman-e2ee-handle-line
                      agent (json-encode (cons (cons 'id id) body))))))
          sent-lines)))

;;;; Encoding/decoding

(ert-deftest leman-e2ee-encode ()
  (should (equal (leman-e2ee--decode (leman-e2ee--encode 5 "hello" nil))
                 '((id . 5) (cmd . "hello"))))
  (should (equal (leman-e2ee--decode
                  (leman-e2ee--encode 6 "update_tracked_users"
                                      (list (cons 'users (vector "@a:x.org")))))
                 '((id . 6) (cmd . "update_tracked_users")
                   (params . ((users . ["@a:x.org"])))))))

(ert-deftest leman-e2ee-decode-invalid ()
  (should (null (leman-e2ee--decode "not json"))))

;;;; Response handling

(ert-deftest leman-e2ee-handle-line-stores-responses ()
  (let ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql))))
    (leman-e2ee-handle-line agent "{\"id\":1,\"ok\":{\"x\":2}}")
    (should (equal (gethash 1 (leman-e2ee-pending agent))
                   '(ok . ((x . 2)))))
    (leman-e2ee-handle-line agent "{\"id\":2,\"err\":{\"code\":\"parse\",\"message\":\"nope\"}}")
    (should (equal (gethash 2 (leman-e2ee-pending agent))
                   '(err . ((code . "parse") (message . "nope")))))
    ;; An empty "ok" object must be a success, not an error.
    (leman-e2ee-handle-line agent "{\"id\":3,\"ok\":{}}")
    (should (equal (gethash 3 (leman-e2ee-pending agent))
                   '(ok)))))

(ert-deftest leman-e2ee-handle-line-ignores-stray-and-garbage ()
  (let ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql))))
    (leman-e2ee-handle-line agent "complete garbage")
    (leman-e2ee-handle-line agent "{\"err\":{\"code\":\"parse\"}}")
    (should (zerop (hash-table-count (leman-e2ee-pending agent))))))

;;;; Requests with the fake transport

(ert-deftest leman-e2ee-request-correlates-by-id ()
  ;; A stray response with a different id must not satisfy the request.
  (let* ((agent (leman-e2ee-tests--fake-agent nil))
         ;; Send two responses by hand, the correct one last.
         (sent-lines (cdr agent)))
    ;; The transport must stay silent: otherwise it would answer the
    ;; request (and clobber the preloaded id) as soon as it is sent.
    (setf (leman-e2ee-fake (car agent)) (lambda (_agent _line) nil))
    (setcdr sent-lines (list "{\"id\":99,\"ok\":{\"stray\":true}}"
                             "{\"id\":1,\"ok\":{\"value\":42}}"))
    (dolist (line (cdr sent-lines))
      (leman-e2ee-handle-line (car agent) line))
    (should (equal (alist-get 'value (leman-e2ee-request (car agent) "hello"))
                   42))))

(ert-deftest leman-e2ee-request-signals-error ()
  (let ((agent (car (leman-e2ee-tests--fake-agent
                     (list (cons 'hello '(err "crypto" "boom")))))))
    (should-error (leman-e2ee-request agent "hello")
                  :type 'leman-e2ee-error)))

(ert-deftest leman-e2ee-request-times-out ()
  (let ((leman-e2ee-request-timeout 0.1)
        (agent (car (leman-e2ee-tests--fake-agent nil))))
    ;; The fake responds to every command; stub out its transport so
    ;; nothing is answered.
    (setf (leman-e2ee-fake agent) (lambda (_agent _line) nil))
    (should-error (leman-e2ee-request agent "hello")
                  :type 'leman-e2ee-error)))

(ert-deftest leman-e2ee-request-no-transport ()
  (let ((agent (leman-e2ee--create)))
    (should-error (leman-e2ee-request agent "hello")
                  :type 'leman-e2ee-error)))

;;;; Real agent (requires a built binary)

(ert-deftest leman-e2ee-real-start-initialize-stop ()
  :tags '(e2ee-real)
  (skip-unless (leman-e2ee-tests--agent-program))
  (let* ((leman-e2ee-agent-program (leman-e2ee-tests--agent-program))
         (store (make-temp-file "leman-e2ee-test-" t))
         (agent (leman-e2ee-start "@bob:example.org" "TESTDEVICE" store)))
    (unwind-protect
        (let ((identity-keys (leman-e2ee-identity-keys agent)))
          (should (stringp (alist-get 'curve25519 identity-keys)))
          (should (stringp (alist-get 'ed25519 identity-keys))))
      (leman-e2ee-stop agent))))

(ert-deftest leman-e2ee-real-persistence ()
  :tags '(e2ee-real)
  (skip-unless (leman-e2ee-tests--agent-program))
  (let* ((leman-e2ee-agent-program (leman-e2ee-tests--agent-program))
         (store (make-temp-file "leman-e2ee-test-" t))
         (first (leman-e2ee-start "@bob:example.org" "TESTDEVICE" store))
         (identity-keys (leman-e2ee-identity-keys first)))
    (leman-e2ee-stop first)
    (let* ((second (leman-e2ee-start "@bob:example.org" "TESTDEVICE" store))
           (identity-keys-2 (leman-e2ee-identity-keys second)))
      (unwind-protect
          (should (equal identity-keys identity-keys-2))
        (leman-e2ee-stop second)))))

(ert-deftest leman-e2ee-real-outgoing-requests ()
  :tags '(e2ee-real)
  (skip-unless (leman-e2ee-tests--agent-program))
  (let* ((leman-e2ee-agent-program (leman-e2ee-tests--agent-program))
         (store (make-temp-file "leman-e2ee-test-" t))
         (agent (leman-e2ee-start "@bob:example.org" "TESTDEVICE" store)))
    (unwind-protect
        (let ((requests (leman-e2ee-outgoing-requests agent)))
          (should requests)
          (let ((request (elt requests 0)))
            (should (member (alist-get 'method request) '("POST" "PUT")))
            (should (string-prefix-p "/_matrix/client/"
                                     (alist-get 'path request)))
            (should (alist-get 'body request))))
      (leman-e2ee-stop agent))))

;;;; Decrypting events

(ert-deftest leman-e2ee-decrypt-event ()
  (let* ((decrypted-event (list (cons 'type "m.room.message")
                                (cons 'content (list (cons 'body "decrypted!")))))
         (fake (leman-e2ee-tests--fake-agent
                (list (cons 'decrypt_room_event
                            (list (cons 'event decrypted-event)
                                  (cons 'shield "None"))))))
         (event (list (cons 'type "m.room.encrypted")
                      (cons 'room_id "!room:x.org")
                      (cons 'content (list (cons 'algorithm "m.megolm.v1.aes-sha2"))))))
    ;; The shield state is carried on the returned event (the agent's
    ;; Red/Grey objects are normalized).
    (should (equal (leman-e2ee-decrypt-event (car fake) event)
                   (cons '(shield none) decrypted-event)))
    ;; A missing shield (an older agent) still decrypts.
    (let ((fake (leman-e2ee-tests--fake-agent
                 (list (cons 'decrypt_room_event (list (cons 'event decrypted-event)))))))
      (should (equal (leman-e2ee-decrypt-event (car fake) event)
                     (cons '(shield none) decrypted-event))))
    ;; The Red/Grey shape is normalized to (KIND CODE MESSAGE).
    (let ((fake (leman-e2ee-tests--fake-agent
                 (list (cons 'decrypt_room_event
                             (list (cons 'event decrypted-event)
                                   (cons 'shield
                                         '((Red (code . "UnverifiedIdentity")
                                                (message . "The sender's identity was not verified"))))))))))
      (should (equal (leman-e2ee-decrypt-event (car fake) event)
                     (cons '(shield red "UnverifiedIdentity"
                                    "The sender's identity was not verified")
                           decrypted-event))))))

(ert-deftest leman-e2ee-decrypt-event-failure-returns-original ()
  (let* ((fake (leman-e2ee-tests--fake-agent
                (list (cons 'decrypt_room_event '(err "crypto" "session not found")))))
         (event (list (cons 'type "m.room.encrypted")
                      (cons 'room_id "!room:x.org")
                      (cons 'content (list (cons 'algorithm "m.megolm.v1.aes-sha2"))))))
    (should (equal (leman-e2ee-decrypt-event (car fake) event)
                   event))))

(ert-deftest leman-e2ee-decrypt-event-ignores-plaintext ()
  (let* ((fake (leman-e2ee-tests--fake-agent nil))
         (event (list (cons 'type "m.room.message")
                      (cons 'content (list (cons 'body "hi"))))))
    (should (equal (leman-e2ee-decrypt-event (car fake) event) event))))

;;;; Integration with the sync flow

(ert-deftest leman-e2ee-split-path ()
  (should (equal (leman-e2ee--split-path "/_matrix/client/v3/keys/upload")
                 (list "v3" "keys/upload")))
  (should (equal (leman-e2ee--split-path
                  "/_matrix/client/v3/sendToDevice/m.room.encrypted/txn1")
                 (list "v3" "sendToDevice/m.room.encrypted/txn1")))
  (should-error (leman-e2ee--split-path "https://example.org/whatever")))

(ert-deftest leman-e2ee-session-decrypt-event ()
  ;; With an agent: encrypted events are decrypted (and get a room ID
  ;; injected when the caller knows it); without one: unchanged.
  (let* ((decrypted-event (list (cons 'type "m.room.message")
                                (cons 'content (list (cons 'body "shh")))))
         (fake (leman-e2ee-tests--fake-agent
                (list (cons 'decrypt_room_event (list (cons 'event decrypted-event))))))
         (session (make-leman-session))
         (event (list (cons 'type "m.room.encrypted")
                      (cons 'content (list (cons 'algorithm "m.megolm.v1.aes-sha2"))))))
    (setf (leman-session-e2ee session) (car fake))
    (let ((result (leman-e2ee--decrypt-event session event "!room:x.org")))
      (should (equal (alist-get 'body (alist-get 'content result)) "shh")))
    ;; Without an agent the event is returned unchanged.
    (setf (leman-session-e2ee session) nil)
    (should (equal (leman-e2ee--decrypt-event session event "!room:x.org")
                   event))))

(ert-deftest leman-e2ee-sync-changes-sends-to-agent ()
  (let* ((fake (leman-e2ee-tests--fake-agent nil))
         (session (make-leman-session))
         (to-device-event (list (cons 'type "m.room.encrypted")
                                (cons 'sender "@a:x.org")))
         (data (list (cons 'next_batch "s42")
                     (cons 'to_device (list (cons 'events (vector to-device-event))))
                     (cons 'device_lists (list (cons 'changed (vector "@a:x.org"))
                                               (cons 'left (vector))))
                     (cons 'device_one_time_keys_count
                           (list (cons 'signed_curve25519 100))))))
    (setf (leman-session-e2ee session) (car fake))
    (leman-e2ee--sync-changes session data)
    (let* ((lines (cdr fake))
           (request (seq-find
                     (lambda (line)
                       (equal (alist-get 'cmd (leman-e2ee--decode line))
                              "receive_sync_changes"))
                     lines)))
      (should request)
      (let ((params (alist-get 'params (leman-e2ee--decode request))))
        (should (equal (alist-get 'next_batch_token params) "s42"))
        (should (equal (alist-get 'type (elt (alist-get 'to_device_events params) 0))
                       "m.room.encrypted"))
        (should (equal (elt (alist-get 'changed (alist-get 'changed_devices params)) 0)
                       "@a:x.org"))
        (should (equal (alist-get 'signed_curve25519
                                  (alist-get 'one_time_keys_count params))
                       100))))))
(ert-deftest leman-e2ee-sync-changes-pumps-outgoing-requests ()
  (let* ((fake (leman-e2ee-tests--fake-agent
                (list (cons 'outgoing_requests
                            (list (cons 'requests (vector
                                                   (list (cons 'id "req1")
                                                         (cons 'method "POST")
                                                         (cons 'path "/_matrix/client/v3/keys/upload")
                                                         (cons 'body "{\"device_keys\":{},\"one_time_keys\":{}}")))))))))
         (session (make-leman-session))
         (performed nil))
    (setf (leman-session-e2ee session) (car fake))
    ;; Stub the HTTP layer: `leman-api' would perform a real request.
    (cl-letf (((symbol-function #'leman-e2ee--perform-outgoing-request)
               (lambda (_session _agent request)
                 (push request performed))))
      (leman-e2ee--sync-changes session (list (cons 'next_batch "s1"))))
    (should (equal (alist-get 'path (car performed))
                   "/_matrix/client/v3/keys/upload"))
    (should (equal (alist-get 'id (car performed)) "req1"))))

(ert-deftest leman-e2ee-outgoing-request-body-passed-through ()
  ;; Request bodies are pre-encoded JSON strings from the agent; the
  ;; pump must pass them through verbatim (re-encoding through elisp
  ;; corrupts empty objects, which elisp cannot represent).
  (let* ((body "{\"device_keys\":{},\"one_time_keys\":{}}")
         (fake (leman-e2ee-tests--fake-agent
                (list (cons 'outgoing_requests
                            (list (cons 'requests (vector
                                                   (list (cons 'id "req1")
                                                         (cons 'method "POST")
                                                         (cons 'path "/_matrix/client/v3/keys/upload")
                                                         (cons 'body body)))))))))
         (session (make-leman-session))
         (bodies nil))
    (setf (leman-session-e2ee session) (car fake))
    ;; Stub the HTTP layer and capture the encoded request bodies.
    (cl-letf (((symbol-function #'leman-api)
               (lambda (_session _endpoint &rest args)
                 (push (plist-get args :data) bodies))))
      (leman-e2ee--process-outgoing-requests session)
      (should (equal (car bodies) body)))))

(ert-deftest leman-push-joined-room-events-dedups-redelivered-events ()
  ;; An event re-delivered by a later sync (e.g. after a limited
  ;; timeline, or a second concurrent sync) must not appear twice.
  ;; NOTE: Each sync response carries fresh event vectors (the push
  ;; path converts them in place).
  (let* ((session (make-leman-session))
         (room-data (lambda ()
                      (list (cons 'timeline
                                  (list (cons 'events
                                              (vector (list (cons 'type "m.room.message")
                                                            (cons 'sender "@alice:x.org")
                                                            (cons 'origin_server_ts 42)
                                                            (cons 'event_id "$dup")
                    (cons 'content (list (cons 'body "once")
                                         (cons 'msgtype "m.text"))))))))))))
    (setf (leman-session-events session) (make-hash-table :test #'equal))
    (leman--push-joined-room-events session (cons (intern "!room:x.org") (funcall room-data)))
    (leman--push-joined-room-events session (cons (intern "!room:x.org") (funcall room-data)))
    (should (= 1 (length (leman-room-timeline
                          (car (leman-session-rooms session))))))))

(ert-deftest leman-e2ee-push-room-events-decrypts ()
  ;; Full push-path integration: an encrypted timeline event is
  ;; decrypted before being turned into an event struct.
  (let* ((decrypted-event (list (cons 'type "m.room.message")
                                (cons 'sender "@alice:x.org")
                                (cons 'origin_server_ts 42)
                                (cons 'content (list (cons 'body "It's a secret")
                                                     (cons 'msgtype "m.text")))))
         (fake (leman-e2ee-tests--fake-agent
                (list (cons 'decrypt_room_event (list (cons 'event decrypted-event))))))
         (session (make-leman-session))
         (encrypted-event (list (cons 'type "m.room.encrypted")
                                (cons 'sender "@alice:x.org")
                                (cons 'origin_server_ts 42)
                                (cons 'event_id "$enc1")
                                (cons 'content (list (cons 'algorithm "m.megolm.v1.aes-sha2"))))))
    (setf (leman-session-e2ee session) (car fake))
    (setf (leman-session-events session) (make-hash-table :test #'equal))
    (leman--push-joined-room-events
     session
     (cons (intern "!room:x.org")
           (list (cons 'timeline (list (cons 'events (vector encrypted-event)))))))
    (let ((room (car (leman-session-rooms session)))
          (event (car (leman-room-timeline (car (leman-session-rooms session))))))
      (should (equal (leman-room-id room) "!room:x.org"))
      (should (equal (leman-event-type event) "m.room.message"))
      (should (equal (alist-get 'body (leman-event-content event))
                     "It's a secret")))))

;;;; E2: encrypting outgoing events

(ert-deftest leman-e2ee-encrypt-event ()
  (let* ((encrypted-content (list (cons 'algorithm "m.megolm.v1.aes-sha2")
                                  (cons 'ciphertext "opaque")))
         (fake (leman-e2ee-tests--fake-agent
                (list (cons 'encrypt_room_event
                            (list (cons 'status "ok")
                                  (cons 'event (list (cons 'type "m.room.encrypted")
                                                     (cons 'content encrypted-content))))))))
         (response (leman-e2ee-encrypt-event (car fake) "!room:x.org"
                                             "m.room.message"
                                             '((msgtype . "m.text") (body . "hi"))
                                             ["@alice:x.org"])))
    (should (equal (alist-get 'status response) "ok"))
    (should (equal (alist-get 'content (alist-get 'event response))
                   encrypted-content))
    ;; The request carried the room, type, content, and members.
    (let* ((line (cadr (cdr fake)))
           (params (alist-get 'params (leman-e2ee--decode line))))
      (should (equal (alist-get 'room_id params) "!room:x.org"))
      (should (equal (alist-get 'event_type params) "m.room.message"))
      (should (equal (elt (alist-get 'users params) 0) "@alice:x.org")))))

(ert-deftest leman-e2ee--encrypt-content-claims-then-encrypts ()
  ;; The full send flow: track members, pump, encrypt (retrying while
  ;; claims are pending, pumping between attempts), and pump again for
  ;; the key shares.  Returns the encrypted content and event type.
  (let* ((encrypted-content (list (cons 'algorithm "m.megolm.v1.aes-sha2")
                                  (cons 'ciphertext "opaque")))
         (encrypt-count 0)
         ;; The fake responds \"claims_pending\" once, then \"ok\".
         (fake (leman-e2ee-tests--fake-agent nil))
         (session (make-leman-session))
         (room (make-leman-room :id "!room:x.org"
                                :members (make-hash-table :test #'equal)))
         (content '((msgtype . "m.text") (body . "hi"))))
    (setf (leman-session-e2ee session) (car fake))
    (puthash "@alice:x.org" (make-leman-user :id "@alice:x.org")
             (leman-room-members room))
    ;; A room with an m.room.encryption state event.
    (setf (leman-room-state room)
          (list (make-leman-event :id "$enc-state" :type "m.room.encryption"
                                  :content '((algorithm . "m.megolm.v1.aes-sha2")))))
    ;; Make the fake's dispatch dynamic: claims_pending then ok.
    (setf (leman-e2ee-fake (car fake))
          (lambda (agent line)
            (let* ((request (leman-e2ee--decode line))
                   (id (alist-get 'id request))
                   (cmd (alist-get 'cmd request)))
              (pcase cmd
                ("encrypt_room_event"
                 (cl-incf encrypt-count)
                 (leman-e2ee-handle-line
                  agent
                  (if (= encrypt-count 1)
                      (json-encode `((id . ,id) (ok . ((status . "claims_pending")))))
                    (json-encode `((id . ,id)
                                   (ok . ((status . "ok")
                                          (event . ((type . "m.room.encrypted")
                                                    (content . ,encrypted-content))))))))))
                (_ (leman-e2ee-handle-line
                    agent (json-encode `((id . ,id) (ok)))))))))
    (cl-letf (((symbol-function #'leman-api)
               (lambda (_session _endpoint &rest _args) nil)))
      (let ((result (leman-e2ee--encrypt-content session room content)))
        (should (equal (cdr result) "m.room.encrypted"))
        (should (equal (car result) encrypted-content))))
    (should (= encrypt-count 2))))

(ert-deftest leman-e2ee--encrypt-content-plaintext-rooms-untouched ()
  ;; A room without encryption state (or without an agent) passes the
  ;; content through unchanged.
  (let* ((session (make-leman-session))
         (room (make-leman-room :id "!room:x.org"))
         (content '((msgtype . "m.text") (body . "hi"))))
    (let ((result (leman-e2ee--encrypt-content session room content)))
      (should (equal result (cons content "m.room.message"))))))

(ert-deftest leman-e2ee--encrypt-content-warns-on-plaintext-into-encrypted-room ()
  ;; Sending plaintext into a room whose timeline contains encrypted
  ;; events (i.e. the room really is encrypted but encryption isn't
  ;; active for us) must warn loudly, not silently.
  (let* ((session (make-leman-session))
         (room (make-leman-room :id "!room:x.org"))
         (content '((msgtype . "m.text") (body . "hi")))
         (warnings nil))
    (setf (leman-room-timeline room)
          (list (make-leman-event :id "$enc1" :type "m.room.encrypted"
                                  :content '((algorithm . "m.megolm.v1.aes-sha2")))))
    (cl-letf (((symbol-function #'leman-message)
               (lambda (format &rest args)
                 (push (apply #'format format args) warnings))))
      (leman-e2ee--encrypt-content session room content))
    (should (= 1 (length warnings)))
    (should (string-match-p "NOT be encrypted" (car warnings)))))

(ert-deftest leman-e2ee--process-outgoing-requests-sync-performs-and-marks ()
  ;; The send path's pump performs requests synchronously (the caller
  ;; must be able to rely on claims/shares being done when it
  ;; returns) and reports the responses to the agent.
  (let* ((fake (leman-e2ee-tests--fake-agent nil))
         (session (make-leman-session))
         (outgoing-count 0))
    (setf (leman-session-e2ee session) (car fake))
    ;; The agent wants one keys/claim performed, then nothing more.
    ;; NOTE: The dynamic fake must push the line itself (it replaces
    ;; the fake installed by the helper).
    (setf (leman-e2ee-fake (car fake))
          (lambda (agent line)
            (let ((sent-lines (cdr fake)))
              (setcdr sent-lines (cons line (cdr sent-lines)))
              (let* ((request (leman-e2ee--decode line))
                     (id (alist-get 'id request))
                     (cmd (alist-get 'cmd request)))
                (pcase cmd
                  ("outgoing_requests"
                   (cl-incf outgoing-count)
                   (leman-e2ee-handle-line
                    agent
                    (if (= outgoing-count 1)
                        (json-encode
                         `((id . ,id)
                           (ok . ((requests . [((id . "req1")
                                                (method . "POST")
                                                (path . "/_matrix/client/v3/keys/claim")
                                                (body . "{\"one_time_keys\":{},\"timeout\":null}"))])))))
                      (json-encode `((id . ,id) (ok . ((requests . []))))))))
                  (_ (leman-e2ee-handle-line
                      agent (json-encode `((id . ,id) (ok))))))))))
    (cl-letf (((symbol-function #'leman-api)
               (lambda (_session _endpoint &rest args)
                 (should (eq (plist-get args :then) 'sync))
                 '((one_time_keys)))))
      (leman-e2ee--process-outgoing-requests-sync session))
    (let* ((lines (cdr (cdr fake)))
           (mark (seq-find (lambda (line)
                             (equal (alist-get 'cmd (leman-e2ee--decode line))
                                    "mark_request_as_sent"))
                           lines)))
      (should mark)
      (should (equal (alist-get 'request_id
                                (alist-get 'params (leman-e2ee--decode mark)))
                     "req1")))))

(ert-deftest leman-send-message-encrypts-in-encrypted-rooms ()
  ;; Sending into an encrypted room sends an m.room.encrypted event
  ;; with the agent's encrypted content.
  (let* ((encrypted-content (list (cons 'algorithm "m.megolm.v1.aes-sha2")
                                  (cons 'ciphertext "opaque")))
         (fake (leman-e2ee-tests--fake-agent
                (list (cons 'encrypt_room_event
                            (list (cons 'status "ok")
                                  (cons 'event (list (cons 'type "m.room.encrypted")
                                                     (cons 'content encrypted-content))))))))
         (session (make-leman-session :transaction-id (leman--initial-transaction-id)))
         (room (make-leman-room :id "!room:x.org"
                                :members (make-hash-table :test #'equal)))
         (requests nil))
    (setf (leman-session-e2ee session) (car fake))
    (setf (leman-room-state room)
          (list (make-leman-event :id "$enc-state" :type "m.room.encryption"
                                  :content '((algorithm . "m.megolm.v1.aes-sha2")))))
    (cl-letf (((symbol-function #'leman-api)
               (lambda (_session endpoint &rest args)
                 (push (cons endpoint (plist-get args :data)) requests))))
      (let ((leman-encrypt-send-content-function #'leman-e2ee--encrypt-content))
        (leman-send-message room session :body "hi")))
    (let ((request (car requests)))
      (should (string-match-p "/send/m.room.encrypted/" (car request)))
      (should (string-match-p "opaque" (cdr request)))
      (should-not (string-match-p "\"body\"" (cdr request))))))

;;;; Verification (E3)

(ert-deftest leman-e2ee-verification-commands-speak-the-protocol ()
  ;; Each verification wrapper sends its documented command and
  ;; returns the documented part of the response.
  (let* ((fake (leman-e2ee-tests--fake-agent
                (list (cons 'devices
                            (list (cons 'devices
                                        (vector (list (cons 'device_id "ABC")
                                                      (cons 'display_name "Element")
                                                      (cons 'verified nil)
                                                      (cons 'deleted nil))))))
                      (cons 'request_verification
                            (list (cons 'flow_id "flow1")))
                      (cons 'verification_requests
                            (list (cons 'requests
                                        (vector (list (cons 'flow_id "flow1")
                                                      (cons 'user_id "@vv:x.org")
                                                      (cons 'device_id "ABC")
                                                      (cons 'state "ready")
                                                      (cons 'we_started nil)
                                                      (cons 'sas nil))))))
                      (cons 'verification_sas
                            (list (cons 'can_be_presented t)
                                  (cons 'accepted t)
                                  (cons 'done nil)
                                  (cons 'cancelled nil)
                                  (cons 'emoji
                                        (vector (list (cons 'number 49)
                                                      (cons 'symbol "🦋")
                                                      (cons 'description "butterfly")))))))))
         (agent (car fake))
         (sent (lambda ()
                 (mapcar #'leman-e2ee--decode (cdr (cdr fake))))))
    (should (equal (leman-e2ee-devices agent)
                   (vector (list (cons 'device_id "ABC")
                                 (cons 'display_name "Element")
                                 (cons 'verified nil)
                                 (cons 'deleted nil)))))
    ;; Devices called without a user ID sends no params.
    (should (null (alist-get 'params (car (funcall sent)))))
    (should (equal (leman-e2ee-request-verification agent "@vv:x.org" "ABC") "flow1"))
    (should (equal (alist-get 'params (car (funcall sent)))
                   '((user_id . "@vv:x.org") (device_id . "ABC"))))
    (should (equal (leman-e2ee-verification-requests agent)
                   (vector (list (cons 'flow_id "flow1")
                                 (cons 'user_id "@vv:x.org")
                                 (cons 'device_id "ABC")
                                 (cons 'state "ready")
                                 (cons 'we_started nil)
                                 (cons 'sas nil)))))
    (should (equal (leman-e2ee-accept-verification agent "@vv:x.org" "flow1")
                   nil))
    (should (equal (alist-get 'params (car (funcall sent)))
                   '((user_id . "@vv:x.org") (flow_id . "flow1"))))
    (should (equal (leman-e2ee-start-sas agent "@vv:x.org" "flow1") nil))
    (should (equal (leman-e2ee-verification-sas agent "@vv:x.org" "flow1")
                   (list (cons 'can_be_presented t)
                         (cons 'accepted t)
                         (cons 'done nil)
                         (cons 'cancelled nil)
                         (cons 'emoji
                               (vector (list (cons 'number 49)
                                             (cons 'symbol "🦋")
                                             (cons 'description "butterfly")))))))
    (should (equal (leman-e2ee-accept-sas agent "@vv:x.org" "flow1") nil))
    (should (equal (leman-e2ee-confirm-sas agent "@vv:x.org" "flow1") nil))
    (should (equal (leman-e2ee-cancel-verification agent "@vv:x.org" "flow1") nil))
    ;; Every verification command carries user_id and flow_id.
    (let ((lines (mapcar #'leman-e2ee--decode (cdr (cdr fake)))))
      (dolist (cmd '("accept_verification" "start_sas" "verification_sas"
                     "accept_sas" "confirm_sas" "cancel_verification"))
        (let ((params (alist-get 'params
                                 (seq-find (lambda (line)
                                             (equal (alist-get 'cmd line) cmd))
                                           lines))))
          (should (equal (alist-get 'user_id params) "@vv:x.org"))
          (should (equal (alist-get 'flow_id params) "flow1")))))))

(ert-deftest leman-e2ee-devices-passes-user-id ()
  (let* ((fake (leman-e2ee-tests--fake-agent
                (list (cons 'devices (list (cons 'devices (vector)))))))
         (agent (car fake)))
    (leman-e2ee-devices agent "@vv:x.org")
    (should (equal (alist-get 'params
                              (leman-e2ee--decode (car (cdr (cdr fake)))))
                   '((user_id . "@vv:x.org"))))))

(ert-deftest leman-e2ee-verification-commands-propagate-errors ()
  (let* ((fake (leman-e2ee-tests--fake-agent
                (list (cons 'verification_sas '(err "crypto" "no SAS for flow flow1")))))
         (agent (car fake)))
    (should (equal (condition-case err
                       (progn (leman-e2ee-verification-sas agent "@vv:x.org" "flow1")
                              nil)
                     (leman-e2ee-error (cdr err)))
                   '("crypto" "no SAS for flow flow1")))))

(ert-deftest leman-e2ee--format-emoji ()
  (should (equal (leman-e2ee--format-emoji
                  (vector (list (cons 'symbol "🦋") (cons 'description "butterfly"))
                          (list (cons 'symbol "🐟") (cons 'description "fish"))))
                 "🦋 butterfly   🐟 fish")))

(defun leman-e2ee-tests--start-verification (session user-id flow-id device-id)
  "Register the active verification dance for SESSION.
Clears `leman-e2ee--active-verification' beforehand; the caller
must still clear it afterwards (e.g. with `unwind-protect')."
  ;; The keys are seeded like `leman-e2ee-verify' does: `setf' of
  ;; `plist-get' only mutates in place for existing keys.
  (setf leman-e2ee--active-verification
        (list :session session :user-id user-id :flow-id flow-id
              :device-id device-id :prompted-p nil :prompt-timer nil)))

(ert-deftest leman-e2ee--advance-verification-starts-sas-when-ready ()
  ;; A ready request without a SAS object: send start, keep waiting.
  (let* ((fake (leman-e2ee-tests--fake-agent
                (list (cons 'verification_requests
                            (list (cons 'requests
                                        (vector (list (cons 'flow_id "flow1")
                                                      (cons 'state "ready")
                                                      (cons 'sas nil)))))))))
         (session (make-leman-session :e2ee (car fake))))
    (unwind-protect
        (progn
          (leman-e2ee-tests--start-verification session "@vv:x.org" "flow1" "ABC")
          (leman-e2ee--advance-verification session (car fake))
          (should leman-e2ee--active-verification)
          (should (equal (alist-get 'cmd (leman-e2ee--decode (car (cdr (cdr fake)))))
                         "start_sas")))
      (setf leman-e2ee--active-verification nil))))

(ert-deftest leman-e2ee--advance-verification-accepts-their-sas-start ()
  ;; Their SAS start arrived (a SAS object exists but we have not
  ;; accepted it): accept it, or the other device waits forever.
  (let* ((fake (leman-e2ee-tests--fake-agent
                (list (cons 'verification_requests
                            (list (cons 'requests
                                        (vector (list (cons 'flow_id "flow1")
                                                      (cons 'state "ready")
                                                      (cons 'sas t))))))
                      (cons 'verification_sas
                            (list (cons 'accepted nil)
                                  (cons 'can_be_presented nil)
                                  (cons 'done nil)
                                  (cons 'cancelled nil))))))
         (session (make-leman-session :e2ee (car fake))))
    (unwind-protect
        (progn
          (leman-e2ee-tests--start-verification session "@vv:x.org" "flow1" "ABC")
          (leman-e2ee--advance-verification session (car fake))
          (should leman-e2ee--active-verification)
          (should (equal (alist-get 'cmd (leman-e2ee--decode (car (cdr (cdr fake)))))
                         "accept_sas")))
      (setf leman-e2ee--active-verification nil))))

(ert-deftest leman-e2ee--advance-verification-presents-emoji-once ()
  ;; The emoji can be compared: schedule the prompt (once; the dance
  ;; is prompted from a timer, at top level).
  (let* ((fake (leman-e2ee-tests--fake-agent
                (list (cons 'verification_requests
                            (list (cons 'requests
                                        (vector (list (cons 'flow_id "flow1")
                                                      (cons 'device_id "ABC")
                                                      (cons 'state "ready")
                                                      (cons 'sas t))))))
                      (cons 'verification_sas
                            (list (cons 'accepted t)
                                  (cons 'can_be_presented t)
                                  (cons 'done nil)
                                  (cons 'cancelled nil)
                                  (cons 'emoji
                                        (vector (list (cons 'symbol "🦋")
                                                      (cons 'description "butterfly")))))))))
         (session (make-leman-session :e2ee (car fake)))
         (callbacks nil))
    (cl-letf (((symbol-function #'run-at-time)
               (lambda (_seconds _repeat function &rest _args)
                 (push function callbacks)
                 'fake-timer)))
      (unwind-protect
          (progn
            (leman-e2ee-tests--start-verification session "@vv:x.org" "flow1" "ABC")
            (leman-e2ee--advance-verification session (car fake))
            (should (= (length callbacks) 1))
            (should (plist-get leman-e2ee--active-verification :prompted-p))
            ;; Already prompted: the next sync does not re-prompt.
            (leman-e2ee--advance-verification session (car fake))
            (should (= (length callbacks) 1)))
        (setf leman-e2ee--active-verification nil)))))

(ert-deftest leman-e2ee--prompt-sas-confirms-and-pumps ()
  ;; Answering yes confirms the SAS and pumps the MAC request out.
  (let* ((fake (leman-e2ee-tests--fake-agent nil))
         (session (make-leman-session :e2ee (car fake)))
         (timer-fn nil)
         (pumps 0)
         (leman-e2ee-verify-confirm-function (lambda (&rest _) t)))
    (cl-letf (((symbol-function #'run-at-time)
               (lambda (_seconds _repeat function &rest _args)
                 (setf timer-fn function)
                 'fake-timer))
              ((symbol-function #'leman-e2ee--process-outgoing-requests)
               (lambda (&rest _) (cl-incf pumps))))
      (unwind-protect
          (progn
            (leman-e2ee-tests--start-verification session "@vv:x.org" "flow1" "ABC")
            (leman-e2ee--prompt-sas session "ABC" (vector))
            (funcall timer-fn)
            (should (= pumps 1))
            (should (equal (alist-get 'cmd (leman-e2ee--decode (car (cdr (cdr fake)))))
                           "confirm_sas")))
        (setf leman-e2ee--active-verification nil)))))

(ert-deftest leman-e2ee--prompt-sas-cancels-on-emoji-mismatch ()
  (let* ((fake (leman-e2ee-tests--fake-agent nil))
         (session (make-leman-session :e2ee (car fake)))
         (timer-fn nil)
         (pumps 0)
         (leman-e2ee-verify-confirm-function (lambda (&rest _) nil)))
    (cl-letf (((symbol-function #'run-at-time)
               (lambda (_seconds _repeat function &rest _args)
                 (setf timer-fn function)
                 'fake-timer))
              ((symbol-function #'leman-e2ee--process-outgoing-requests)
               (lambda (&rest _) (cl-incf pumps))))
      (unwind-protect
          (progn
            (leman-e2ee-tests--start-verification session "@vv:x.org" "flow1" "ABC")
            (leman-e2ee--prompt-sas session "ABC" (vector))
            (funcall timer-fn)
            (should (= pumps 1))
            (should (equal (alist-get 'cmd (leman-e2ee--decode (car (cdr (cdr fake)))))
                           "cancel_verification")))
        (setf leman-e2ee--active-verification nil)))))

(ert-deftest leman-e2ee--prompt-sas-defers-on-quit ()
  ;; C-g during the prompt defers it: the dance stays active and
  ;; `leman-e2ee-verify' can re-prompt.
  (let* ((fake (leman-e2ee-tests--fake-agent nil))
         (session (make-leman-session :e2ee (car fake)))
         (timer-fn nil)
         (leman-e2ee-verify-confirm-function (lambda (&rest _) (signal 'quit nil))))
    (cl-letf (((symbol-function #'run-at-time)
               (lambda (_seconds _repeat function &rest _args)
                 (setf timer-fn function)
                 'fake-timer)))
      (unwind-protect
          (progn
            (leman-e2ee-tests--start-verification session "@vv:x.org" "flow1" "ABC")
            (leman-e2ee--prompt-sas session "ABC" (vector))
            (funcall timer-fn)
            (should (eq (plist-get leman-e2ee--active-verification :session) session))
            (should-not (alist-get 'cmd (leman-e2ee--decode (car (cdr (cdr fake)))))))
        (setf leman-e2ee--active-verification nil)))))

(ert-deftest leman-e2ee--prompt-sas-ignores-cleared-dance ()
  ;; The dance may finish (e.g. cancelled elsewhere) while the prompt
  ;; waits: answering it must then do nothing.
  (let* ((fake (leman-e2ee-tests--fake-agent nil))
         (session (make-leman-session :e2ee (car fake)))
         (timer-fn nil)
         (leman-e2ee-verify-confirm-function (lambda (&rest _) t)))
    (cl-letf (((symbol-function #'run-at-time)
               (lambda (_seconds _repeat function &rest _args)
                 (setf timer-fn function)
                 'fake-timer)))
      (unwind-protect
          (progn
            (leman-e2ee-tests--start-verification session "@vv:x.org" "flow1" "ABC")
            (leman-e2ee--prompt-sas session "ABC" (vector))
            (setf leman-e2ee--active-verification nil)
            (funcall timer-fn)
            (should (null (cdr (cdr fake)))))  ; nothing was sent to the agent
        (setf leman-e2ee--active-verification nil)))))

(ert-deftest leman-e2ee--advance-verification-finishes ()
  (let* ((fake (leman-e2ee-tests--fake-agent
                (list (cons 'verification_requests
                            (list (cons 'requests
                                        (vector (list (cons 'flow_id "flow1")
                                                      (cons 'state "done")
                                                      (cons 'sas t)))))))))
         (session (make-leman-session :e2ee (car fake)))
         (messages nil))
    (cl-letf (((symbol-function #'leman-message)
               (lambda (format &rest args)
                 (push (apply #'format format args) messages))))
      (unwind-protect
          (progn
            (leman-e2ee-tests--start-verification session "@vv:x.org" "flow1" "ABC")
            (leman-e2ee--advance-verification session (car fake))
            (should (null leman-e2ee--active-verification))
            (should (string-match-p "is verified" (car messages))))
        (setf leman-e2ee--active-verification nil)))))

(ert-deftest leman-e2ee--advance-verification-finishes-on-sas-done ()
  ;; A request may lag behind its SAS (both still in flight); a done
  ;; SAS means verified even if the request state hasn't caught up.
  (let* ((fake (leman-e2ee-tests--fake-agent
                (list (cons 'verification_requests
                            (list (cons 'requests
                                        (vector (list (cons 'flow_id "flow1")
                                                      (cons 'state "ready")
                                                      (cons 'sas t))))))
                      (cons 'verification_sas
                            (list (cons 'accepted t)
                                  (cons 'can_be_presented nil)
                                  (cons 'done t)
                                  (cons 'cancelled nil))))))
         (session (make-leman-session :e2ee (car fake))))
    (unwind-protect
        (progn
          (leman-e2ee-tests--start-verification session "@vv:x.org" "flow1" "ABC")
          (leman-e2ee--advance-verification session (car fake))
          (should (null leman-e2ee--active-verification)))
      (setf leman-e2ee--active-verification nil))))

(ert-deftest leman-e2ee--advance-verification-finishes-when-garbage-collected ()
  ;; After the dance completes, the state machine garbage-collects the
  ;; done request and SAS; the device's verified state is the
  ;; remaining signal.
  (let* ((fake (leman-e2ee-tests--fake-agent
                (list (cons 'verification_requests
                            (list (cons 'requests (vector))))
                      (cons 'devices
                            (list (cons 'devices
                                        (vector (list (cons 'device_id "ABC")
                                                      (cons 'verified t)))))))))
         (session (make-leman-session :e2ee (car fake))))
    (unwind-protect
        (progn
          (leman-e2ee-tests--start-verification session "@vv:x.org" "flow1" "ABC")
          (leman-e2ee--advance-verification session (car fake))
          (should (null leman-e2ee--active-verification)))
      (setf leman-e2ee--active-verification nil))))

(ert-deftest leman-e2ee--advance-verification-reports-expiry-when-gone-but-unverified ()
  (let* ((fake (leman-e2ee-tests--fake-agent
                (list (cons 'verification_requests
                            (list (cons 'requests (vector))))
                      (cons 'devices
                            (list (cons 'devices
                                        (vector (list (cons 'device_id "ABC")
                                                      (cons 'verified nil)))))))))
         (session (make-leman-session :e2ee (car fake)))
         (messages nil))
    (cl-letf (((symbol-function #'leman-message)
               (lambda (format &rest args)
                 (push (apply #'format format args) messages))))
      (unwind-protect
          (progn
            (leman-e2ee-tests--start-verification session "@vv:x.org" "flow1" "ABC")
            (leman-e2ee--advance-verification session (car fake))
            (should (null leman-e2ee--active-verification))
            (should (string-match-p "did not complete" (car messages))))
        (setf leman-e2ee--active-verification nil)))))

(ert-deftest leman-e2ee--advance-verification-ignores-other-sessions ()
  ;; Only the active dance's own session advances.
  (let* ((fake (leman-e2ee-tests--fake-agent nil))
         (session (make-leman-session :e2ee (car fake)))
         (other (make-leman-session :e2ee (car fake))))
    (unwind-protect
        (progn
          (leman-e2ee-tests--start-verification other "@vv:x.org" "flow1" "ABC")
          (leman-e2ee--advance-verification session (car fake))
          (should (null (cdr (cdr fake)))))  ; nothing was sent to the agent
      (setf leman-e2ee--active-verification nil))))

(ert-deftest leman-e2ee-verify-starts-dance-and-pumps-asynchronously ()
  "Verification must not synchronously wait for the homeserver or the agent."
  (let* ((agent (leman-e2ee--create))
         (session (make-leman-session
                   :e2ee agent
                   :user (make-leman-user :id "@me:x.org")))
         (async-pumps 0)
         dance)
    (cl-letf (((symbol-function #'read-string)
               (lambda (&rest _) "@me:x.org"))
              ((symbol-function #'completing-read)
               (lambda (&rest _) "ABC"))
              ((symbol-function #'leman-e2ee-devices)
               (lambda (&rest _)
                 (vector (list (cons 'device_id "ABC")
                               (cons 'display_name "Other device")
                               (cons 'verified nil)))))
              ((symbol-function #'leman-e2ee-verification-requests)
               (lambda (&rest _) (vector)))
              ((symbol-function #'leman-e2ee-request-verification)
               (lambda (&rest _) "flow1"))
              ((symbol-function #'leman-e2ee--process-outgoing-requests)
               (lambda (&rest _) (cl-incf async-pumps)))
              ((symbol-function #'leman-e2ee--process-outgoing-requests-sync)
               (lambda (&rest _)
                 (error "verification must not synchronously pump requests")))
              (leman-e2ee--active-verification nil))
      (unwind-protect
          (progn
            (leman-e2ee-verify session)
            (setf dance leman-e2ee--active-verification))
        (setf leman-e2ee--active-verification nil)))
    (should (= async-pumps 1))
    ;; The dance is registered and advances from the sync path.
    (should (eq (plist-get dance :session) session))
    (should (equal (plist-get dance :flow-id) "flow1"))
    (should (equal (plist-get dance :device-id) "ABC"))))

(ert-deftest leman-e2ee-verify-resumes-active-dance ()
  ;; Verifying again while a dance is in progress continues it (e.g.
  ;; to answer a deferred prompt) instead of starting a new one.
  (let* ((session (make-leman-session
                   :e2ee (leman-e2ee--create)
                   :user (make-leman-user :id "@me:x.org")))
         (advanced 0))
    (cl-letf (((symbol-function #'leman-e2ee--advance-verification)
               (lambda (&rest _) (cl-incf advanced)))
              (leman-e2ee--active-verification nil))
      (leman-e2ee-tests--start-verification session "@me:x.org" "flow1" "ABC")
      (leman-e2ee-verify session))
    (should (= advanced 1))
    (setf leman-e2ee--active-verification nil)))

(ert-deftest leman-e2ee--device-annotation-annotates ()
  ;; Device candidates are annotated with the display name (dimmed)
  ;; and a trust marker: green ~verified~, warning ~unverified~.  The
  ;; ID alone is hard to tell apart.
  (let* ((devices (vector (list (cons 'device_id "ABC")
                                (cons 'display_name "Element on phone")
                                (cons 'verified t))
                          (list (cons 'device_id "DEF")
                                (cons 'display_name "FluffyChat")
                                (cons 'verified nil))
                          (list (cons 'device_id "GHI"))))
         (result (funcall (leman-e2ee--device-annotation devices)
                          (list "ABC" "DEF" "GHI"))))
    (should (equal (nth 0 result) '("ABC" "" "  Element on phone  verified")))
    (should (equal (nth 1 result) '("DEF" "" "  FluffyChat  unverified")))
    ;; An unknown verified state counts as unverified (unnamed device).
    (should (equal (nth 2 result) '("GHI" "" "  unverified")))
    ;; The annotation text is dimmed; the markers are face-marked.
    (should (eq (get-text-property 20 'face (nth 2 (nth 0 result))) 'success))
    (should (eq (get-text-property 13 'face (nth 2 (nth 1 result))) 'warning))))

(ert-deftest leman-e2ee-verify-offers-live-devices-with-annotations ()
  ;; Deleted devices are not offered (there are no keys left to
  ;; verify), and the picker annotates the rest with their names.
  (let* ((agent (leman-e2ee--create))
         (session (make-leman-session
                   :e2ee agent
                   :user (make-leman-user :id "@me:x.org")))
         (collections nil) (affixations nil) (async-pumps 0))
    (cl-letf (((symbol-function #'read-string)
               (lambda (&rest _) "@me:x.org"))
              ((symbol-function #'completing-read)
               (lambda (_prompt collection &rest _)
                 (push collection collections)
                 (push (plist-get completion-extra-properties
                                  :affixation-function)
                       affixations)
                 "ABC"))
              ((symbol-function #'leman-e2ee-devices)
               (lambda (&rest _)
                 (vector (list (cons 'device_id "DEL")
                               (cons 'display_name "Old device")
                               (cons 'verified t)
                               (cons 'deleted t))
                         (list (cons 'device_id "ABC")
                               (cons 'display_name "Element on phone")
                               (cons 'verified nil)))))
              ((symbol-function #'leman-e2ee-verification-requests)
               (lambda (&rest _) (vector)))
              ((symbol-function #'leman-e2ee-request-verification)
               (lambda (&rest _) "flow1"))
              ((symbol-function #'leman-e2ee--process-outgoing-requests)
               (lambda (&rest _) (cl-incf async-pumps)))
              (leman-e2ee--active-verification nil))
      (unwind-protect (leman-e2ee-verify session)
        (setf leman-e2ee--active-verification nil)))
    ;; Only the live device is offered, with its annotation function.
    (should (equal (car collections) '("ABC")))
    (should (= 1 (length affixations)))
    (should (functionp (car affixations)))))

(ert-deftest leman-e2ee-verify-errors-without-live-devices ()
  (let* ((agent (leman-e2ee--create))
         (session (make-leman-session
                   :e2ee agent
                   :user (make-leman-user :id "@me:x.org"))))
    (cl-letf (((symbol-function #'read-string)
               (lambda (&rest _) "@me:x.org"))
              ((symbol-function #'leman-e2ee-devices)
               (lambda (&rest _)
                 (vector (list (cons 'device_id "DEL")
                               (cons 'display_name "Old device")
                               (cons 'deleted t)))))
              (leman-e2ee--active-verification nil))
      (should-error (leman-e2ee-verify session) :type 'user-error))))

(ert-deftest leman-e2ee-verify-hints-about-missing-cross-signing-keys ()
  ;; Devices verified by other clients only count once the agent holds
  ;; the private cross-signing keys: without them the trust view shows
  ;; everything unverified, and the user must be told why.
  (let* ((fake (leman-e2ee-tests--fake-agent
                (list (cons 'cross_signing_status
                            (list (cons 'has_master nil)
                                  (cons 'has_self_signing nil)
                                  (cons 'has_user_signing nil))))))
         (session (make-leman-session
                   :e2ee (car fake)
                   :user (make-leman-user :id "@me:x.org")))
         (messages nil))
    (cl-letf (((symbol-function #'leman-message)
               (lambda (format &rest args)
                 (push (apply #'format format args) messages)))
              ((symbol-function #'read-string)
               (lambda (&rest _) "@me:x.org"))
              ((symbol-function #'completing-read)
               (lambda (&rest _) "ABC"))
              ((symbol-function #'leman-e2ee-devices)
               (lambda (&rest _)
                 (vector (list (cons 'device_id "ABC")
                               (cons 'display_name "Element")
                               (cons 'verified nil)))))
              ((symbol-function #'leman-e2ee-verification-requests)
               (lambda (&rest _) (vector)))
              ((symbol-function #'leman-e2ee-request-verification)
               (lambda (&rest _) "flow1"))
              ((symbol-function #'leman-e2ee--process-outgoing-requests)
               (lambda (&rest _)))
              (leman-e2ee--active-verification nil))
      (unwind-protect (leman-e2ee-verify session)
        (setf leman-e2ee--active-verification nil)))
    (should (string-match-p "cross-signing" (car messages)))))

;;;; Incoming request announcements

(ert-deftest leman-e2ee--announce-requests-announces-new-incoming ()
  (let* ((fake (leman-e2ee-tests--fake-agent
                (list (cons 'verification_requests
                            (list (cons 'requests
                                        (vector (list (cons 'flow_id "flow1")
                                                      (cons 'user_id "@vv:x.org")
                                                      (cons 'device_id "ABC")
                                                      (cons 'state "created")
                                                      (cons 'we_started nil)))))))))
         (session (make-leman-session))
         (messages nil))
    (cl-letf (((symbol-function #'leman-message)
               (lambda (format &rest args)
                 (push (apply #'format format args) messages))))
      (leman-e2ee--announce-requests session (car fake))
      ;; A new incoming request is announced once...
      (should (= 1 (length messages)))
      (should (string-match-p "ABC" (car messages)))
      (should (string-match-p "leman-e2ee-verify" (car messages)))
      (should (equal (leman-session-e2ee-announced-requests session) '("flow1")))
      ;; ...and not announced again.
      (leman-e2ee--announce-requests session (car fake))
      (should (= 1 (length messages))))))

(ert-deftest leman-e2ee--announce-requests-ignores-outgoing-and-announced ()
  (let* ((fake (leman-e2ee-tests--fake-agent
                (list (cons 'verification_requests
                            (list (cons 'requests
                                        (vector (list (cons 'flow_id "outgoing")
                                                      (cons 'device_id "ABC")
                                                      (cons 'state "ready")
                                                      (cons 'we_started t))
                                                (list (cons 'flow_id "old")
                                                      (cons 'device_id "DEF")
                                                      (cons 'state "created")
                                                      (cons 'we_started nil)))))))))
         (session (make-leman-session :e2ee-announced-requests '("old")))
         (messages nil))
    (cl-letf (((symbol-function #'leman-message)
               (lambda (format &rest args)
                 (push (apply #'format format args) messages))))
      (leman-e2ee--announce-requests session (car fake))
      (should (null messages)))))

;;;; Session revocation

(ert-deftest leman--response-revoked-p ()
  (should (leman--response-revoked-p
           (make-plz-error :response
                           (make-plz-response :status 401
                                              :body "{\"errcode\":\"M_UNKNOWN_TOKEN\"}"))))
  (should-not (leman--response-revoked-p
               (make-plz-error :response (make-plz-response :status 200 :body "{}"))))
  (should-not (leman--response-revoked-p
               (make-plz-error :response
                               (make-plz-response :status 401
                                                  :body "{\"errcode\":\"M_LIMIT_EXCEEDED\"}"))))
  (should-not (leman--response-revoked-p
               (make-plz-error :curl-error '(7 . "connection refused")))))

(ert-deftest leman--session-revoked-runs-hook-once ()
  (let* ((session (make-leman-session :user (make-leman-user :id "@vv:x.org")))
         (calls 0)
         (soft-logout-seen 'unset)
         (warnings 0)
         leman-session-revoked-hook)
    (add-hook 'leman-session-revoked-hook
              (lambda (_session soft-logout)
                (cl-incf calls)
                (setf soft-logout-seen soft-logout)))
    (cl-letf (((symbol-function #'display-warning) (lambda (&rest _) (cl-incf warnings))))
      (leman--session-revoked session 'soft)
      (leman--session-revoked session nil))
    (should (= 1 calls))
    (should (eq soft-logout-seen 'soft))
    (should (= 1 warnings))
    (should (leman-session-revoked-p session))))

(ert-deftest leman-api-refuses-revoked-session ()
  (let* ((session (make-leman-session
                   :server (make-leman-server :name "x" :uri-prefix "https://x.org")))
         (leman--revoked-sessions (make-hash-table :weakness 'key :test #'eq)))
    (puthash session t leman--revoked-sessions)
    (cl-letf (((symbol-function #'plz)
               (lambda (&rest _) (error "plz must not be called"))))
      (should-error (leman-api session "sync")
                    :type 'leman-api-session-revoked))))

(ert-deftest leman--sync-failed-detects-revoked-token ()
  (let* ((session (make-leman-session :user (make-leman-user :id "@vv:x.org")))
         (calls 0)
         leman-session-revoked-hook)
    (add-hook 'leman-session-revoked-hook (lambda (_session _soft-logout) (cl-incf calls)))
    (cl-letf (((symbol-function #'leman--sync)
               (lambda (&rest _) (error "must not resync")))
              ((symbol-function #'display-warning) #'ignore))
      (should-error
       (leman--sync-failed
        session 30
        (make-plz-error :response
                        (make-plz-response :status 401
                                           :body "{\"errcode\":\"M_UNKNOWN_TOKEN\"}")))
       :type 'leman-api-error))
    (should (= 1 calls))
    (should (leman-session-revoked-p session))))

(ert-deftest leman-room--send-typing-stops-on-revoked-session ()
  (let* ((session (make-leman-session :user (make-leman-user :id "@vv:x.org")))
         (room (make-leman-room :id "!room:x.org"))
         (leman--revoked-sessions (make-hash-table :weakness 'key :test #'eq))
         (canceled 0)
         (leman-room-typing-timer (run-at-time 60 nil #'ignore)))
    (puthash session t leman--revoked-sessions)
    (cl-letf (((symbol-function #'leman-api)
               (lambda (&rest _) (error "leman-api must not be called")))
              ((symbol-function #'cancel-timer)
               (lambda (_timer) (cl-incf canceled))))
      (leman-room--send-typing session room))
    (should (= 1 canceled))
    (should (null leman-room-typing-timer))))

;;;; Store identity (per-device paths, hard-logout discard)

(ert-deftest leman-e2ee--store-path-per-device ()
  (let ((leman-e2ee-data-directory (make-temp-file "leman-store-test-" 'directory)))
    (let ((path (leman-e2ee--store-path "@vv:x.org" "ABC")))
      (should (string-match-p "crypto/_vv_x\\.org/ABC/\\'" path))
      (should (file-directory-p path))
      (should (= #o700 (file-modes path))))
    ;; A second device gets its own directory.
    (let ((path (leman-e2ee--store-path "@vv:x.org" "DEF")))
      (should-not (equal path
                         (leman-e2ee--store-path "@vv:x.org" "ABC"))))))

(ert-deftest leman-e2ee--discard-store-removes-device-store-and-legacy-files ()
  (let* ((leman-e2ee-data-directory (make-temp-file "leman-store-test-" 'dir))
         (device-dir (leman-e2ee--store-path "@vv:x.org" "ABC")))
    (write-region "db" nil (expand-file-name "matrix-sdk-crypto.sqlite3" device-dir))
    ;; Legacy layout files in the user directory.
    (write-region "old" nil
                  (expand-file-name "crypto/_vv_x.org/matrix-sdk-crypto.sqlite3"
                                    leman-e2ee-data-directory))
    (leman-e2ee--discard-store "@vv:x.org" "ABC")
    (should-not (file-directory-p device-dir))
    (should-not (file-exists-p
                 (expand-file-name "crypto/_vv_x.org/matrix-sdk-crypto.sqlite3"
                                   leman-e2ee-data-directory)))))

(ert-deftest leman-e2ee--discard-store-keeps-other-devices ()
  (let* ((leman-e2ee-data-directory (make-temp-file "leman-store-test-" 'dir))
         (abc (leman-e2ee--store-path "@vv:x.org" "ABC"))
         (def (leman-e2ee--store-path "@vv:x.org" "DEF")))
    (write-region "db" nil (expand-file-name "matrix-sdk-crypto.sqlite3" abc))
    (write-region "db" nil (expand-file-name "matrix-sdk-crypto.sqlite3" def))
    (leman-e2ee--discard-store "@vv:x.org" "ABC")
    (should-not (file-directory-p abc))
    (should (file-directory-p def))))

(ert-deftest leman--new-session-does-not-reclaim-existing-device ()
  ;; A fresh login must receive a new device ID even if an old store
  ;; remains.  Reclaiming it can make the homeserver remove its keys.
  (let* ((leman-e2ee-data-directory (make-temp-file "leman-store-test-" 'dir))
         (device-dir (leman-e2ee--store-path "@vv:x.org" "ABC")))
    (write-region "db" nil (expand-file-name "matrix-sdk-crypto.sqlite3" device-dir))
    (let ((session (leman--new-session "@vv:x.org" "https://x.org")))
      (should (null (leman-session-device-id session))))))

(ert-deftest leman--password-login-omits-device-id ()
  ;; An absent device ID must stay absent: an alist entry with a nil
  ;; cdr would encode as null instead of asking the server to mint one.
  (let (bodies)
    (cl-letf (((symbol-function #'leman-api)
               (lambda (_session _endpoint &rest args)
                 (push (plist-get args :data) bodies)))
              ((symbol-function #'read-passwd) (lambda (&rest _) "secret")))
      ;; Ignore even a stale in-memory ID: this login creates a new
      ;; device rather than reclaiming the old one.
      (leman--password-login
       (make-leman-session
        :user (make-leman-user :id "@vv:x.org")
        :initial-device-display-name "test"
        :device-id "ABC"))
      (let ((session (make-leman-session
                      :user (make-leman-user :id "@vv:x.org")
                      :initial-device-display-name "test")))
        (leman--password-login session)))
    (dolist (body bodies)
      (should (equal (json-read-from-string body)
                     '((type . "m.login.password")
                       (identifier . ((type . "m.id.user") (user . "@vv:x.org")))
                       (password . "secret")
                       (initial_device_display_name . "test")))))))

(ert-deftest leman--sso-login-omits-device-id ()
  "An SSO login also creates a new device."
  (let (body)
    (cl-letf (((symbol-function #'leman-api)
               (lambda (_session _endpoint &rest args)
                 (setf body (plist-get args :data)))))
      (leman--sso-login-with-token
       "token"
       (make-leman-session
        :user (make-leman-user :id "@vv:x.org")
        :initial-device-display-name "test"
        :device-id "ABC")))
    (should (equal (json-read-from-string body)
                   '((type . "m.login.token")
                     (identifier . ((type . "m.id.user") (user . "@vv:x.org")))
                     (token . "token")
                     (initial_device_display_name . "test"))))))

(ert-deftest leman--response-soft-logout-p ()
  (should (leman--response-soft-logout-p
           (make-plz-error :response
                           (make-plz-response :status 401
                                              :body "{\"errcode\":\"M_UNKNOWN_TOKEN\",\"soft_logout\":true}"))))
  (should-not (leman--response-soft-logout-p
               (make-plz-error :response
                               (make-plz-response :status 401
                                                  :body "{\"errcode\":\"M_UNKNOWN_TOKEN\"}")))))

(ert-deftest leman--session-revoked-cleanup-discards-store-on-hard-logout ()
  (let* ((leman-e2ee-data-directory (make-temp-file "leman-store-test-" 'dir))
         (session (make-leman-session
                   :user (make-leman-user :id "@vv:x.org")
                   :device-id "ABC"))
         (fake (leman-e2ee-tests--fake-agent nil))
         (leman--revoked-sessions (make-hash-table :weakness 'key :test #'eq))
         (device-dir (leman-e2ee--store-path "@vv:x.org" "ABC")))
    (setf (leman-session-e2ee session) (car fake))
    (write-region "db" nil (expand-file-name "matrix-sdk-crypto.sqlite3" device-dir))
    (cl-letf (((symbol-function #'display-warning) #'ignore)
              ((symbol-function #'leman--write-sessions) #'ignore)
              (leman-room-typing-timer nil)
              (leman-read-receipt-idle-timer nil)
              (leman-syncs (make-hash-table :test #'eq)))
      (leman--session-revoked-cleanup session)
      (should-not (file-directory-p device-dir)))
    (should (null (leman-session-e2ee session)))
    (should (null (leman-session-token session)))))

(ert-deftest leman--session-revoked-cleanup-keeps-store-on-soft-logout ()
  (let* ((leman-e2ee-data-directory (make-temp-file "leman-store-test-" 'dir))
         (session (make-leman-session
                   :user (make-leman-user :id "@vv:x.org")
                   :device-id "ABC"))
         (fake (leman-e2ee-tests--fake-agent nil))
         (leman--revoked-sessions (make-hash-table :weakness 'key :test #'eq))
         (device-dir (leman-e2ee--store-path "@vv:x.org" "ABC")))
    (setf (leman-session-e2ee session) (car fake))
    (write-region "db" nil (expand-file-name "matrix-sdk-crypto.sqlite3" device-dir))
    (cl-letf (((symbol-function #'display-warning) #'ignore)
              ((symbol-function #'leman--write-sessions) #'ignore)
              (leman-room-typing-timer nil)
              (leman-read-receipt-idle-timer nil)
              (leman-syncs (make-hash-table :test #'eq)))
      (leman--session-revoked-cleanup session 'soft)
      (should (file-directory-p device-dir)))
    (should (null (leman-session-e2ee session)))))

(ert-deftest leman--session-revoked-cleanup-removes-session-from-list ()
  ;; Reconnecting must prompt a fresh login, not resume the revoked
  ;; session.
  (let* ((session (make-leman-session :user (make-leman-user :id "@vv:x.org")))
         (other (make-leman-session :user (make-leman-user :id "@other:x.org")))
         (leman-sessions (list (cons "@vv:x.org" session)
                               (cons "@other:x.org" other)))
         (fake (leman-e2ee-tests--fake-agent nil))
         (leman--revoked-sessions (make-hash-table :weakness 'key :test #'eq)))
    (setf (leman-session-e2ee session) (car fake))
    (cl-letf (((symbol-function #'display-warning) #'ignore)
              ((symbol-function #'leman--write-sessions) #'ignore)
              (leman-room-typing-timer nil)
              (leman-read-receipt-idle-timer nil)
              (leman-syncs (make-hash-table :test #'eq))
              (leman-e2ee-data-directory (make-temp-file "leman-store-test-" 'dir)))
      (leman--session-revoked-cleanup session)
      (should (equal (mapcar #'cdr leman-sessions) (list other))))))

(ert-deftest leman-e2ee--agent-stale-p ()
  (let* ((root (make-temp-file "leman-agent-stale-" 'dir))
         (src (expand-file-name "e2ee/agent/src/lib.rs" root))
         (binary (expand-file-name "e2ee/agent/target/debug/leman-agent" root)))
    (make-directory (file-name-directory src) t)
    (make-directory (file-name-directory binary) t)
    (write-region "agent" nil src)
    (write-region "binary" nil binary)
    ;; Binary older than the source: stale.
    (set-file-times src (current-time))
    (set-file-times binary (time-subtract (current-time) 60))
    (should (leman-e2ee--agent-stale-p binary root))
    ;; Binary newer than the source: fine.
    (set-file-times binary (current-time))
    (set-file-times src (time-subtract (current-time) 60))
    (should-not (leman-e2ee--agent-stale-p binary root))
    ;; No source available (e.g. a PATH lookup): not stale.
    (should-not (leman-e2ee--agent-stale-p binary (make-temp-file "leman-none-" 'dir)))))

;;;; Key backup and SSSS

(ert-deftest leman-e2ee-backup-wrappers ()
  (pcase-let* ((fake (leman-e2ee-tests--fake-agent
                      `((backup_create . ((recovery_key . "AbC")
                                          (algorithm . "m.megolm_backup.v1.curve25519-aes-sha2")
                                          (auth_data . ((public_key . "pk")))))
                        (backup_status . ((enabled . t) (version . "1")))
                        (backup_room_keys . ((request . ((id . "t1")))))
                        (backup_verify . ((matches . t)))
                        (backup_import . ((imported . 2) (total . 2)))
                        (ssss_create . ((key_id . "k1") (recovery_key . "Rk")
                                        (content . ((algorithm . "m.secret_storage.v1.aes-hmac-sha2")))))
                        (ssss_encrypt_secret . ((iv . "iv") (ciphertext . "ct") (mac . "mac")))
                        (ssss_decrypt_secret . ((secret . "c2VjcmV0"))))))
                   (agent (car fake))
                   (sent (cdr fake)))
    ;; Creators return the full ok object.
    (should (equal (alist-get 'recovery_key (leman-e2ee-backup-create agent)) "AbC"))
    (should (equal (alist-get 'version (leman-e2ee-backup-status agent)) "1"))
    (should (equal (alist-get 'id (leman-e2ee-backup-room-keys agent)) "t1"))
    (should (equal (alist-get 'imported (leman-e2ee-backup-import agent "Rk" '((rooms)) t)) 2))
    (should (equal (alist-get 'key_id (leman-e2ee-ssss-create agent)) "k1"))
    (should (equal (alist-get 'ciphertext (leman-e2ee-ssss-encrypt-secret agent "k1" "Rk" nil "n" "c2M=")) "ct"))
    (should (equal (leman-e2ee-ssss-decrypt-secret agent "k1" "Rk" nil "n" "iv" "ct" "mac") "c2VjcmV0"))
    ;; Extractors return just the interesting field.
    (should (equal (leman-e2ee-backup-verify agent "AbC" nil) t))
    ;; Enabling and marking as sent pass their params.
    (leman-e2ee-backup-enable agent "AbC" "1")
    (leman-e2ee-backup-mark-as-sent agent "t1")
    ;; The params reach the agent.
    (let ((lines (mapcar (lambda (line) (leman-e2ee--decode line)) sent)))
      (should (equal (alist-get 'params (seq-find (lambda (r) (equal (alist-get 'cmd r) "backup_enable")) lines))
                     '((recovery_key . "AbC") (version . "1"))))
      (should (equal (alist-get 'params (seq-find (lambda (r) (equal (alist-get 'cmd r) "backup_mark_as_sent")) lines))
                     '((id . "t1"))))
      ;; Imports mark the keys as backed up unless told otherwise
      ;; (the sent lines are newest first).
      (should (eq (alist-get 'mark_backed_up
                             (alist-get 'params
                                        (seq-find (lambda (r) (equal (alist-get 'cmd r) "backup_import"))
                                                  lines)))
                  t))
      (leman-e2ee-backup-import agent "Rk" '((rooms)) nil)
      (let ((import (car (seq-filter (lambda (r) (equal (alist-get 'cmd r) "backup_import"))
                                     (mapcar (lambda (line) (leman-e2ee--decode line)) sent)))))
        (should (eq (alist-get 'mark_backed_up (alist-get 'params import)) nil))))))

(ert-deftest leman-e2ee-backup-room-keys-nil-when-nothing-to-do ()
  (pcase-let* ((fake (leman-e2ee-tests--fake-agent '((backup_room_keys . nil))))
               (agent (car fake)))
    (should-not (leman-e2ee-backup-room-keys agent))))

(ert-deftest leman-e2ee--backup-pump-drains-requests ()
  (let* ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql)))
         (requests (list '((id . "t1")
                           (path . "/_matrix/client/v3/room_keys/keys")
                           (params . ((version . "1")))
                           (body . "{\"rooms\":{}}"))
                         nil))
         (posts nil)
         (marked nil))
    (cl-letf (((symbol-function #'leman-e2ee-backup-room-keys)
               (lambda (_agent) (pop requests)))
              ((symbol-function #'leman-api)
               (lambda (_session endpoint &rest args)
                 (push (cons endpoint args) posts)
                 (funcall (plist-get args :then) '((etag . "x")))))
              ((symbol-function #'leman-e2ee-backup-mark-as-sent)
               (lambda (_agent id) (push id marked))))
      (leman-e2ee--backup-pump nil agent)
      (should (equal (mapcar #'car posts) (list "room_keys/keys")))
      (should (equal (plist-get (cdr (car posts)) :method) 'put))
      (should (equal (plist-get (cdr (car posts)) :params) '((version . "1"))))
      (should (equal (plist-get (cdr (car posts)) :data) "{\"rooms\":{}}"))
      (should (equal marked '("t1")))
      ;; Nothing more to back up: no second request was performed.
      (should (= (length posts) 1)))))

(ert-deftest leman-e2ee--backup-pump-survives-agent-errors ()
  ;; The pump must not break the sync loop when the agent errors (e.g.
  ;; no backup enabled yet).
  (let ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql)))
        (messages nil))
    (cl-letf (((symbol-function #'leman-e2ee-request)
               (lambda (&rest _args) (signal 'leman-e2ee-error (list "no backup"))))
              ((symbol-function #'leman-message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (leman-e2ee--backup-pump nil agent)
      (should messages))))

(ert-deftest leman-e2ee--backup-stale-version-p ()
  (should (leman-e2ee--backup-stale-version-p
           (make-plz-error :response
                           (make-plz-response :status 400
                                              :body "{\"errcode\":\"M_INVALID_PARAM\",\"error\":\"You may only manipulate the most recently created version of the backup.\"}"))))
  (should (leman-e2ee--backup-stale-version-p
           (make-plz-error :response
                           (make-plz-response :status 404
                                              :body "{\"errcode\":\"M_NOT_FOUND\"}"))))
  ;; A rejected upload for another reason (e.g. the old POST bug) is
  ;; not a stale-version rejection.
  (should-not (leman-e2ee--backup-stale-version-p
               (make-plz-error :response
                               (make-plz-response :status 405
                                                  :body "{\"errcode\":\"M_UNRECOGNIZED\"}"))))
  (should-not (leman-e2ee--backup-stale-version-p
               (make-plz-error :curl-error '(7 . "connection refused")))))

(ert-deftest leman-e2ee--decrypt-event-struct-stashes-raw ()
  (let* ((session (make-leman-session :user (make-leman-user :id "@me:x.org")))
         (raw '((type . "m.room.encrypted") (event_id . "$e1") (sender . "@a:x.org")
                (origin_server_ts . 1)
                (content . ((algorithm . "m.megolm.v1.aes-sha2"))))))
    ;; Decryption fails (no agent): the raw event is stashed on the
    ;; struct for later retries.
    (let ((struct (leman-e2ee--decrypt-event-struct session "!r:x.org" raw)))
      (should (equal (leman-event-type struct) "m.room.encrypted"))
      (should (equal (alist-get 'encrypted-raw (leman-event-local struct)) raw)))
    ;; Decryption succeeds: the struct is the decrypted event, with no
    ;; stash.
    (let ((session (make-leman-session
                    :user (make-leman-user :id "@me:x.org")
                    :e2ee (leman-e2ee--create :pending (make-hash-table :test #'eql)))))
      (cl-letf (((symbol-function #'leman-e2ee-decrypt-event)
                 (lambda (_agent _event)
                   '((type . "m.room.message") (event_id . "$e1") (sender . "@a:x.org")
                     (origin_server_ts . 1) (content . ((body . "hello")))))))
        (let ((struct (leman-e2ee--decrypt-event-struct session "!r:x.org" raw)))
          (should (equal (leman-event-type struct) "m.room.message"))
          (should (equal (alist-get 'body (leman-event-content struct)) "hello"))
          (should-not (alist-get 'encrypted-raw (leman-event-local struct))))))))

(ert-deftest leman-e2ee--retry-decryption-updates-stored-events ()
  (let* ((session (make-leman-session
                   :user (make-leman-user :id "@me:x.org")
                   :e2ee (leman-e2ee--create :pending (make-hash-table :test #'eql))))
         (raw '((type . "m.room.encrypted") (event_id . "$e1") (sender . "@a:x.org")
                (origin_server_ts . 1)
                (content . ((algorithm . "m.megolm.v1.aes-sha2") (ciphertext . "x")))))
         (undecrypted (leman-e2ee--decrypt-event-struct session "!r:x.org" raw))
         (room (make-leman-room :id "!r:x.org" :timeline (list undecrypted))))
    (setf (leman-session-rooms session) (list room))
    ;; Still no keys: nothing changes.
    (cl-letf (((symbol-function #'leman-e2ee--decrypt-event) (lambda (&rest _) raw)))
      (should (equal (leman-e2ee--retry-decryption session) '(0 . 1)))
      (should (equal (leman-event-type undecrypted) "m.room.encrypted")))
    ;; Keys arrived: the event is decrypted and updated in place.
    (cl-letf (((symbol-function #'leman-e2ee--decrypt-event)
               (lambda (&rest _)
                 '((type . "m.room.message") (event_id . "$e1") (sender . "@a:x.org")
                   (origin_server_ts . 1) (content . ((body . "hello")))))))
      (should (= (car (leman-e2ee--retry-decryption session)) 1))
      (should (equal (leman-event-type undecrypted) "m.room.message"))
      (should (equal (alist-get 'body (leman-event-content undecrypted)) "hello"))
      (should-not (alist-get 'encrypted-raw (leman-event-local undecrypted)))
      ;; Already-decrypted events are not retried.
      (should (equal (leman-e2ee--retry-decryption session) '(0 . 0))))))

(ert-deftest leman-e2ee--retry-decryption-notifies-recent-events ()
  ;; An event that only decrypted because its key arrived after it is
  ;; notified (its placeholder was already ignored); old events
  ;; decrypted by imported keys are not.
  (let* ((session (make-leman-session
                   :user (make-leman-user :id "@me:x.org")
                   :e2ee (leman-e2ee--create :pending (make-hash-table :test #'eql))))
         (now-ms (round (* 1000 (float-time))))
         (raw-recent `((type . "m.room.encrypted") (event_id . "$e1")
                       (sender . "@a:x.org") (origin_server_ts . ,now-ms)
                       (content . ((algorithm . "m.megolm.v1.aes-sha2")))))
         (raw-old `((type . "m.room.encrypted") (event_id . "$e2")
                    (sender . "@a:x.org") (origin_server_ts . 1)
                    (content . ((algorithm . "m.megolm.v1.aes-sha2")))))
         (undecrypted (leman-e2ee--decrypt-event-struct session "!r:x.org" raw-recent))
         (room (make-leman-room :id "!r:x.org" :timeline (list undecrypted)))
         (notifies 0))
    (setf (leman-session-rooms session) (list room))
    (cl-letf (((symbol-function #'leman-e2ee--decrypt-event)
               (lambda (_session raw _room-id)
                 (if (equal (alist-get 'event_id raw) "$e1")
                     `((type . "m.room.message") (event_id . "$e1") (sender . "@a:x.org")
                       (origin_server_ts . ,now-ms) (content . ((body . "hello"))))
                   '((type . "m.room.message") (event_id . "$e2") (sender . "@a:x.org")
                     (origin_server_ts . 1) (content . ((body . "old")))))))
              ((symbol-function #'leman-notify)
               (lambda (&rest _) (cl-incf notifies))))
      (leman-e2ee--retry-decryption session)
      (should (= notifies 1))
      ;; An old event (e.g. decrypted by importing keys) does not notify.
      (push (leman-e2ee--decrypt-event-struct session "!r:x.org" raw-old)
            (leman-room-timeline room))
      (leman-e2ee--retry-decryption session)
      (should (= notifies 1)))))

(ert-deftest leman-e2ee--sync-changes-flags-room-keys ()
  (let* ((session (make-leman-session
                   :user (make-leman-user :id "@me:x.org")
                   :e2ee (leman-e2ee--create :pending (make-hash-table :test #'eql)))))
    (cl-letf (((symbol-function #'leman-e2ee-receive-sync-changes)
               (lambda (&rest _)
                 '((to_device_events . [((type . "m.room.encrypted"))]))))
              (leman-e2ee--room-keys-arrived-p nil)
              ((symbol-function #'leman-e2ee--process-outgoing-requests) #'ignore)
              ((symbol-function #'leman-e2ee--announce-requests) #'ignore)
              ((symbol-function #'leman-e2ee--backup-pump) #'ignore))
      (leman-e2ee--sync-changes session '((next_batch . "s1")))
      (should-not leman-e2ee--room-keys-arrived-p)
      (cl-letf (((symbol-function #'leman-e2ee-receive-sync-changes)
                 (lambda (&rest _)
                   '((to_device_events . [((type . "m.room_key"))
                                          ((type . "m.forwarded_room_key"))])))))
        (leman-e2ee--sync-changes session '((next_batch . "s2")))
        (should leman-e2ee--room-keys-arrived-p)))))

(ert-deftest leman-e2ee--sync-changes-survives-dance-and-pump-errors ()
  ;; The sync path is the only place syncs restart from: a failed
  ;; dance step or a dead agent request must cost a message, not stop
  ;; syncing (announce and the backup pump still run).
  (let* ((session (make-leman-session
                   :user (make-leman-user :id "@me:x.org")
                   :e2ee (leman-e2ee--create :pending (make-hash-table :test #'eql))))
         (announced 0) (pumped 0) (messages nil))
    (cl-letf (((symbol-function #'leman-e2ee-receive-sync-changes) #'ignore)
              ((symbol-function #'leman-e2ee--advance-verification)
               (lambda (&rest _) (signal 'leman-e2ee-error '("timeout" "no response"))))
              ((symbol-function #'leman-e2ee--process-outgoing-requests)
               (lambda (&rest _) (signal 'leman-e2ee-error '("timeout" "no response"))))
              ((symbol-function #'leman-e2ee--announce-requests)
               (lambda (&rest _) (cl-incf announced)))
              ((symbol-function #'leman-e2ee--backup-pump)
               (lambda (&rest _) (cl-incf pumped)))
              ((symbol-function #'leman-message)
               (lambda (format &rest args)
                 (push (apply #'format format args) messages))))
      (leman-e2ee--sync-changes session '((next_batch . "s1"))))
    (should (= announced 1))
    (should (= pumped 1))
    (should (= 2 (length messages)))
    ;; The advance error was demoted first, then the pump's.
    (should (string-match-p "advancing verification failed" (cadr messages)))
    (should (string-match-p "performing outgoing requests failed" (car messages)))))

(ert-deftest leman--sync-callback-keeps-syncing-through-hook-error ()
  ;; The sync callback is the only place the next sync is started: an
  ;; error in a hook must not stop the chain.
  (let* ((session (make-leman-session :user (make-leman-user :id "@me:x.org")))
         (syncs 0)
         (messages nil))
    (cl-letf (((symbol-function #'leman-e2ee--sync-changes) #'ignore)
              ((symbol-function #'leman--sync)
               (lambda (&rest _) (cl-incf syncs)))
              ((symbol-function #'message)
               (lambda (format &rest args)
                 (push (apply #'format format args) messages)))
              (leman-sync-callback-hook
               (list (lambda (_session) (error "hook bug"))))
              (leman-auto-sync t)
              (leman-syncs nil))
      (leman--sync-callback session '((next_batch . "s2"))))
    (should (= syncs 1))
    (should (equal (leman-session-next-batch session) "s2"))
    (should (string-match-p "hook bug" (car messages)))))

(ert-deftest leman--sync-callback-does-not-double-sync ()
  ;; When a hook (e.g. `leman--auto-sync') already started the next
  ;; sync, the chain-keeping tail must not start another one.
  (let* ((session (make-leman-session :user (make-leman-user :id "@me:x.org")))
         (syncs 0))
    (cl-letf (((symbol-function #'leman-e2ee--sync-changes) #'ignore)
              ((symbol-function #'leman--sync)
               (lambda (&rest _)
                 (cl-incf syncs)
                 ;; The real sync registers itself; mimic that.
                 (setf (map-elt leman-syncs session) 'fake-process)))
              (leman-sync-callback-hook (list #'leman--auto-sync))
              (leman-auto-sync t)
              (leman-syncs nil))
      (leman--sync-callback session '((next_batch . "s2"))))
    (should (= syncs 1))))

(ert-deftest leman-e2ee--backup-pump-warns-once-when-stale ()
  (let* ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql)))
         (messages nil)
         (stale (make-plz-error :response
                                (make-plz-response :status 400
                                                   :body "{\"errcode\":\"M_INVALID_PARAM\"}")))
         (request '((id . "t1")
                    (path . "/_matrix/client/v3/room_keys/keys")
                    (params . ((version . "1")))
                    (body . "{\"rooms\":{}}")))
         ;; Fresh global state (other tests may have warned already).
         (leman-e2ee--backup-stale-warned-p nil))
    (cl-letf (((symbol-function #'leman-message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      ;; Two stale rejections: only one warning.
      (let ((requests (list request nil)))
        (cl-letf (((symbol-function #'leman-e2ee-backup-room-keys)
                   (lambda (_agent) (pop requests)))
                  ((symbol-function #'leman-api)
                   (lambda (_session _endpoint &rest args)
                     (funcall (plist-get args :else) stale))))
          (leman-e2ee--backup-pump nil agent)
          (leman-e2ee--backup-pump nil agent)))
      (should (= (length messages) 1))
      (should (string-prefix-p "Leman E2EE: the server's key backup changed" (car messages)))
      ;; A successful upload clears the flag: the next rejection warns again.
      (let ((requests (list request)))
        (cl-letf (((symbol-function #'leman-e2ee-backup-room-keys)
                   (lambda (_agent) (pop requests)))
                  ((symbol-function #'leman-e2ee-backup-mark-as-sent) #'ignore)
                  ((symbol-function #'leman-api)
                   (lambda (_session _endpoint &rest args)
                     (funcall (plist-get args :then) nil))))
          (leman-e2ee--backup-pump nil agent)))
      (let ((requests (list request)))
        (cl-letf (((symbol-function #'leman-e2ee-backup-room-keys)
                   (lambda (_agent) (pop requests)))
                  ((symbol-function #'leman-api)
                   (lambda (_session _endpoint &rest args)
                     (funcall (plist-get args :else) stale))))
          (leman-e2ee--backup-pump nil agent)))
      (should (= (length messages) 2)))))

(ert-deftest leman-e2ee-ssss-check-key ()
  (pcase-let* ((fake (leman-e2ee-tests--fake-agent
                      '((ssss_check_key . ((valid . t))))))
               (agent (car fake)))
    (should (leman-e2ee-ssss-check-key agent "k1" "EsGood" nil)))
  (pcase-let* ((fake (leman-e2ee-tests--fake-agent
                      '((ssss_check_key . (err "crypto" "The MAC check failed")))))
               (agent (car fake)))
    (should-not (leman-e2ee-ssss-check-key agent "k1" "EsBad" nil))))

(ert-deftest leman-e2ee--unlock-backup-secret-tries-default-key-first ()
  (let* ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql)))
         (attempts nil)
         (checks nil)
         (verifies nil)
         (account-data `(("m.secret_storage.key.k1" . ((algorithm . "x")))
                         ("m.secret_storage.key.k2" . ((algorithm . "y"))))))
    (cl-letf (((symbol-function #'leman-e2ee--backup-version-info)
               (lambda (_session)
                 '((version . "9")
                   (algorithm . "m.megolm_backup.v1.curve25519-aes-sha2")
                   (auth_data . ((public_key . "pk9"))))))
              ((symbol-function #'leman-e2ee--account-data-get)
               (lambda (_session type)
                 (alist-get type account-data nil nil #'equal)))
              ((symbol-function #'leman-e2ee-ssss-check-key)
               (lambda (_agent key-id _recovery _content)
                 (push key-id checks)
                 ;; Only k2's recovery key matches the entered one.
                 (equal key-id "k2")))
              ((symbol-function #'leman-e2ee-ssss-decrypt-secret)
               (lambda (_agent key-id _recovery _content _name _iv _ct _mac)
                 (push key-id attempts)
                 ;; Leman's old (pre-interop) encoding wraps the
                 ;; base58 key in base64; the unlock accepts it.
                 (base64-encode-string "EsBACKUP" t)))
              ((symbol-function #'leman-e2ee-backup-verify)
               (lambda (_agent recovery _backup-info)
                 (push recovery verifies)
                 ;; Only the real key matches the current version.
                 (equal recovery "EsBACKUP"))))
      ;; The default key (k1) is tried first (its entry fails to
      ;; unlock), then k2's entry unlocks.
      (let ((result (leman-e2ee--unlock-backup-secret
                     nil agent "EsK2"
                     '((k1 . ((iv . "i") (ciphertext . "c") (mac . "m")))
                       (k2 . ((iv . "i") (ciphertext . "c") (mac . "m"))))
                     "k1")))
        (should (equal result "EsBACKUP"))
        (should (equal checks '("k2" "k1")))
        (should (equal attempts '("k2")))
        ;; Both encodings were tried: the base64-wrapped value failed
        ;; to verify, then the decoded key matched.
        (should (equal verifies
                        (list "EsBACKUP"
                              (base64-encode-string "EsBACKUP" t))))))))

(ert-deftest leman-e2ee--unlock-backup-secret-skips-stale-entries ()
  ;; An entry that unlocks but holds an older version's key is
  ;; skipped (Element may have created a newer backup version since
  ;; the entry was written); the error names it.
  (let* ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql)))
         (account-data `(("m.secret_storage.key.k1" . ((algorithm . "x"))))))
    (cl-letf (((symbol-function #'leman-e2ee--backup-version-info)
               (lambda (_session)
                 '((version . "9")
                   (algorithm . "m.megolm_backup.v1.curve25519-aes-sha2")
                   (auth_data . ((public_key . "pk9"))))))
              ((symbol-function #'leman-e2ee--account-data-get)
               (lambda (_session type)
                 (alist-get type account-data nil nil #'equal)))
              ((symbol-function #'leman-e2ee-ssss-check-key)
               (lambda (_agent _key-id _recovery _content) t))
              ((symbol-function #'leman-e2ee-ssss-decrypt-secret)
                (lambda (&rest _) "EsSTALE"))
               ((symbol-function #'leman-e2ee-backup-verify)
                (lambda (_agent _recovery _backup-info) nil)))
      (let ((err (should-error
                  (leman-e2ee--unlock-backup-secret
                   nil agent "EsOld"
                   '((k1 . ((iv . "i") (ciphertext . "c") (mac . "m"))))
                   "k1")
                  :type 'user-error)))
        (should (string-search "another backup version" (cadr err)))
        (should (string-search "k1" (cadr err)))))))

(ert-deftest leman-e2ee--unlock-backup-secret-reports-failures ()
  (let* ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql)))
         (account-data `(("m.secret_storage.key.k1" . ((algorithm . "x"))))))
    (cl-letf (((symbol-function #'leman-e2ee--backup-version-info)
               (lambda (_session)
                 '((version . "9")
                   (algorithm . "m.megolm_backup.v1.curve25519-aes-sha2")
                   (auth_data . ((public_key . "pk9"))))))
              ((symbol-function #'leman-e2ee--account-data-get)
               (lambda (_session type)
                 (alist-get type account-data nil nil #'equal)))
              ((symbol-function #'leman-e2ee-ssss-check-key)
               (lambda (_agent _key-id _recovery _content) nil)))
      (let ((err (should-error
                  (leman-e2ee--unlock-backup-secret
                   nil agent "EsWrong" '((k1 . ((iv . "i") (ciphertext . "c") (mac . "m"))))
                   "k1")
                  :type 'user-error)))
        (should (string-search "does not unlock" (cadr err)))
        (should (string-search "k1" (cadr err)))))))

(ert-deftest leman-e2ee--re-store-backup-secret-adds-default-entry ()
  (let* ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql)))
         (puts nil)
         ;; Symbol keys: the shape `json-read' returns.
         (account-data `(("m.secret_storage.default_key" . ((key . "kd")))
                         ("m.secret_storage.secret.m.megolm_backup.v1"
                          . ((encrypted . ((k1 . ((iv . "i")))))))
                         ("m.secret_storage.key.kd" . ((algorithm . "x"))))))
    (cl-letf (((symbol-function #'leman-e2ee--account-data-get)
               (lambda (_session type)
                 (alist-get type account-data nil nil #'equal)))
              ((symbol-function #'leman-e2ee--account-data-put)
               (lambda (_session type data) (push (cons type data) puts)))
              ((symbol-function #'leman-e2ee-ssss-check-key)
               (lambda (_agent _key-id _recovery _content) t))
              ((symbol-function #'leman-e2ee-ssss-encrypt-secret)
               (lambda (_agent _key-id _recovery _content _name _secret)
                 '((iv . "i2") (ciphertext . "c2") (mac . "m2")))))
      (leman-e2ee--re-store-backup-secret nil agent "EsK" "EsBACKUP")
      (should (= (length puts) 1))
      (let* ((put (car puts))
             (entries (alist-get 'encrypted (cdr put))))
        (should (equal (car put) "m.secret_storage.secret.m.megolm_backup.v1"))
        ;; Both entries are kept: the old one and the new default one.
        (should (alist-get "k1" entries nil nil #'equal))
        (should (equal (alist-get "kd" entries nil nil #'equal)
                       '((iv . "i2") (ciphertext . "c2") (mac . "m2"))))))))

(ert-deftest leman-e2ee--re-store-backup-secret-skips-existing-entry ()
  ;; The entry under the default key already holds the current
  ;; backup version's key: nothing is written.
  (let* ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql)))
         (puts nil)
         (account-data `(("m.secret_storage.default_key" . ((key . "kd")))
                         ("m.secret_storage.secret.m.megolm_backup.v1"
                          . ((encrypted . ((kd . ((iv . "i") (ciphertext . "c")
                                                  (mac . "m")))))))
                         ("m.secret_storage.key.kd" . ((algorithm . "x"))))))
    (cl-letf (((symbol-function #'leman-e2ee--account-data-get)
               (lambda (_session type)
                 (alist-get type account-data nil nil #'equal)))
              ((symbol-function #'leman-e2ee--account-data-put)
               (lambda (_session type _data) (push type puts)))
              ((symbol-function #'leman-e2ee-ssss-check-key)
               (lambda (_agent _key-id _recovery _content) t))
              ((symbol-function #'leman-e2ee--backup-version-info)
               (lambda (_session) '((version . "12148797") (algorithm . "alg")
                                    (auth_data . "auth"))))
              ((symbol-function #'leman-e2ee-ssss-decrypt-secret)
               (lambda (&rest _args) "EsCURRENT"))
              ((symbol-function #'leman-e2ee-backup-verify)
               (lambda (_agent _key _info) t)))
      (leman-e2ee--re-store-backup-secret nil agent "EsK" "EsOTHERBACKUP")
      (should-not puts))))

(ert-deftest leman-e2ee--re-store-backup-secret-asks-for-default-recovery ()
  ;; The entered recovery key does not unlock the default key: the
  ;; ask-path supplies the default key's recovery key instead.
  (let* ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql)))
         (puts nil)
         (prompts nil)
         (account-data `(("m.secret_storage.default_key" . ((key . "kd")))
                         ("m.secret_storage.secret.m.megolm_backup.v1"
                          . ((encrypted . ((k1 . ((iv . "i")))))))
                         ("m.secret_storage.key.kd" . ((algorithm . "x"))))))
    (cl-letf (((symbol-function #'leman-e2ee--account-data-get)
               (lambda (_session type)
                 (alist-get type account-data nil nil #'equal)))
              ((symbol-function #'leman-e2ee--account-data-put)
               (lambda (_session type data) (push (cons type data) puts)))
              ((symbol-function #'leman-e2ee-ssss-check-key)
               (lambda (_agent _key-id recovery _content)
                 (equal recovery "EsDef")))
              ((symbol-function #'leman-e2ee-ssss-encrypt-secret)
               (lambda (_agent _key-id _recovery _content _name _secret)
                 '((iv . "i2") (ciphertext . "c2") (mac . "m2"))))
              ((symbol-function #'read-string)
               (lambda (prompt &rest _rest)
                 (push prompt prompts) "EsDef")))
      (leman-e2ee--re-store-backup-secret nil agent "EsOther" "EsBACKUP")
      (should (= (length puts) 1))
      (should (string-search "kd" (car prompts)))
      (let ((entries (alist-get 'encrypted (cdr (car puts)))))
        (should (alist-get "k1" entries nil nil #'equal))
        (should (alist-get "kd" entries nil nil #'equal))))))

(ert-deftest leman-e2ee--re-store-backup-secret-empty-answer-skips ()
  (let* ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql)))
         (puts nil)
         (account-data `(("m.secret_storage.default_key" . ((key . "kd")))
                         ("m.secret_storage.secret.m.megolm_backup.v1"
                          . ((encrypted . ((k1 . ((iv . "i")))))))
                         ("m.secret_storage.key.kd" . ((algorithm . "x"))))))
    (cl-letf (((symbol-function #'leman-e2ee--account-data-get)
               (lambda (_session type)
                 (alist-get type account-data nil nil #'equal)))
              ((symbol-function #'leman-e2ee--account-data-put)
               (lambda (_session type _data) (push type puts)))
              ((symbol-function #'leman-e2ee-ssss-check-key)
               (lambda (_agent _key-id _recovery _content) nil))
              ((symbol-function #'read-string)
               (lambda (&rest _args) "")))
      (leman-e2ee--re-store-backup-secret nil agent "EsOther" "EsBACKUP")
      (should-not puts))))

(ert-deftest leman-e2ee--re-store-backup-secret-replaces-stale-entry ()
  ;; An entry under the default key that holds another backup
  ;; version's key is replaced, so other clients can unlock the
  ;; current backup again.
  (let* ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql)))
         (puts nil)
         (account-data `(("m.secret_storage.default_key" . ((key . "kd")))
                         ("m.secret_storage.secret.m.megolm_backup.v1"
                          . ((encrypted . ((kd . ((iv . "old") (ciphertext . "co") (mac . "mo")))
                                           (k1 . ((iv . "i")))))))
                         ("m.secret_storage.key.kd" . ((algorithm . "x"))))))
    (cl-letf (((symbol-function #'leman-e2ee--account-data-get)
               (lambda (_session type)
                 (alist-get type account-data nil nil #'equal)))
              ((symbol-function #'leman-e2ee--account-data-put)
               (lambda (_session type data) (push (cons type data) puts)))
              ((symbol-function #'leman-e2ee-ssss-check-key)
               (lambda (_agent _key-id _recovery _content) t))
              ((symbol-function #'leman-e2ee--backup-version-info)
               (lambda (_session) '((version . "12148797") (algorithm . "alg") (auth_data . "auth"))))
              ((symbol-function #'leman-e2ee-ssss-decrypt-secret)
               (lambda (&rest _args) "EsOLD"))
              ((symbol-function #'leman-e2ee-backup-verify)
               (lambda (_agent _key _info) nil))
              ((symbol-function #'leman-e2ee-ssss-encrypt-secret)
               (lambda (_agent _key-id _recovery _content _name _secret)
                 '((iv . "new") (ciphertext . "c2") (mac . "m2")))))
      (leman-e2ee--re-store-backup-secret nil agent "EsK" "EsNEWBACKUP")
      (should (= (length puts) 1))
      (let* ((entries (alist-get 'encrypted (cdr (car puts)))))
        (should (= (length entries) 2))
        (should (equal (alist-get "kd" entries nil nil #'equal)
                       '((iv . "new") (ciphertext . "c2") (mac . "m2"))))
        (should (equal (alist-get "k1" entries nil nil #'equal) '((iv . "i"))))))))

(ert-deftest leman-e2ee--re-store-backup-secret-keeps-current-entry ()
  ;; An entry under the default key holding the current backup
  ;; version's key is left alone.
  (let* ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql)))
         (puts nil)
         (account-data `(("m.secret_storage.default_key" . ((key . "kd")))
                         ("m.secret_storage.secret.m.megolm_backup.v1"
                          . ((encrypted . ((kd . ((iv . "i") (ciphertext . "c") (mac . "m")))))))
                         ("m.secret_storage.key.kd" . ((algorithm . "x"))))))
    (cl-letf (((symbol-function #'leman-e2ee--account-data-get)
               (lambda (_session type)
                 (alist-get type account-data nil nil #'equal)))
              ((symbol-function #'leman-e2ee--account-data-put)
               (lambda (_session type _data) (push type puts)))
              ((symbol-function #'leman-e2ee-ssss-check-key)
               (lambda (_agent _key-id _recovery _content) t))
              ((symbol-function #'leman-e2ee--backup-version-info)
               (lambda (_session) '((version . "12148797") (algorithm . "alg") (auth_data . "auth"))))
              ((symbol-function #'leman-e2ee-ssss-decrypt-secret)
               (lambda (&rest _args) "EsCURRENT"))
              ((symbol-function #'leman-e2ee-backup-verify)
               (lambda (_agent _key _info) t)))
      (leman-e2ee--re-store-backup-secret nil agent "EsK" "EsOTHERBACKUP")
      (should-not puts))))

(ert-deftest leman-e2ee-cross-signing-wrappers ()
  (pcase-let* ((fake (leman-e2ee-tests--fake-agent
                      '((cross_signing_status . ((has_master . t)
                                                 (has_self_signing . t)
                                                 (has_user_signing)))))
                (agent (car fake)))
    (should (equal (leman-e2ee-cross-signing-status agent)
                   '((has_master . t) (has_self_signing . t) (has_user_signing))))
    (leman-e2ee-import-cross-signing agent "MK" "SSK" nil)
    (let ((sent (mapcar #'leman-e2ee--decode (cdr fake))))
      (should (equal (alist-get 'params
                                (seq-find (lambda (r)
                                            (equal (alist-get 'cmd r)
                                                   "import_cross_signing_keys"))
                                          sent))
     '((master_key . "MK")
       (self_signing_key . "SSK")
       (user_signing_key))))))))

(ert-deftest leman-e2ee--decrypt-ssss-secret ()
  (let* ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql)))
         (account-data `(("m.secret_storage.key.kd" . ((algorithm . "x")))
                         ("m.secret_storage.secret.m.cross_signing.self_signing"
                          . ((encrypted . ((kd . ((iv . "i") (ciphertext . "c") (mac . "m")))
                                           (other . ((iv . "x"))))))))))
    (cl-letf (((symbol-function #'leman-e2ee--account-data-get)
               (lambda (_session type)
                 (alist-get type account-data nil nil #'equal)))
              ((symbol-function #'leman-e2ee-ssss-check-key)
               (lambda (_agent _key-id recovery _content) (equal recovery "EsK")))
              ((symbol-function #'leman-e2ee-ssss-decrypt-secret)
               (lambda (&rest _args) "T0hBc2VlZA")))
      ;; Symbol keys (the json-read shape) are matched.
      (should (equal (leman-e2ee--decrypt-ssss-secret
                      nil agent "kd" "EsK" "m.cross_signing.self_signing")
                     "T0hBc2VlZA"))
      ;; The value is returned verbatim (no base64-decoding): the
      ;; m.cross_signing secrets hold the unpadded base64 seeds.
      (should-not (equal (leman-e2ee--decrypt-ssss-secret
                          nil agent "kd" "EsK" "m.cross_signing.self_signing")
                         "OhAseed"))
      ;; A wrong recovery key or an unknown secret: nil.
      (should-not (leman-e2ee--decrypt-ssss-secret
                   nil agent "kd" "EsOther" "m.cross_signing.self_signing"))
      (should-not (leman-e2ee--decrypt-ssss-secret
                   nil agent "kd" "EsK" "m.cross_signing.master")))))

(ert-deftest leman-e2ee--import-cross-signing ()
  (let* ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql)))
         (imports nil)
         (account-data `(("m.secret_storage.default_key" . ((key . "kd")))
                         ("m.secret_storage.key.kd" . ((algorithm . "x")))
                         ("m.secret_storage.secret.m.cross_signing.master"
                          . ((encrypted . ((kd . ((iv . "i") (ciphertext . "c") (mac . "m")))))))
                         ("m.secret_storage.secret.m.cross_signing.self_signing"
                          . ((encrypted . ((kd . ((iv . "i") (ciphertext . "c") (mac . "m"))))))))))
    (cl-letf (((symbol-function #'leman-e2ee--account-data-get)
               (lambda (_session type)
                 (alist-get type account-data nil nil #'equal)))
              ((symbol-function #'leman-e2ee-ssss-check-key)
               (lambda (_agent _key-id _recovery _content) t))
              ((symbol-function #'leman-e2ee-ssss-decrypt-secret)
               (lambda (_agent _key-id _recovery _content name &rest _)
                 (pcase name
                   ("m.cross_signing.master" "TWFpblNlZWQ")
                   ("m.cross_signing.self_signing" "U2VsZlNlZWQ"))))
              ((symbol-function #'leman-e2ee-import-cross-signing)
               (lambda (_agent master self-signing user-signing)
                 (push (list master self-signing user-signing) imports)
                 '((has_master . t) (has_self_signing . t) (has_user_signing))))
              ((symbol-function #'leman-message) #'ignore))
      ;; The user-signing secret is not stored on the account; the
      ;; others are imported.
      (should (leman-e2ee--import-cross-signing nil agent "EsK"))
      (should (equal imports '(("TWFpblNlZWQ" "U2VsZlNlZWQ" nil))))
      ;; When no secret unlocks, nothing is imported.
      (cl-letf (((symbol-function #'leman-e2ee-ssss-check-key)
                 (lambda (_agent _key-id _recovery _content) nil)))
        (should-not (leman-e2ee--import-cross-signing nil agent "EsK"))
        (should (= (length imports) 1))))))

(ert-deftest leman-e2ee--account-data-put-signals-on-failure ()
  ;; With `:else' nil, `plz' returns the error from a sync request
  ;; instead of signaling it: the wrapper must not drop it.
  (let ((session (make-leman-session :user (make-leman-user :id "@me:x.org"))))
    (cl-letf (((symbol-function #'leman-api)
               (lambda (&rest _) (make-plz-error :message "boom"))))
      (should-error (leman-e2ee--account-data-put session "m.test" nil)
                    :type 'plz-error))
    (cl-letf (((symbol-function #'leman-api)
               (lambda (&rest _) '((content . t)))))
      (should-not (leman-e2ee--account-data-put session "m.test" nil)))))

(ert-deftest leman-e2ee--create-backup-version-replaces-stale-entry ()
  ;; The new backup key replaces the (stale) entry under the default
  ;; secret-storage key instead of being appended as a duplicate.
  (let* ((fake (leman-e2ee-tests--fake-agent
                '((backup_create . ((recovery_key . "EsNew")
                                    (algorithm . "m.megolm_backup.v1.curve25519-aes-sha2")
                                    (auth_data . "auth"))))))
         (agent (car fake))
         (session (make-leman-session :user (make-leman-user :id "@me:x.org")
                                      :e2ee agent))
         (puts nil)
         (pumps 0)
         (account-data `(("m.secret_storage.key.kd" . ((algorithm . "x")))
                         ("m.secret_storage.secret.m.megolm_backup.v1"
                          ;; Symbol keys: the shape `json-read' returns.
                          . ((encrypted . ((kd . ((iv . "stale")))
                                           (k1 . ((iv . "i"))))))))))
    (cl-letf (((symbol-function #'leman-e2ee--backup-version-info)
               (lambda (_session) '((version . "12148797"))))
              ((symbol-function #'leman-api)
               (lambda (_session endpoint &rest args)
                 (if (eq (plist-get args :method) 'post)
                     '((version . "12148797"))
                   (user-error "unexpected endpoint %s" endpoint))))
              ((symbol-function #'leman-e2ee--account-data-get)
               (lambda (_session type)
                 (alist-get type account-data nil nil #'equal)))
              ((symbol-function #'leman-e2ee--account-data-put)
               (lambda (_session type data) (push (cons type data) puts)))
              ((symbol-function #'leman-e2ee-ssss-encrypt-secret)
               (lambda (_agent _key-id _recovery _content _name _secret)
                 '((iv . "new") (ciphertext . "c") (mac . "m"))))
              ((symbol-function #'leman-e2ee--backup-pump)
               (lambda (_session _agent) (cl-incf pumps)))
              ((symbol-function #'leman-message) #'ignore))
      (leman-e2ee--create-backup-version session agent "kd" "EsDefault")
      (should (= (length puts) 1))
      (let* ((put (car puts))
             (entries (alist-get 'encrypted (cdr put))))
        (should (equal (car put) "m.secret_storage.secret.m.megolm_backup.v1"))
        ;; One entry per key: the stale one was replaced, not duplicated.
        (should (= (length entries) 2))
        (should (equal (alist-get "kd" entries nil nil #'equal)
                       '((iv . "new") (ciphertext . "c") (mac . "m"))))
        (should (equal (alist-get "k1" entries nil nil #'equal) '((iv . "i")))))
      (should (= pumps 1)))))

(ert-deftest leman-e2ee--create-backup-version-verifies-current ()
  ;; Some homeservers return the new version id while keeping an old
  ;; version current: creating must not proceed while the server
  ;; refuses to make the new version current.
  (let* ((fake (leman-e2ee-tests--fake-agent
                '((backup_create . ((recovery_key . "EsNew")
                                    (algorithm . "m.megolm_backup.v1.curve25519-aes-sha2")
                                    (auth_data . "auth"))))))
         (agent (car fake))
         (session (make-leman-session :user (make-leman-user :id "@me:x.org")
                                      :e2ee agent))
         (enables 0))
    (cl-letf (((symbol-function #'leman-e2ee--backup-version-info)
               ;; The server keeps the old version current.
               (lambda (_session) '((version . "69562"))))
              ((symbol-function #'leman-e2ee--api-sync)
               (lambda (&rest _) '((version . "12150227"))))
              ((symbol-function #'leman-e2ee-backup-enable)
               (lambda (&rest _) (cl-incf enables)))
              ((symbol-function #'y-or-n-p) (lambda (&rest _) nil))
              ((symbol-function #'leman-message) #'ignore))
      (should-error (leman-e2ee--create-backup-version session agent "kd" "EsDefault")
                    :type 'user-error)
      (should (= enables 0)))))

(ert-deftest leman-e2ee-restore-old-backup ()
  ;; The chosen older version's key is unlocked from the secret
  ;; entry of an older secret-storage key; its room keys are
  ;; imported WITHOUT being marked as backed up (the pump re-uploads
  ;; them to the current version).
  (let* ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql)))
         (session (make-leman-session :user (make-leman-user :id "@vv:x.org")))
         (imports nil)
         (pumps 0)
         (retries nil)
         (account-data `(("m.secret_storage.key.old" . ((algorithm . "x")))
                         ("m.secret_storage.secret.m.megolm_backup.v1"
                          . ((encrypted . ((old . ((iv . "i") (ciphertext . "c")
                                                   (mac . "m"))))))))))
    (setf (leman-session-e2ee session) agent)
    (cl-letf (((symbol-function #'leman-e2ee--api-sync)
               (lambda (_session endpoint &rest args)
                 (cond ((equal endpoint "room_keys/versions")
                        '((versions . ((12151043 . ((algorithm . "alg")
                                                    (auth_data . "auth")))))))
                       ((equal endpoint "room_keys/keys")
                        (should (equal (cadr (car (plist-get args :params)))
                                       "12151043"))
                        '((rooms . ((r1 . ((sessions . ((s1 . t)))))))))
                       (t (error "unexpected endpoint %S" endpoint)))))
              ((symbol-function #'leman-e2ee--backup-version-info)
               (lambda (_session) '((version . "12154626"))))
              ((symbol-function #'leman-e2ee--account-data-get)
               (lambda (_session type)
                 (alist-get type account-data nil nil #'equal)))
              ((symbol-function #'completing-read)
               (lambda (_prompt _choices &rest _) "12151043 (alg)"))
              ((symbol-function #'read-string)
               (lambda (&rest _) "EsOld"))
              ((symbol-function #'leman-e2ee-ssss-check-key)
               (lambda (_agent key-id _recovery _content) (equal key-id "old")))
              ((symbol-function #'leman-e2ee-ssss-decrypt-secret)
               (lambda (&rest _) "EsOldKey"))
              ((symbol-function #'leman-e2ee-backup-verify)
               (lambda (_agent key _info) (equal key "EsOldKey")))
              ((symbol-function #'leman-e2ee-backup-import)
               (lambda (_agent recovery-key rooms mark-backed-up)
                 (push (list recovery-key rooms mark-backed-up) imports)
                 '((imported . 3) (total . 3))))
              ((symbol-function #'leman-e2ee--backup-pump)
               (lambda (_session _agent) (cl-incf pumps)))
              ((symbol-function #'leman-e2ee--retry-decryption)
               (lambda (_session) (setf retries t)))
              ((symbol-function #'leman-message) #'ignore))
      (leman-e2ee-restore-old-backup session)
      ;; The old version's key unlocked its entry; the keys were
      ;; imported pending re-upload, and the pump + retry ran.
      (should (equal imports '(("EsOldKey" ((r1 . ((sessions . ((s1 . t)))))) nil))))
      (should (= pumps 1))
      (should retries))))

(ert-deftest leman-e2ee-restore-old-backup-unsupported-homeserver ()
  ;; Conduit-family homeservers don't implement the versions list:
  ;; the command fails with a clear message, not a raw plz error.
  (let* ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql)))
         (session (make-leman-session :user (make-leman-user :id "@vv:x.org"))))
    (setf (leman-session-e2ee session) agent)
    (cl-letf (((symbol-function #'leman-e2ee--backup-version-info) #'ignore)
              ((symbol-function #'leman-e2ee--api-sync)
               (lambda (_session _endpoint &rest _)
                 (signal 'plz-error
                         (list (make-plz-error :response
                                               (make-plz-response :status 404)))))))
      (let ((err (should-error (leman-e2ee-restore-old-backup session)
                               :type 'user-error)))
        (should (string-search "does not support listing key backup versions"
                               (cadr err)))))))

(ert-deftest leman-e2ee--delete-backup-version ()
  (let ((calls nil))
    (cl-letf (((symbol-function #'leman-e2ee--api-sync)
               (lambda (_session endpoint &rest args)
                 (push (cons endpoint args) calls)
                 '())))
      (leman-e2ee--delete-backup-version nil "69562")
      (should (equal (car calls)
                     (cons "room_keys/version/69562"
                           (list :method 'delete :version "v3")))))))

(ert-deftest leman-e2ee--ensure-version-current-noop ()
  (let ((deletes 0))
    (cl-letf (((symbol-function #'leman-e2ee--backup-version-info)
               (lambda (_session) '((version . "12150227"))))
              ((symbol-function #'leman-e2ee--delete-backup-version)
               (lambda (&rest _) (cl-incf deletes))))
      (should (leman-e2ee--ensure-version-current nil "12150227"))
      (should (= deletes 0)))))

(ert-deftest leman-e2ee--ensure-version-current-deletes-stale ()
  ;; The lexicographic-latest server bug: the shorter, older version
  ;; stays current; deleting it makes the new version current.
  (let* ((versions '("69562" "12150227"))
         (deletes nil))
    (cl-letf (((symbol-function #'leman-e2ee--backup-version-info)
               (lambda (_session) `((version . ,(pop versions)))))
              ((symbol-function #'leman-e2ee--delete-backup-version)
               (lambda (_session version) (push version deletes)))
              ((symbol-function #'y-or-n-p) (lambda (&rest _) t)))
      (should (leman-e2ee--ensure-version-current nil "12150227"))
      (should (equal deletes (list "69562"))))))

(ert-deftest leman-e2ee--ensure-version-current-refusal-errors ()
  (cl-letf (((symbol-function #'leman-e2ee--backup-version-info)
             (lambda (_session) '((version . "69562"))))
            ((symbol-function #'leman-e2ee--delete-backup-version)
             (lambda (&rest _) (error "must not delete")))
            ((symbol-function #'y-or-n-p) (lambda (&rest _) nil)))
    (should-error (leman-e2ee--ensure-version-current nil "12150227")
                  :type 'user-error)))

(ert-deftest leman-e2ee--ensure-version-current-no-version-is-fine ()
  ;; When the homeserver has no version at all, ours is current.
  (cl-letf (((symbol-function #'leman-e2ee--backup-version-info)
             (lambda (_session) (user-error "Leman E2EE: no key backup exists"))))
    (should (leman-e2ee--ensure-version-current nil "12150227"))))

(declare-function leman-e2ee-restore-keys "leman")
(declare-function leman-e2ee--restore-with-agent-key "leman")
(declare-function leman-e2ee--restore-backup "leman")
(declare-function leman-e2ee--enable-backup-and-import "leman")
(declare-function leman-e2ee--create-backup-version "leman")
(declare-function leman-e2ee--api-sync "leman")
(declare-function leman-e2ee--decrypt-ssss-secret "leman")
(declare-function leman-e2ee--import-cross-signing "leman")
(declare-function leman-e2ee-backup-verify "leman-e2ee")
(declare-function leman-e2ee-ssss-decrypt-secret "leman-e2ee")
(declare-function leman-e2ee--backup-version-info "leman")

(ert-deftest leman-e2ee-restore-keys-uses-agent-key ()
  ;; When the agent holds the current backup's decryption key,
  ;; restoring needs no recovery key and no secret-storage unlock.
  (let* ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql)))
         (session (make-leman-session :user (make-leman-user :id "@me:x.org")
                                      :e2ee agent))
         (imports nil)
         (pumps 0)
         (fallbacks 0))
    (cl-letf (((symbol-function #'leman-e2ee-backup-recovery-key)
               (lambda (_agent) "EsSaved"))
              ((symbol-function #'leman-e2ee--enable-backup-and-import)
               (lambda (_session _agent key) (push key imports)))
              ((symbol-function #'leman-e2ee--backup-pump)
               (lambda (_session _agent) (cl-incf pumps)))
              ((symbol-function #'leman-e2ee--restore-backup)
               (lambda (&rest _) (error "restore-backup must not be called")))
              ((symbol-function #'leman-message) #'ignore))
      (should (leman-e2ee-restore-keys session))
      (should (equal imports (list "EsSaved")))
      (should (= pumps 1)))
    ;; Without a saved key, the secret-storage flow is used.
    (cl-letf (((symbol-function #'leman-e2ee-backup-recovery-key)
               (lambda (_agent) nil))
              ((symbol-function #'leman-e2ee--restore-backup)
               (lambda (&rest _) (cl-incf fallbacks)))
              ((symbol-function #'leman-message) #'ignore))
      (should (leman-e2ee-restore-keys session))
      (should (= fallbacks 1)))))

(ert-deftest leman-e2ee--restore-with-agent-key-falls-back-on-mismatch ()
  ;; A saved key that does not match the current backup version must
  ;; not abort: restore falls back to the secret-storage flow.
  (let* ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql)))
         (session (make-leman-session :user (make-leman-user :id "@me:x.org")
                                      :e2ee agent))
         (fallbacks 0))
    (cl-letf (((symbol-function #'leman-e2ee-backup-recovery-key)
               (lambda (_agent) "EsStale"))
              ((symbol-function #'leman-e2ee--enable-backup-and-import)
               (lambda (&rest _) (user-error "Leman E2EE: key does not match")))
              ((symbol-function #'leman-e2ee--restore-backup)
               (lambda (&rest _) (cl-incf fallbacks)))
              ((symbol-function #'leman-message) #'ignore))
      (should (leman-e2ee-restore-keys session))
      (should (= fallbacks 1)))))

(ert-deftest leman-e2ee--restore-backup-heals-stale-secret ()
  ;; When the unlock fails but the agent holds the current backup's
  ;; key and the entered recovery key unlocks the default key, the
  ;; backup key is re-stored under the default key and the keys are
  ;; restored -- no new backup version is created.
  (let* ((agent (leman-e2ee--create :pending (make-hash-table :test #'eql)))
         (session (make-leman-session :user (make-leman-user :id "@me:x.org")
                                      :e2ee agent))
         (imports nil)
         (re-stores nil)
         (pumps 0)
         (messages nil)
         (account-data `(("m.secret_storage.default_key" . ((key . "kd")))
                         ("m.secret_storage.key.kd" . ((algorithm . "x")))
                         ("m.secret_storage.secret.m.megolm_backup.v1"
                          . ((encrypted . ((kd . ((iv . "old"))))))))))
    (cl-letf (((symbol-function #'leman-e2ee--account-data-get)
               (lambda (_session type)
                 (alist-get type account-data nil nil #'equal)))
              ((symbol-function #'read-string)
               (lambda (&rest _) "EsUser"))
              ((symbol-function #'leman-e2ee--unlock-backup-secret)
               (lambda (&rest _) (user-error "Leman E2EE: no entry holds the current version's key")))
              ((symbol-function #'leman-e2ee--backup-version-info)
               (lambda (_session) '((version . "12148797") (algorithm . "alg") (auth_data . "auth"))))
              ;; The entered key is not the backup's own recovery key;
              ;; the agent's saved key is.
              ((symbol-function #'leman-e2ee-backup-verify)
               (lambda (_agent key _info) (equal key "EsSaved")))
              ((symbol-function #'leman-e2ee-backup-recovery-key)
               (lambda (_agent) "EsSaved"))
              ((symbol-function #'leman-e2ee-ssss-check-key)
               (lambda (_agent _key-id _recovery _content) t))
              ((symbol-function #'leman-e2ee--enable-backup-and-import)
               (lambda (_session _agent key) (push key imports)))
              ((symbol-function #'leman-e2ee--re-store-backup-secret)
               (lambda (_session _agent recovery backup)
                 (push (cons recovery backup) re-stores)))
              ((symbol-function #'leman-e2ee--backup-pump)
               (lambda (_session _agent) (cl-incf pumps)))
              ((symbol-function #'leman-message)
               (lambda (format &rest args)
                 (push (apply #'format format args) messages)))
              ((symbol-function #'y-or-n-p)
               (lambda (&rest _) (error "no fresh version should be offered"))))
      (leman-e2ee--restore-backup session agent 'adopt)
      (should (equal imports (list "EsSaved")))
      (should (equal re-stores (list (cons "EsUser" "EsSaved"))))
      (should (= pumps 1))
      (should (equal (car messages)
                     "Leman E2EE: the backup key is stored under the default secret-storage key again; keys restored")))))

(ert-deftest leman-e2ee-backup-recovery-key ()
  (pcase-let* ((fake (leman-e2ee-tests--fake-agent
                      '((backup_recovery_key . ((recovery_key . "EsBack"))))))
               (agent (car fake)))
    (should (equal (leman-e2ee-backup-recovery-key agent) "EsBack")))
  (pcase-let* ((fake (leman-e2ee-tests--fake-agent
                      '((backup_recovery_key . ((recovery_key))))))
               (agent (car fake)))
    (should-not (leman-e2ee-backup-recovery-key agent))))

(ert-deftest leman-e2ee--key-export-import-wrappers ()
  (pcase-let* ((fake (leman-e2ee-tests--fake-agent
                      '((export_room_keys . ((keys . "-----BEGIN MEGOLM SESSION DATA-----")))
                        (import_room_keys . ((imported . 2) (total . 3))))))
               (agent (car fake)))
    (should (equal (leman-e2ee--export-keys agent "secret")
                   "-----BEGIN MEGOLM SESSION DATA-----"))
    (should (equal (leman-e2ee--import-keys agent "DATA" "secret")
                   '((imported . 2) (total . 3))))
    ;; The params reach the agent.
    (let ((sent (mapcar #'leman-e2ee--decode (cdr fake))))
      (should (equal (alist-get 'params
                                (seq-find (lambda (r)
                                            (equal (alist-get 'cmd r) "export_room_keys"))
                                          sent))
                     '((passphrase . "secret"))))
      (should (equal (alist-get 'params
                                (seq-find (lambda (r)
                                            (equal (alist-get 'cmd r) "import_room_keys"))
                                          sent))
                     '((keys . "DATA") (passphrase . "secret")))))))

(ert-deftest leman-e2ee--format-recovery-key ()
  (should (equal (leman-e2ee--format-recovery-key "EsTjUCTr1234ABCD")
                 "EsTj UCTr 1234 ABCD"))
  (should (equal (leman-e2ee--format-recovery-key "Es")
                 "Es")))

(ert-deftest leman-e2ee-backup-dump ()
  (let* ((fake (leman-e2ee-tests--fake-agent
                '((backup_status . ((enabled . t) (version . "3")
                                    (room_key_counts . ((total . 5) (backed_up . 4))))))))
         (session (make-leman-session :user (make-leman-user :id "@vv:x.org")))
         (account-data `(("m.secret_storage.default_key" . ((key . "kDef")))
                         ("m.secret_storage.key.kDef"
                          . ((algorithm . "m.secret_storage.v1.aes-hmac-sha2")
                             (iv . "i") (mac . "m")))
                         ("m.secret_storage.secret.m.megolm_backup.v1"
                          . ((encrypted . (("kDef" . ((iv . "i") (ciphertext . "c")
                                                       (mac . "m"))))))))))
    (setf (leman-session-e2ee session) (car fake))
    (cl-letf (((symbol-function #'leman-e2ee--account-data-get)
               (lambda (_session type)
                 (alist-get type account-data nil nil #'equal)))
              ((symbol-function #'leman-api)
               (lambda (_session _endpoint &rest _args)
                 '((version . "3")
                   (algorithm . "m.megolm_backup.v1.curve25519-aes-sha2")
                   (auth_data . ((public_key . "pk1")))))))
      (leman-e2ee-backup-dump session)
      (with-current-buffer "*Leman backup state*"
        (let ((text (buffer-string)))
          (should (string-search "Default secret-storage key: kDef" text))
          (should (string-search "entry encrypted for key kDef" text))
          (should (string-search "Current backup version: 3" text))
          (should (string-search "backup public key: pk1" text))
          (should (string-search "4 of 5 room keys backed up" text))
          (should (string-search "Recovery key" text))))
      (kill-buffer "*Leman backup state*"))))

(ert-deftest leman-e2ee-backup-dump-missing-pieces ()
  ;; Absent account data and no backup version must not error; the
  ;; dump reports them.  The homeserver stub returns a real 404
  ;; plz-error struct, so the account-data and backup-version error
  ;; conversions run for real (a 404 on room_keys/version means "no
  ;; backup", not "request failed").
  (let* ((fake (leman-e2ee-tests--fake-agent
                '((backup_status . ((enabled)))
                  (cross_signing_status . ((has_master)
                                           (has_self_signing)
                                           (has_user_signing))))))
         (session (make-leman-session :user (make-leman-user :id "@vv:x.org"))))
    (setf (leman-session-e2ee session) (car fake))
    (cl-letf (((symbol-function #'leman-api)
               (lambda (_session _endpoint &rest _args)
                 (make-plz-error :response (make-plz-response :status 404)))))
      (leman-e2ee-backup-dump session)
      (with-current-buffer "*Leman backup state*"
        (let ((text (buffer-string)))
          (should (string-search "Default secret-storage key: none" text))
          (should (string-search "NOT STORED" text))
          (should (string-search "Current backup version: none" text))
          (should (string-search "Agent: backup disabled" text))
          (should (string-search "Agent: private cross-signing keys (master/self-signing/user-signing): NO/NO/NO" text))
          (should (string-search "m.cross_signing.master secret: NOT STORED" text))))
      (kill-buffer "*Leman backup state*"))))

;;;; Footer

(provide 'leman-e2ee-tests)

;;;; leman-e2ee-tests.el ends here
