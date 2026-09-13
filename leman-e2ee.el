;;;; leman-e2ee.el --- E2EE subprocess agent support        -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation

;; Author: ThArGos <thargos@gmail.com>
;; Keywords: comm

;; This file is part of Leman.

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

;; Support for Leman's end-to-end encryption, provided by the
;; `leman-agent' subprocess (a small Rust binary wrapping the
;; matrix-sdk-crypto state machine; see e2ee/PROTOCOL.org).
;;
;; The agent speaks a line-delimited JSON protocol over stdio:
;; requests carry an id, responses correlate by id.  This library
;; manages the subprocess, correlates responses, and exposes the
;; protocol commands as functions.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'json)
(require 'subr-x)

;;;; Variables

(defgroup leman-e2ee nil
  "Leman end-to-end encryption."
  :group 'leman)

(defcustom leman-e2ee-agent-program nil
  "Path to the `leman-agent' executable.
If nil, it is looked up on the `exec-path', then relative to the
Leman installation (e2ee/agent/target/{release,debug}/leman-agent)."
  :type '(choice (const :tag "Auto-discover" nil)
                 (file :tag "Path")))

(defcustom leman-e2ee-request-timeout 30
  "Seconds to wait for an agent response before signaling an error."
  :type 'natnum)

(defcustom leman-e2ee-initialize-timeout 60
  "Seconds to wait for the agent's `initialize' command.
Initialization may have to ratchet an existing store open."
  :type 'natnum)

(defcustom leman-e2ee-data-directory
  (expand-file-name "leman/"
                    (or (getenv "XDG_DATA_HOME")
                        "~/.local/share/"))
  "Directory in which the E2EE crypto store is kept.
The store contains the account's key material and is kept with
user-only permissions; it is not itself encrypted (the same
trade-off as Pantalaimon)."
  :type 'directory)

(define-error 'leman-e2ee-error "Leman E2EE agent error")

;;;; Structs

(cl-defstruct (leman-e2ee
               (:constructor leman-e2ee--create))
  "An E2EE agent connection."
  (user-id nil :read-only t)
  (device-id nil :read-only t)
  process
  log-buffer
  (identity-keys nil)
  (exit nil)
  (next-id 1)
  (partial "")
  (pending (make-hash-table :test #'eql))
  (fake nil))

;;;; Encoding/decoding

(defun leman-e2ee--encode (id command &optional params)
  "Encode request ID, COMMAND, and PARAMS as a protocol line."
  ;; NOTE: `json-encode' (not `json-serialize') is used because
  ;; request params may contain string-keyed alists (e.g. message
  ;; content built with `leman-alist'), which `json-serialize'
  ;; rejects.  Absent sync fields then encode as null rather than an
  ;; empty object; the agent treats both the same.  Arrays may be
  ;; lists or vectors; both encode as JSON arrays.
  (json-encode (append (list (cons 'id id)
                             (cons 'cmd command))
                       (when params
                         (list (cons 'params params))))))

(defun leman-e2ee--decode (line)
  "Decode a protocol LINE into an alist, or nil if unparseable."
  ;; NOTE: Arrays decode to vectors (the default "array" type) so
  ;; that data received from the agent can be sent back to it (and to
  ;; the homeserver) with `json-serialize', which requires vectors
  ;; for arrays.
  (ignore-errors
    (json-parse-string line
                       :object-type 'alist
                       :array-type 'array
                       :null-object nil
                       :false-object nil)))

;;;; Logging

(defun leman-e2ee--log (agent string)
  "Log STRING for AGENT."
  (let ((buffer (leman-e2ee-log-buffer agent)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-max))
            (insert string "\n")))
      (message "Leman E2EE: %s" string))))

;;;; Sending/receiving

(defun leman-e2ee--send (agent line)
  "Send protocol LINE to AGENT."
  (let ((fake (leman-e2ee-fake agent)))
    (if fake
        (funcall fake agent line)
      (let ((process (leman-e2ee-process agent)))
        (unless (and process (process-live-p process))
          (signal 'leman-e2ee-error
                  (list "exit" "agent is not running")))
        (process-send-string process (concat line "\n"))))))

(defun leman-e2ee-handle-line (agent line)
  "Handle one response LINE from AGENT.
Responses are correlated by id and stored in the agent's pending
table; responses without a recognized id are logged."
  (let ((response (leman-e2ee--decode line)))
    (cond
     ((null response)
      (leman-e2ee--log agent (format "unparseable response: %s" line)))
     ((alist-get 'id response)
      ;; Use key presence (assq), not value truthiness: an empty "ok"
      ;; object ("ok":{}) decodes to a nil value but is still a
      ;; success, and elisp cannot distinguish {} from null.
      (puthash (alist-get 'id response)
               (cond ((assq 'ok response)
                      (cons 'ok (alist-get 'ok response)))
                     ((assq 'err response)
                      (cons 'err (alist-get 'err response)))
                     (t (cons 'err nil)))
               (leman-e2ee-pending agent)))
     (t (leman-e2ee--log agent (format "stray response: %s" line))))))

(defun leman-e2ee--process-filter (process string)
  "Filter for the agent PROCESS, accumulating STRING into lines."
  (let ((agent (process-get process 'leman-e2ee)))
    (when agent
      (let ((data (concat (leman-e2ee-partial agent) string)))
        (setf (leman-e2ee-partial agent) "")
        (while (string-match "\n" data)
          (let ((line (substring data 0 (match-beginning 0))))
            (setq data (substring data (match-end 0)))
            (unless (string-empty-p line)
              (leman-e2ee-handle-line agent line))))
        (setf (leman-e2ee-partial agent) data)))))

(defun leman-e2ee--sentinel (process event)
  "Sentinel for the agent PROCESS, recording EVENT."
  (let ((agent (process-get process 'leman-e2ee)))
    (when agent
      (setf (leman-e2ee-exit agent) event))))

;;;; Requests

(defun leman-e2ee--next-id (agent)
  "Return the next protocol request ID for AGENT."
  (let ((id (leman-e2ee-next-id agent)))
    (cl-incf (leman-e2ee-next-id agent))
    id))

(defun leman-e2ee-request (agent command &optional params timeout)
  "Send COMMAND with PARAMS to AGENT and return the \"ok\" value.
Signal `leman-e2ee-error' if the agent reports an error, exits,
or does not respond within TIMEOUT seconds
(`leman-e2ee-request-timeout' by default)."
  (let* ((id (leman-e2ee--next-id agent))
         (deadline (+ (float-time) (or timeout leman-e2ee-request-timeout)))
         result)
    (leman-e2ee--send agent (leman-e2ee--encode id command params))
    (while (and (not result)
                (< (float-time) deadline))
      (setq result (gethash id (leman-e2ee-pending agent)))
      (unless result
        (let ((process (leman-e2ee-process agent)))
          (cond
           ((and process (not (process-live-p process)))
            (signal 'leman-e2ee-error
                    (list "exit" (or (leman-e2ee-exit agent)
                                     "agent is not running"))))
           (t (accept-process-output process 0.1))))))
    (remhash id (leman-e2ee-pending agent))
    (cond
     (result
      (pcase result
        (`(ok . ,value) value)
        (`(err . ,err) (signal 'leman-e2ee-error
                               (list (alist-get 'code err)
                                     (alist-get 'message err))))))
     (t (signal 'leman-e2ee-error
                (list "timeout" (format "no response to %S within %s seconds"
                                        command (or timeout leman-e2ee-request-timeout))))))))

;;;; Decrypting events

(defun leman-e2ee-decrypt-event (agent event)
  "Decrypt EVENT (a m.room.encrypted event alist) with AGENT.
EVENT must have a `room_id' key (events from sync responses
don't; callers must add it).  Return the decrypted event alist,
or EVENT unchanged if it isn't encrypted or decryption fails
(e.g. the room key hasn't arrived yet; the caller should keep the
encrypted event and retry when keys arrive)."
  (if (and agent (equal (alist-get 'type event) "m.room.encrypted"))
      (condition-case err
          (leman-e2ee-decrypt-room-event agent (alist-get 'room_id event) event)
        (leman-e2ee-error
         (leman-e2ee--log agent (format "decryption failed: %S" (cdr err)))
         event))
    event))

(defun leman-e2ee--split-path (path)
  "Split an agent request PATH into (VERSION ENDPOINT).
E.g. \"/_matrix/client/v3/keys/upload\" -> (\"v3\" \"keys/upload\")."
  (let ((segments (split-string path "/")))
    (unless (and (equal (nth 0 segments) "")
                 (equal (nth 1 segments) "_matrix")
                 (equal (nth 2 segments) "client")
                 (>= (length segments) 4))
      (signal 'leman-e2ee-error
              (list "invalid" (format "unexpected request path: %S" path))))
    (list (nth 3 segments) (string-join (nthcdr 4 segments) "/"))))

;;;; Lifecycle

(defun leman-e2ee--load-dir ()
  "Return the directory Leman is loaded from."
  (file-name-directory
   (or (locate-library "leman.el" t)
       default-directory)))

(defun leman-e2ee--agent-program ()
  "Return the path to the `leman-agent' executable."
  (or leman-e2ee-agent-program
      (executable-find "leman-agent")
      (let ((load-dir (leman-e2ee--load-dir)))
        (seq-find #'file-executable-p
                  (list (expand-file-name "e2ee/agent/target/release/leman-agent" load-dir)
                        (expand-file-name "e2ee/agent/target/debug/leman-agent" load-dir))))))

(defun leman-e2ee--agent-stale-p (program &optional load-dir)
  "Return non-nil when the agent PROGRAM is older than its source.
LOAD-DIR is the Leman installation directory (for tests)."
  (let* ((load-dir (or load-dir (leman-e2ee--load-dir)))
         (source (expand-file-name "e2ee/agent/src/lib.rs" load-dir)))
    (and (file-exists-p source)
         (file-exists-p program)
         (time-less-p (file-attribute-modification-time
                       (file-attributes program))
                      (file-attribute-modification-time
                       (file-attributes source))))))

(defun leman-e2ee-build-agent ()
  "Build the E2EE agent program with cargo.
Uses the agent source directory of the Leman installation.  Note
that cargo must be found on the `exec-path'."
  (interactive)
  (unless (executable-find "cargo")
    (user-error "Leman E2EE: cargo not found on `exec-path'; install Rust or add ~/.cargo/bin to the `exec-path'"))
  (let* ((agent-dir (expand-file-name "e2ee/agent" (leman-e2ee--load-dir))))
    (unless (file-directory-p agent-dir)
      (user-error "Leman E2EE: agent source not found at %S" agent-dir))
    (let ((default-directory agent-dir))
      (compilation-start "cargo build"))))

(defun leman-e2ee--sanitize-name (name)
  "Return NAME with characters unsafe for file names replaced."
  (replace-regexp-in-string "[^[:alnum:]._-]" "_" name))

(defun leman-e2ee--store-path (user-id device-id)
  "Return the crypto store directory for USER-ID and DEVICE-ID.
The store is kept per device: a device's crypto identity must
never be reused under another device.  The directory is created
and restricted to the current user."
  (let* ((user-dir (expand-file-name
                    (concat "crypto/" (leman-e2ee--sanitize-name user-id) "/")
                    leman-e2ee-data-directory))
         (path (expand-file-name
                (concat (leman-e2ee--sanitize-name device-id) "/")
                user-dir)))
    (make-directory path t)
    (set-file-modes (directory-file-name path) #o700)
    path))

(defun leman-e2ee--discard-store (user-id device-id)
  "Delete the crypto store of USER-ID's DEVICE-ID.
Used when the session is signed out (hard logout): the spec
requires that persisted encryption keys and device information
are not reused.  Also removes legacy user-level store files
belonging to a former single-store layout."
  (let* ((user-dir (expand-file-name
                    (concat "crypto/" (leman-e2ee--sanitize-name user-id) "/")
                    leman-e2ee-data-directory))
         (device-dir (expand-file-name
                      (leman-e2ee--sanitize-name device-id) user-dir)))
    (when (file-directory-p device-dir)
      (delete-directory device-dir 'recursive))
    ;; Legacy layout: the store database used to live directly in the
    ;; user directory.
    (dolist (file (directory-files user-dir t "\\`matrix-sdk-crypto\\.sqlite3"))
      (ignore-errors (delete-file file)))))

(defun leman-e2ee-start (user-id device-id &optional store-path)
  "Start an E2EE agent for USER-ID and DEVICE-ID.
The agent subprocess is spawned, version-checked, and
initialized; the crypto store (STORE-PATH or under
`leman-e2ee-data-directory') is created or reopened.  Return the
`leman-e2ee' object."
  (let* ((program (or (leman-e2ee--agent-program)
                      (error "Leman E2EE agent program not found; see `leman-e2ee-agent-program'")))
         (load-dir (leman-e2ee--load-dir)))
    (when (leman-e2ee--agent-stale-p program load-dir)
      ;; The binary is older than its source (e.g. after a package
      ;; upgrade): the agent may lag behind the protocol.
      (display-warning
       'leman
       "Leman E2EE: the agent binary is older than its source; run M-x leman-e2ee-build-agent and reconnect."))
    (let* ((store-path (or store-path (leman-e2ee--store-path user-id device-id)))
           (agent (leman-e2ee--create :user-id user-id :device-id device-id))
           (buffer (get-buffer-create (format " *Leman E2EE agent[%s]*" user-id)))
           (process (make-process
                     :name (format "leman-agent[%s]" user-id)
                     :buffer buffer
                     :command (list program)
                     :connection-type 'pipe
                     :noquery t
                     :filter #'leman-e2ee--process-filter
                     :sentinel #'leman-e2ee--sentinel)))
      (setf (leman-e2ee-process agent) process
            (leman-e2ee-log-buffer agent) buffer)
      (process-put process 'leman-e2ee agent)
      (let ((hello (leman-e2ee-request agent "hello")))
        (unless (equal (alist-get 'protocol_version hello) 1)
          (leman-e2ee-stop agent)
          (error "Leman E2EE agent protocol version mismatch: %S" hello)))
      (let ((initialized (leman-e2ee-request
                          agent "initialize"
                          (list (cons 'user_id user-id)
                                (cons 'device_id device-id)
                                (cons 'store_path (directory-file-name store-path)))
                          leman-e2ee-initialize-timeout)))
        (setf (leman-e2ee-identity-keys agent)
              (alist-get 'identity_keys initialized)))
      agent)))

(defun leman-e2ee-stop (agent)
  "Stop AGENT's subprocess."
  (let ((process (leman-e2ee-process agent)))
    (when (and process (process-live-p process))
      (ignore-errors
        (leman-e2ee-request agent "quit" nil 1))
      (when (process-live-p process)
        (delete-process process)))))

;;;; Protocol commands

(defun leman-e2ee-outgoing-requests (agent)
  "Return the list of HTTP requests the agent wants performed.
Each request is an alist with ~id~, ~method~, ~path~, and ~body~
keys (see e2ee/PROTOCOL.org); the response of each must be
reported with `leman-e2ee-mark-request-as-sent'."
  (alist-get 'requests (leman-e2ee-request agent "outgoing_requests")))

(defun leman-e2ee-mark-request-as-sent (agent request-id response)
  "Report to AGENT that REQUEST-ID got RESPONSE from the homeserver."
  (leman-e2ee-request agent "mark_request_as_sent"
                      (list (cons 'request_id request-id)
                            (cons 'response response))))

(defun leman-e2ee-receive-sync-changes
    (agent to-device-events changed-devices one-time-keys-count
           unused-fallback-keys next-batch-token)
  "Feed the E2EE parts of a sync response to AGENT.
Return an alist with \"to_device_events\" (with encrypted events
decrypted) and \"outgoing_requests\".

NOTE: This must be called before the sync's ~next_batch~ token is
persisted, or to-device events (room keys) can be lost."
  (leman-e2ee-request agent "receive_sync_changes"
                      (list (cons 'to_device_events (vconcat to-device-events))
                            (cons 'changed_devices changed-devices)
                            (cons 'one_time_keys_count one-time-keys-count)
                            (cons 'unused_fallback_keys unused-fallback-keys)
                            (cons 'next_batch_token next-batch-token))))

(defun leman-e2ee-decrypt-room-event (agent room-id event)
  "Decrypt EVENT (a m.room.encrypted event) for ROOM-ID with AGENT.
Return the decrypted event as an alist; signal
`leman-e2ee-error' if it cannot be decrypted (e.g. session not
found; the caller should retry when keys arrive)."
  (alist-get 'event (leman-e2ee-request agent "decrypt_room_event"
                                        (list (cons 'room_id room-id)
                                              (cons 'event event)))))

(defun leman-e2ee-update-tracked-users (agent users)
  "Track USERS' devices with AGENT (needed before encryption)."
  (leman-e2ee-request agent "update_tracked_users"
                      (list (cons 'users (vconcat users)))))

(defun leman-e2ee-encrypt-event (agent room-id event-type content users)
  "Encrypt CONTENT of EVENT-TYPE for ROOM-ID with AGENT.
USERS are the room's member user IDs (for key sharing).  Return
the response alist: \"status\" is \"ok\" with an \"event\" whose
\"content\" is the encrypted event content, or
\"claims_pending\" (the client must perform the pending outgoing
requests, then retry).  Signal `leman-e2ee-error' on failure."
  (leman-e2ee-request agent "encrypt_room_event"
                      (list (cons 'room_id room-id)
                            (cons 'event_type event-type)
                            (cons 'content content)
                            (cons 'users (vconcat users)))))

;;;; Verification (E3)

(defun leman-e2ee-devices (agent &optional user-id)
  "Return the devices AGENT knows for USER-ID (the agent's own
user by default).  Each device is an alist with ~device_id~,
~display_name~, ~verified~, and ~deleted~ keys."
  (alist-get 'devices
             (leman-e2ee-request
              agent "devices"
              (when user-id (list (cons 'user_id user-id))))))

(defun leman-e2ee-request-verification (agent user-id device-id)
  "Start verifying DEVICE-ID of USER-ID with AGENT.
Return the verification flow ID."
  (alist-get 'flow_id
             (leman-e2ee-request
              agent "request_verification"
              (list (cons 'user_id user-id)
                    (cons 'device_id device-id)))))

(defun leman-e2ee-verification-requests (agent &optional user-id)
  "Return AGENT's verification requests for USER-ID.
Each request is an alist with ~flow_id~, ~user_id~, ~device_id~,
~state~ (created/ready/done/cancelled), ~we_started~, and ~sas~
keys."
  (alist-get 'requests
             (leman-e2ee-request
              agent "verification_requests"
              (when user-id (list (cons 'user_id user-id))))))

(defun leman-e2ee-accept-verification (agent user-id flow-id)
  "Accept the incoming verification FLOW-ID of USER-ID on AGENT."
  (leman-e2ee-request agent "accept_verification"
                      (list (cons 'user_id user-id)
                            (cons 'flow_id flow-id))))

(defun leman-e2ee-start-sas (agent user-id flow-id)
  "Start SAS for the verification FLOW-ID of USER-ID on AGENT."
  (leman-e2ee-request agent "start_sas"
                      (list (cons 'user_id user-id)
                            (cons 'flow_id flow-id))))

(defun leman-e2ee-verification-sas (agent user-id flow-id)
  "Return the SAS state for FLOW-ID of USER-ID on AGENT.
An alist with ~accepted~, ~can_be_presented~, ~done~,
~cancelled~, and (when presentable) ~emoji~ keys."
  (leman-e2ee-request agent "verification_sas"
                      (list (cons 'user_id user-id)
                            (cons 'flow_id flow-id))))

(defun leman-e2ee-accept-sas (agent user-id flow-id)
  "Accept their SAS start for FLOW-ID of USER-ID on AGENT."
  (leman-e2ee-request agent "accept_sas"
                      (list (cons 'user_id user-id)
                            (cons 'flow_id flow-id))))

(defun leman-e2ee-confirm-sas (agent user-id flow-id)
  "Confirm the short auth string for FLOW-ID of USER-ID on AGENT."
  (leman-e2ee-request agent "confirm_sas"
                      (list (cons 'user_id user-id)
                            (cons 'flow_id flow-id))))

(defun leman-e2ee-cancel-verification (agent user-id flow-id)
  "Cancel the verification FLOW-ID of USER-ID on AGENT."
  (leman-e2ee-request agent "cancel_verification"
                      (list (cons 'user_id user-id)
                            (cons 'flow_id flow-id))))

;;;; Footer

(provide 'leman-e2ee)

;;;; leman-e2ee.el ends here
