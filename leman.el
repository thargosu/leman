;;; leman.el --- Matrix client                       -*- lexical-binding: t; -*-

;; URL: https://github.com/thargosu/leman.el
;; Version: 0.1
;; Package-Requires: ((emacs "30.1") (map "2.1") (persist "0.5") (plz "0.6") (taxy "0.10") (taxy-magit-section "0.13") (svg-lib "0.2.5") (transient "0.3.7"))
;; Keywords: comm

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

;; Another Matrix client!  This one is written from scratch and is
;; intended to be more "Emacsy," more suitable for MELPA, etc.  Also
;; it has a shorter, perhaps catchier name, that is a mildly clever
;; play on the name of the official Matrix client and the Emacs Lisp
;; filename extension (oops, I explained the joke), which makes for
;; much shorter symbol names.

;; This file implements the core client library.  Functions that may be called in multiple
;; files belong in `leman-lib'.

;;; Code:

;;;; Debugging

;; NOTE: Uncomment this form and `emacs-lisp-byte-compile-and-load' the file to enable
;; `leman-debug' messages.  This is commented out by default because, even though the
;; messages are only displayed when `warning-minimum-log-level' is `:debug' at runtime, if
;; that is so at expansion time, the expanded macro calls format the message and check the
;; log level at runtime, which is not zero-cost.

;; (eval-and-compile
;;   (require 'warnings)
;;   (setq-local warning-minimum-log-level nil)
;;   (setq-local warning-minimum-log-level :debug))

;;;; Requirements

;; Built in.
(require 'cl-lib)
(require 'dns)
(require 'files)
(require 'map)

;; This package.
(require 'leman-lib)
(require 'leman-e2ee)
(require 'leman-room)
(require 'leman-notifications)
(require 'leman-notify)

;;;; Variables

(defvar leman-sessions nil
  "Alist of active `leman-session' sessions, keyed by MXID.")

(defvar leman-syncs nil
  "Alist of outstanding sync processes for each session.")

(defvar leman-users (make-hash-table :test #'equal)
  ;; NOTE: When changing the leman-user struct, it's necessary to
  ;; reset this table to clear old-type structs.
  "Hash table storing user structs keyed on user ID.")

(defvar leman-progress-reporter nil
  "Used to report progress while processing sync events.")

(defvar leman-progress-value nil
  "Used to report progress while processing sync events.")

(defvar leman-sync-callback-hook
  '(leman--update-room-buffers leman--auto-sync leman-tabulated-room-list-auto-update
                               leman-room-list-auto-update)
  "Hook run after `leman--sync-callback'.
Hooks are called with one argument, the session that was
synced.")

(defvar leman-event-hook
  '(leman-notify leman--process-event leman--put-event)
  "Hook called for events.
Each function is called with three arguments: the event, the
room, and the session.  This hook isn't intended to be modified
by users; ones who do so should know what they're doing.")

(defvar leman-default-sync-filter
  '((room (state (lazy_load_members . t))
          (timeline (lazy_load_members . t))))
  "Default filter for sync requests.")

(defvar leman-images-queue (make-plz-queue :limit 5)
  "`plz' HTTP request queue for image requests.")

(defvar leman-read-receipt-idle-timer nil
  "Idle timer used to update read receipts.")

(defvar leman-connect-user-id-history nil
  "History list of user IDs entered into `leman-connect'.")

;; From other files.
(defvar leman-room-avatar-max-width)
(defvar leman-room-avatar-max-height)

;;;; Customization

(defgroup leman-faces nil
  "Faces for Leman."
  :group 'leman)

(defgroup leman nil
  "Options for Leman, the Matrix client."
  :group 'comm)

(defcustom leman-save-sessions nil
  "Save session to disk.
Writes the session file when Emacs is killed."
  :type 'boolean
  :set (lambda (option value)
         (set-default option value)
         (if value
             (add-hook 'kill-emacs-hook #'leman--kill-emacs-hook)
           (remove-hook 'kill-emacs-hook #'leman--kill-emacs-hook))))

(defcustom leman-sessions-file "~/.cache/leman.el"
  ;; FIXME: Expand correct XDG cache directory (new in Emacs 27).
  "Save username and access token to this file."
  :type 'file)

(defcustom leman-auto-sync t
  "Automatically sync again after syncing."
  :type 'boolean)

(defcustom leman-after-initial-sync-hook
  '(leman-room-list--after-initial-sync leman-view-initial-rooms leman--link-children leman--run-idle-timer)
  "Hook run after initial sync.
Run with one argument, the session synced."
  :type 'hook)

(defcustom leman-initial-sync-timeout 40
  "Timeout in seconds for initial sync requests.
For accounts in many rooms, the Matrix server may take some time
to prepare the initial sync response, and increasing this timeout
might be necessary."
  :type 'integer)

(defcustom leman-auto-view-rooms nil
  "Rooms to view after initial sync.
Alist mapping user IDs to a list of room aliases/IDs to open buffers for."
  :type '(alist :key-type (string :tag "Local user ID")
                :value-type (repeat (string :tag "Room alias/ID"))))

(defcustom leman-disconnect-hook '(leman-kill-buffers leman--stop-idle-timer)
  ;; FIXME: Put private functions in a private hook.
  "Functions called when disconnecting.
That is, when calling command `leman-disconnect'.  Functions are
called with no arguments."
  :type 'hook)

(defcustom leman-view-room-display-buffer-action '(display-buffer-same-window)
  "Display buffer action to use when opening room buffers.
See function `display-buffer' and info node `(elisp) Buffer
Display Action Functions'."
  :type 'function)

(defcustom leman-auto-view-room-display-buffer-action '(display-buffer-no-window)
  "Display buffer action to use when automatically opening room buffers.
That is, rooms listed in `leman-auto-view-rooms', which see.  See
function `display-buffer' and info node `(elisp) Buffer Display
Action Functions'."
  :type 'function)

(defcustom leman-interrupted-sync-hook '(leman-interrupted-sync-warning)
  "Functions to call when syncing of a session is interrupted.
Only called when `leman-auto-sync' is non-nil.  Functions are
called with one argument, the session whose sync was interrupted.

This hook allows the user to customize how sync interruptions are
handled (e.g. how to be notified)."
  :type 'hook
  :options '(leman-interrupted-sync-message leman-interrupted-sync-warning))

(defcustom leman-sso-server-port 4567
  "TCP port used for local HTTP server for SSO logins.
It shouldn't usually be necessary to change this."
  :type 'integer)

;;;; Commands

(defun leman--new-session (user-id &optional uri-prefix)
  "Return a new session for USER-ID, using URI-PREFIX if given."
  (unless (string-match (rx bos "@" (group (1+ (not (any ":")))) ; Username
                            ":" (group (optional (1+ (not (any blank)))))) ; Server name
                        user-id)
    (user-error "Invalid user ID format: use @USERNAME:SERVER"))
  (let* ((username (match-string 1 user-id))
         (server-name (match-string 2 user-id))
         (uri-prefix (or uri-prefix (leman--hostname-uri server-name)))
         (user (make-leman-user :id user-id :username username))
         (server (make-leman-server :name server-name :uri-prefix uri-prefix))
         (transaction-id (leman--initial-transaction-id))
         (initial-device-display-name (format "Leman.el: %s@%s"
                                              ;; Just to be extra careful:
                                              (or user-login-name "[unknown user-login-name]")
                                              (or (system-name) "[unknown system-name]")))
         ;; NOTE: A fresh login must NOT claim a device ID (e.g. a
         ;; deterministic one): a fresh login is a new device
         ;; identity, and reusing a removed device's ID would
         ;; resurrect its verified keys.  The server mints the ID;
         ;; only session restoration reuses a device.
         (device-id nil))
    (make-leman-session :user user :server server :transaction-id transaction-id
                        :device-id device-id :initial-device-display-name initial-device-display-name
                        :events (make-hash-table :test #'equal))))

(defun leman--password-login (session &optional password)
  "Log in to SESSION using PASSWORD, prompting if not given."
  (pcase-let* (((cl-struct leman-session user initial-device-display-name) session)
               ((cl-struct leman-user id) user)
               (data (leman-alist "type" "m.login.password"
                                  "identifier"
                                  (leman-alist "type" "m.id.user"
                                               "user" id)
                                  "password" (or password
                                                 (read-passwd (format "Password for %s: " id)))
                                  ;; No device_id: the server mints one.
                                  "initial_device_display_name" initial-device-display-name)))
    ;; TODO: Clear password in callback (if we decide to hold on to it for retrying login timeouts).
    (leman-api session "login" :method 'post :data (json-encode data)
      :then (apply-partially #'leman--login-callback session))
    (leman-message "Logging in with password...")))

(defun leman--sso-login-with-token (token session)
  "Submit SSO login TOKEN for SESSION."
  (pcase-let* (((cl-struct leman-session user initial-device-display-name) session)
               ((cl-struct leman-user id) user)
               (data (leman-alist
                      "type" "m.login.token"
                      "identifier" (leman-alist "type" "m.id.user"
                                                "user" id)
                      "token" token
                      ;; No device_id: the server mints one.
                      "initial_device_display_name" initial-device-display-name)))
    (leman-api session "login" :method 'post
      :data (json-encode data)
      :then (apply-partially #'leman--login-callback session))))

(defun leman--sso-login (session)
  "Log in to SESSION using single sign-on.
Starts a throwaway, local HTTP server on `leman-sso-server-port'
to receive the login token, and browses to the server's SSO
redirect page."
  (let (sso-server-process)
    (setf sso-server-process
          (make-network-process
           :name "leman-sso" :family 'ipv4 :host 'local :service leman-sso-server-port
           :filter (lambda (process string)
                     ;; NOTE: This is technically wrong, because it's not guaranteed that the
                     ;; string will be a complete request--it could just be a chunk.  But in
                     ;; practice, if this works, it's much simpler than setting up process log
                     ;; functions and per-client buffers for this throwaway, pretend HTTP server.
                     (when (string-match (rx "GET /?loginToken=" (group (0+ nonl)) " " (0+ nonl)) string)
                       (unwind-protect
                           (progn
                             (leman--sso-login-with-token (match-string 1 string) session)
                             (process-send-string process "HTTP/1.0 202 Accepted
Content-Type: text/plain; charset=utf-8

Leman: SSO login accepted; session token received.  Connecting to Matrix server.  (You may close this page.)")
                             (process-send-eof process))
                         (delete-process sso-server-process)
                         (delete-process process))))
           :server t :noquery t))
    ;; Kill server after 2 minutes in case of problems.
    (run-at-time 120 nil (lambda ()
                           (when (process-live-p sso-server-process)
                             (delete-process sso-server-process))))
    (let ((url (concat (leman-server-uri-prefix (leman-session-server session))
                       "/_matrix/client/r0/login/sso/redirect?redirectUrl=http://localhost:"
                       (number-to-string leman-sso-server-port))))
      (funcall browse-url-secondary-browser-function url)
      (message "Browsing to single sign-on page <%s>..." url))))

(defun leman--login-with-flow (flow session &optional password)
  "Begin login FLOW (\"password\" or \"sso\") for SESSION.
PASSWORD, if given, is used for password login."
  (pcase flow
    ("password" (leman--password-login session password))
    ("sso" (leman--sso-login session))
    (_ (error "Leman: Unsupported login flow: %s  Server:%S"
              flow (leman-server-uri-prefix (leman-session-server session))))))

(defun leman--connect-flows-callback (session password data)
  "Begin a login flow supported by the server for SESSION.
PASSWORD, if given, is used for password login; otherwise the
user is prompted."
  (let ((flows (cl-loop for flow across (map-elt data 'flows)
                        for type = (map-elt flow 'type)
                        when (member type '("m.login.password" "m.login.sso"))
                        collect type)))
    (pcase (length flows)
      (0 (error "Leman: No supported login flows:  Server:%S  Supported flows:%S"
                (leman-server-uri-prefix (leman-session-server session))
                (map-elt data 'flows)))
      (1 (leman--login-with-flow (string-trim-left (car flows) (rx "m.login."))
                                 session password))
      (_ (leman--login-with-flow
          (completing-read "Select authentication method: "
                           (cl-loop for flow in flows
                                    collect (string-trim-left flow (rx "m.login."))))
          session password)))))

(defun leman--session-start-sync (session)
  "Register SESSION in `leman-sessions' and start syncing it."
  ;; HACK: If session is already in leman-sessions, this replaces it.  I think that's okay...
  (setf (alist-get (leman-user-id (leman-session-user session))
                   leman-sessions nil nil #'equal)
        session)
  (leman-e2ee--start-agent session
                           (lambda ()
                             (leman--sync session :timeout leman-initial-sync-timeout))))

(defun leman--connect-args ()
  "Return arguments for interactively calling `leman-connect'.
With prefix arg, ignore any saved session and prompt to log in
again; otherwise, use a saved session if one is available."
  (if current-prefix-arg
      ;; Force new session.
      (list :user-id (read-string "User ID: " nil 'leman-connect-user-id-history))
    ;; Use known session.
    (unless leman-sessions
      ;; Read sessions from disk.
      (condition-case err
          (setf leman-sessions (leman--read-sessions))
        (error (display-warning 'leman (format "Unable to read session data from disk (%s).  Prompting to log in again."
                                               (error-message-string err))))))
    ;; Never resume a revoked session: prompt for a fresh login.
    (setf leman-sessions (cl-remove-if (lambda (entry)
                                         (leman-session-revoked-p (cdr entry)))
                                       leman-sessions))
    (cl-case (length leman-sessions)
      (0 (list :user-id (read-string "User ID: " nil 'leman-connect-user-id-history)))
      (1 (list :session (cdar leman-sessions)))
      (otherwise (list :session (leman-complete-session))))))

;;;###autoload
(cl-defun leman-connect (&key user-id password uri-prefix session)
  "Connect to Matrix with USER-ID and PASSWORD, or using SESSION.
Interactively, with prefix, ignore a saved session and log in
again; otherwise, use a saved session if `leman-save-sessions' is
enabled and a saved session is available, or prompt to log in if
not enabled or available.

If USER-ID or PASSWORD are not specified, the user will be
prompted for them.

If URI-PREFIX is specified, it should be the prefix of the
server's API URI, including protocol, hostname, and optionally
the port, e.g.

  \"https://matrix-client.matrix.org\"
  \"http://localhost:8080\""
  (interactive (leman--connect-args))
  (if session
      ;; Start syncing given session.
      (leman--session-start-sync session)
    ;; Start the login flow.  Prompt for user ID if not given (i.e. if
    ;; not called interactively).
    (unless user-id
      (setf user-id (read-string "User ID: " nil 'leman-connect-user-id-history)))
    (setf session (leman--new-session user-id uri-prefix))
    (when (leman-api session "login"
            :then (apply-partially #'leman--connect-flows-callback session password))
      (message "Leman: Checking server's login flows..."))))
(defun leman-disconnect (sessions)
  "Disconnect from SESSIONS.
Interactively, with prefix, disconnect from all sessions.  If
`leman-auto-sync' is enabled, stop syncing, and clear the session
data.  When enabled, write the session to disk.  Any existing
room buffers are left alive and can be read, but other commands
in them won't work."
  (interactive (list (if current-prefix-arg
                         (mapcar #'cdr leman-sessions)
                       (list (leman-complete-session)))))
  (when leman-save-sessions
    ;; Write sessions before we remove them from the variable.
    (leman--write-sessions leman-sessions))
  (dolist (session sessions)
    (when-let ((agent (leman-session-e2ee session)))
      (leman-e2ee-stop agent))
    (let ((user-id (leman-user-id (leman-session-user session))))
      (when-let ((process (map-elt leman-syncs session)))
        ;; Disable the sync process's ELSE handler, preventing error messages, but still
        ;; allowing `plz--respond' to clean up the buffer, etc.
        (setf (process-get process :plz-else) #'ignore)
        (delete-process process))
      ;; NOTE: I'd like to use `map-elt' here, but not until
      ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=47368> is fixed, I guess.
      (setf (alist-get session leman-syncs nil nil #'equal) nil
            (alist-get user-id leman-sessions nil 'remove #'equal) nil)))
  (unless leman-sessions
    ;; HACK: If no sessions remain, clear the users table.  It might be best
    ;; to store a per-session users table, but this is probably good enough.
    (clrhash leman-users))
  (run-hooks 'leman-disconnect-hook)
  (message "Leman: Disconnected <%s>."
           (string-join (cl-loop for session in sessions
                                 collect (leman-user-id (leman-session-user session)))
                        ", ")))

(defun leman-kill-buffers ()
  "Kill all Leman buffers.
Useful in, e.g. `leman-disconnect-hook', which see."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (string-prefix-p "leman-" (symbol-name (buffer-local-value 'major-mode buffer)))
      (kill-buffer buffer))))

(defun leman--login-callback (session data)
  "Record DATA from logging in to SESSION and do initial sync."
  (pcase-let* (((map ('access_token token) ('device_id device-id)) data))
    (setf (leman-session-token session) token
          (leman-session-device-id session) device-id)
    (leman--session-start-sync session)))

;;;;; E2EE integration

;; Encrypt outgoing message content before sending.
(setf leman-encrypt-send-content-function #'leman-e2ee--encrypt-content)

(defun leman-e2ee--start-agent (session &optional then)
  "Start an E2EE agent for SESSION, then call THEN, if given.
THEN is also called when the agent can't be started (after a
message is shown).  If SESSION has no device ID (e.g. the session
was restored from disk, which doesn't save device IDs), it is
fetched with the whoami API first."
  (cl-labels ((start-agent
               ()
               (let ((user-id (leman-user-id (leman-session-user session)))
                     (device-id (leman-session-device-id session)))
                 (if (and user-id device-id)
                     (condition-case err
                         (setf (leman-session-e2ee session)
                               (leman-e2ee-start user-id device-id))
                       ;; Use a warning rather than a message: users
                       ;; miss echo-area messages while connecting.
                       (error (display-warning 'leman
                                               (format "Leman E2EE unavailable: %s"
                                                       (error-message-string err)))))
                   (display-warning 'leman "Leman E2EE disabled: device ID unknown."))))
              (finish
               ()
               (when then (funcall then))))
    (if (leman-session-device-id session)
        (progn (start-agent)
               (finish))
      (leman-api session "account/whoami"
        :then (lambda (data)
                (setf (leman-session-device-id session) (alist-get 'device_id data))
                (start-agent)
                (finish))
        :else (lambda (plz-error)
                (display-warning 'leman
                                 (format "Leman E2EE disabled: unable to fetch device ID: %S" plz-error))
                (finish))))))

(defun leman-e2ee-status (session)
  "Show the E2EE status of SESSION in the echo area."
  (interactive (list (leman-complete-session)))
  (let ((agent (leman-session-e2ee session)))
    (if (not agent)
        (let ((user (leman-user-id (leman-session-user session)))
              (program (leman-e2ee--agent-program)))
          (if program
              (message "Leman E2EE: no agent running for %s (agent found at %s; try reconnecting)."
                       user program)
            (message (concat "Leman E2EE: no agent running for %s; agent program not found."
                             "  Build it with `M-x leman-e2ee-build-agent' or set"
                             " `leman-e2ee-agent-program', then reconnect.")
                     user)))
      (message "Leman E2EE: agent %s for %s on device %s (log buffer: %s)"
               (if (process-live-p (leman-e2ee-process agent))
                   "running" "NOT RUNNING")
               (leman-e2ee-user-id agent)
               (leman-e2ee-device-id agent)
               (buffer-name (leman-e2ee-log-buffer agent))))))

(defun leman-e2ee--format-emoji (emoji)
  "Format the EMOJI list for comparing with the other device."
  (mapconcat (lambda (item)
               (format "%s %s" (alist-get 'symbol item)
                       (alist-get 'description item)))
             emoji "   "))

(defcustom leman-e2ee-verify-confirm-function
  #'leman-e2ee--verify-confirm
  "Function called with (DEVICE-ID EMOJI) to compare the SAS emoji.
Return non-nil to confirm the short auth string, nil to cancel."
  :type 'function)

(defun leman-e2ee--verify-confirm (device-id emoji)
  "Ask the user to compare EMOJI for DEVICE-ID with the other device."
  (y-or-n-p (format "Leman E2EE: compare with the other device (%s): %s -- do the emoji match?"
                    device-id (leman-e2ee--format-emoji emoji))))

(defun leman-e2ee--device-verified-p (agent user-id device-id)
  "Return non-nil if DEVICE-ID of USER-ID is verified on AGENT."
  (alist-get 'verified
             (seq-find (lambda (device)
                         (equal (alist-get 'device_id device) device-id))
                       (leman-e2ee-devices agent user-id))))

(defun leman-e2ee--verify-step (agent user-id flow-id device-id)
  "Perform one round of the verification dance for FLOW-ID of
USER-ID on AGENT (verifying DEVICE-ID).  Return `done' when the
device is verified, `cancelled' when the dance was cancelled, and
nil to keep waiting."
  (let* ((request (seq-find (lambda (request)
                              (equal (alist-get 'flow_id request) flow-id))
                            (leman-e2ee-verification-requests agent user-id)))
         (state (alist-get 'state request)))
    (cond
     ((equal state "done") 'done)
     ((equal state "cancelled") 'cancelled)
     ((and (equal state "ready") (not (alist-get 'sas request)))
      ;; The request is accepted on both sides; start the SAS.
      (leman-e2ee-start-sas agent user-id flow-id)
      nil)
     (t
      (let ((sas (ignore-errors
                   (leman-e2ee-verification-sas agent user-id flow-id))))
        (cond
         ((and sas (alist-get 'cancelled sas)) 'cancelled)
         ((and sas (alist-get 'done sas)) 'done)
         ;; Their SAS start arrived before ours (or without ours):
         ;; accept it, or the other device waits forever.
         ((and sas (not (alist-get 'accepted sas)))
          (leman-e2ee-accept-sas agent user-id flow-id)
          nil)
         ((and sas (alist-get 'can_be_presented sas))
          (if (funcall leman-e2ee-verify-confirm-function
                       (alist-get 'device_id request)
                       (alist-get 'emoji sas))
              (progn (leman-e2ee-confirm-sas agent user-id flow-id) nil)
            (leman-e2ee-cancel-verification agent user-id flow-id)
            'cancelled))
         ;; No request and no SAS: the state machine garbage-collects
         ;; both once the dance finishes; the device's trust state is
         ;; then the remaining signal.
         ((leman-e2ee--device-verified-p agent user-id device-id) 'done)
         (t nil)))))))

(defun leman-e2ee-verify (session)
  "Verify a device with SESSION's E2EE agent (emoji SAS).
Pick a device, run the interactive verification dance, and
compare the short auth string with the other device.  An incoming
verification request for the device is accepted; otherwise a new
one is started."
  (interactive (list (leman-complete-session)))
  (let ((agent (leman-session-e2ee session)))
    (unless agent
      (user-error "Leman E2EE: no agent running (try reconnecting)"))
    (let* ((own-user (leman-user-id (leman-session-user session)))
           (user-id (read-string
                     (format "Verify a device of user (default %s): " own-user)
                     nil nil own-user))
           (devices (leman-e2ee-devices agent user-id))
           (choices (mapcar (lambda (device)
                              (cons (alist-get 'device_id device)
                                    (alist-get 'display_name device)))
                            devices))
           (device-id (completing-read "Device: " choices nil t)))
      (when (equal (alist-get 'verified
                              (seq-find (lambda (device)
                                          (equal (alist-get 'device_id device) device-id))
                                        devices))
                    t)
        (user-error "Leman E2EE: device %s is already verified" device-id))
      (let* ((existing (seq-find (lambda (request)
                                   (equal (alist-get 'device_id request) device-id))
                                 (leman-e2ee-verification-requests agent user-id)))
             (flow-id (if (and existing
                               (not (alist-get 'we_started existing))
                               (member (alist-get 'state existing) '("created" "ready")))
                          (progn
                            (leman-e2ee-accept-verification
                             agent user-id (alist-get 'flow_id existing))
                            (alist-get 'flow_id existing))
                        (leman-e2ee-request-verification agent user-id device-id))))
        (message "Leman E2EE: verifying %s of %s; accept the request on the other device."
                 device-id user-id)
        ;; The dance's to-device events travel with the session's
        ;; syncs; pump the agent's outgoing requests each round.
        ;; This must remain asynchronous: confirming the emoji queues
        ;; the MAC request, and waiting synchronously for a slow
        ;; homeserver here makes Emacs appear to hang immediately after
        ;; the user answers `y'.
        (catch 'finished
          (cl-loop for round from 1 upto 90
                   do (leman-e2ee--process-outgoing-requests session)
                      (let ((result (leman-e2ee--verify-step
                                     agent user-id flow-id device-id)))
                        (pcase result
                          (`done
                           (message "Leman E2EE: device %s is verified." device-id)
                           (throw 'finished t))
                          (`cancelled
                           (message "Leman E2EE: verification of %s was cancelled." device-id)
                           (throw 'finished t))))
                      (sleep-for 2)
                   finally (message "Leman E2EE: verification of %s timed out; run `M-x leman-e2ee-verify' again."
                                    device-id)))
        (leman-e2ee--process-outgoing-requests session)))))

(defun leman-e2ee--decrypt-event (session event &optional room-id)
  "Decrypt EVENT (from ROOM-ID) with SESSION's E2EE agent.
If EVENT is not encrypted, the agent is unavailable, or
decryption fails, return EVENT unchanged."
  (let ((agent (leman-session-e2ee session)))
    (if (and agent (equal (alist-get 'type event) "m.room.encrypted"))
        ;; Sync room events don't include the room ID; the agent needs it.
        (leman-e2ee-decrypt-event agent
                                   (if room-id
                                       (cons (cons 'room_id room-id) event)
                                     event))
      event)))

(defvar leman-e2ee--room-keys-arrived-p nil
  "Non-nil when the last sync delivered room keys to the agent.
Used to trigger a decryption retry at the end of that sync.")

(defun leman-e2ee--decrypt-event-struct (session room-id event)
  "Decrypt raw encrypted EVENT of ROOM-ID with SESSION's agent.
Return the `leman-event' struct; when decryption fails, the raw
event is stashed in the struct's local slot, so that
`leman-e2ee--retry-decryption' can decrypt it later, when the
room's key arrives (e.g. forwarded by another device)."
  (let* ((decrypted (leman-e2ee--decrypt-event session event room-id))
         (event-struct (leman--make-event decrypted)))
    (when (equal (leman-event-type event-struct) "m.room.encrypted")
      (setf (leman-event-local event-struct)
            (cons (cons 'encrypted-raw event) (leman-event-local event-struct))))
    event-struct))

(defcustom leman-e2ee-decrypt-notify-window 300
  "Seconds within which a late-decrypted event still notifies.
Events decrypted by `leman-e2ee--retry-decryption' only run the
notification hook when they were sent within this many seconds,
so that importing older keys does not replay old messages as
notifications."
  :type 'natnum
  :group 'leman-notify)

(defun leman-e2ee--update-decrypted-event (event-struct decrypted session room)
  "Update EVENT-STRUCT in place from the decrypted event DECRYPTED.
The struct is shared by the session's events table and the room's
event lists, so updating it in place propagates everywhere; ROOM's
buffer is refreshed."
  (pcase-let* ((new (leman--make-event decrypted)))
    (setf (leman-event-sender event-struct) (leman-event-sender new)
          (leman-event-content event-struct) (leman-event-content new)
          (leman-event-origin-server-ts event-struct) (leman-event-origin-server-ts new)
          (leman-event-type event-struct) (leman-event-type new)
          (leman-event-unsigned event-struct) (leman-event-unsigned new)
          (leman-event-state-key event-struct) (leman-event-state-key new)
          (leman-event-local event-struct)
          (assq-delete-all 'encrypted-raw (leman-event-local event-struct))))
  (when-let ((buffer (map-elt (leman-room-local room) 'buffer))
             ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when-let ((nodes (leman-room--ewoc-last-matching
                         leman-ewoc
                         (lambda (data)
                           (and (leman-event-p data)
                                (equal (leman-event-id data)
                                       (leman-event-id event-struct)))))))
        (with-silent-modifications
          (ewoc-invalidate leman-ewoc nodes)))))
  ;; The event was already run through `leman-event-hook' while it
  ;; was undecrypted (when it was ignored by, e.g., notifications).
  ;; Notify again for recent events, which decrypt late only because
  ;; their key arrived after they did.
  (when (and (leman-event-origin-server-ts event-struct)
             (> (leman-event-origin-server-ts event-struct)
                (- (* 1000 (float-time))
                   (* 1000 leman-e2ee-decrypt-notify-window))))
    (leman-notify event-struct room session)))

(defun leman-e2ee--retry-decryption (session)
  "Retry decryption of SESSION's stored undecryptable events.
Events that failed to decrypt (their keys had not arrived yet) are
kept with their raw form; when a retry succeeds, the stored event
struct is updated in place and refreshed in its room's buffer.
Return (DECRYPTED . PENDING): the number of newly decrypted
events, and the number that are still undecryptable (nil when no
agent is running)."
  (when-let ((agent (leman-session-e2ee session)))
    (let ((count 0) (pending 0))
      (dolist (room (leman-session-rooms session))
        (dolist (event-struct (append (leman-room-timeline room)
                                      (leman-room-state room)))
          (when-let ((raw (and (equal (leman-event-type event-struct) "m.room.encrypted")
                               (alist-get 'encrypted-raw
                                          (leman-event-local event-struct)))))
            (cl-incf pending)
            (let ((decrypted (leman-e2ee--decrypt-event session raw (leman-room-id room))))
              (unless (equal (alist-get 'type decrypted) "m.room.encrypted")
                (leman-e2ee--update-decrypted-event event-struct decrypted session room)
                (cl-incf count))))))
      (cons count pending))))

(defun leman-e2ee--sync-changes (session data)
  "Send the E2EE parts of the sync DATA to SESSION's agent.
This must be called before the sync's next-batch token is
persisted (to-device events are ephemeral; persisting the token
first could lose room keys).  Afterwards, the agent's outgoing
requests are performed."
  (when-let ((agent (leman-session-e2ee session)))
    (condition-case err
        (let ((response (leman-e2ee-receive-sync-changes
                         agent
                         (alist-get 'events (alist-get 'to_device data))
                         (or (alist-get 'device_lists data) (list))
                         (or (alist-get 'device_one_time_keys_count data) (list))
                         (alist-get 'device_unused_fallback_key_types data)
                         (alist-get 'next_batch data))))
          (when (seq-find (lambda (event)
                            (let ((type (or (alist-get 'type event) "")))
                              (or (string-prefix-p "m.room_key" type)
                                  (string-prefix-p "m.forwarded_room_key" type))))
                          (alist-get 'to_device_events response))
            ;; Room keys arrived: let the sync callback retry the
            ;; decryption of events that failed before they arrived.
            (setf leman-e2ee--room-keys-arrived-p t)))
      (leman-e2ee-error
       (leman-message "Leman E2EE: processing sync changes failed: %S" (cdr err))))
    (leman-e2ee--process-outgoing-requests session)
    (leman-e2ee--announce-requests session agent)
    (leman-e2ee--backup-pump session agent)))

(defun leman-e2ee--announce-requests (session agent)
  "Tell the user about incoming verification requests for SESSION.
Each new incoming request (not started by us) is announced once,
in the echo area; SESSION remembers the announced flow IDs."
  (let ((announced (leman-session-e2ee-announced-requests session)))
    (dolist (request (append (ignore-errors
                               (leman-e2ee-verification-requests agent))
                             nil))
      (let ((flow-id (alist-get 'flow_id request)))
        (when (and (not (alist-get 'we_started request))
                   (equal (alist-get 'state request) "created")
                   (not (member flow-id announced)))
          (leman-message "Leman E2EE: %s wants to verify device %s (run M-x leman-e2ee-verify)"
                         (alist-get 'user_id request)
                         (alist-get 'device_id request))
          (push flow-id announced))))
    (setf (leman-session-e2ee-announced-requests session) announced)))

(defun leman-e2ee--process-outgoing-requests (session)
  "Perform the E2EE agent's outgoing requests for SESSION.
Asynchronous; used from the sync path (the responses are reported
to the agent as they arrive)."
  (when-let* ((agent (leman-session-e2ee session))
              (requests (leman-e2ee-outgoing-requests agent)))
    (cl-loop for request across requests
             do (leman-e2ee--perform-outgoing-request session agent request))))

(defun leman-e2ee--perform-outgoing-request-sync (session agent request)
  "Perform the agent's outgoing REQUEST synchronously.
Return non-nil when the response was reported to the agent."
  (pcase-let* (((map ('id id) ('method method) ('path path) ('body body)) request)
               (`(,version ,endpoint) (leman-e2ee--split-path path))
               (method (intern (downcase method))))
    ;; NOTE: The body is a pre-encoded JSON string from the agent;
    ;; pass it through verbatim.
    (condition-case err
        (let ((data (leman-e2ee--api-sync session endpoint
                                          :method method
                                          :version version
                                          :data body)))
          (leman-e2ee-mark-request-as-sent agent id data)
          t)
      (plz-error
       (leman-message "Leman E2EE: request %s failed: %S" endpoint (cdr err))
       nil))))

(defun leman-e2ee--process-outgoing-requests-sync (session)
  "Perform the E2EE agent's outgoing requests for SESSION, synchronously.
Used by the send path, which must complete key claims and room key
shares before it can retry encryption.  Returns non-nil if all
pending requests were performed and reported."
  (when-let ((agent (leman-session-e2ee session)))
    (catch 'stuck
      (cl-loop for iteration from 1 upto 10
               for requests = (leman-e2ee-outgoing-requests agent)
               while (and requests (> (length requests) 0))
               do (cl-loop for request across requests
                           unless (leman-e2ee--perform-outgoing-request-sync
                                   session agent request)
                           do (throw 'stuck nil))
               finally return t))))

(defun leman-e2ee--perform-outgoing-request (session agent request)
  "Perform the agent's outgoing REQUEST on SESSION's homeserver.
When it succeeds, report the response to the agent."
  (pcase-let* (((map ('id id) ('method method) ('path path) ('body body)) request)
               (`(,version ,endpoint) (leman-e2ee--split-path path))
               (method (intern (downcase method))))
    ;; NOTE: The body is a pre-encoded JSON string from the agent;
    ;; pass it through verbatim (re-encoding it through elisp would
    ;; corrupt empty objects, which cannot be represented in elisp).
    (leman-api session endpoint
               :method method
               :version version
               :data body
               :then (lambda (data)
                       (condition-case err
                           (leman-e2ee-mark-request-as-sent agent id data)
                         (leman-e2ee-error
                          (leman-message "Leman E2EE: marking request as sent failed: %S"
                                         (cdr err)))))
                :else (lambda (plz-error)
                        (leman-message "Leman E2EE: request %s failed: %S"
                                       endpoint plz-error)))))

(defun leman-e2ee--account-data-endpoint (session type)
  "Return the account-data endpoint of TYPE on SESSION."
  (format "user/%s/account_data/%s"
          (url-hexify-string (leman-user-id (leman-session-user session)))
          (url-hexify-string type)))

(defvar leman-e2ee--backup-stale-warned-p nil
  "Non-nil when the user was already warned about a stale backup
version (the server rejected an upload because another client
created a newer backup version), so the warning is shown once
per streak, not on every pump retry.")

(defun leman-e2ee--backup-stale-version-p (plz-error)
  "Return non-nil when PLZ-ERROR is a stale backup version rejection.
That is M_INVALID_PARAM (a newer backup version was created
elsewhere, e.g. by Element) or M_NOT_FOUND (the version was
deleted); either way the agent's version no longer matches the
homeserver's current one."
  (pcase-let (((cl-struct plz-error response) plz-error))
    (when (plz-response-p response)
      (let* ((body (ignore-errors
                     (json-read-from-string (plz-response-body response))))
             (errcode (alist-get 'errcode body)))
        (or (and (= (plz-response-status response) 400)
                 (equal errcode "M_INVALID_PARAM"))
            (and (= (plz-response-status response) 404)
                 (equal errcode "M_NOT_FOUND")))))))

(defun leman-e2ee--backup-pump (session agent)
  "Back up SESSION's room keys that are not backed up yet.
Asynchronously drain the agent's pending backup requests,
performing one at a time until there is nothing left to back up."
  (condition-case err
      (let ((request (leman-e2ee-backup-room-keys agent)))
        (when request
          (pcase-let* (((map ('id id) ('path path) ('params params) ('body body)) request)
                       (`(,version ,endpoint) (leman-e2ee--split-path path)))
            (leman-api session endpoint
                       :method 'put
                       :version version
                       :params params
                       :data body
                       :then (lambda (_data)
                               (setq leman-e2ee--backup-stale-warned-p nil)
                               (leman-e2ee-backup-mark-as-sent agent id)
                               (leman-e2ee--backup-pump session agent))
                       :else (lambda (plz-error)
                               (if (leman-e2ee--backup-stale-version-p plz-error)
                                   (unless leman-e2ee--backup-stale-warned-p
                                     (setq leman-e2ee--backup-stale-warned-p t)
                                     (leman-message
                                      "Leman E2EE: the server's key backup changed elsewhere; run M-x leman-e2ee-setup-backup to adopt the current version"))
                                 (leman-message "Leman E2EE: backing up room keys failed: %S"
                                                plz-error)))))))
    (leman-e2ee-error
     (leman-message "Leman E2EE: backing up room keys failed: %S" (cdr err)))))

(defun leman-e2ee--api-sync (session endpoint &rest args)
  "Perform a synchronous API request to ENDPOINT on SESSION.
ARGS are passed to `leman-api'.  Signal `plz-error' when the
request fails: with `:then' sync, plz replaces `:else' with a
handler that only stores the error, so a failed request is
returned as a `plz-error' struct instead of being signaled; the
error must be detected here (silently dropped requests have left
e.g. the m.megolm_backup.v1 secret pointing to a stale key)."
  (let ((result (apply #'leman-api session endpoint :then 'sync args)))
    (when (plz-error-p result)
      (signal 'plz-error (list result)))
    result))

(defun leman-e2ee--account-data-get (session type)
  "Return the global account data of TYPE on SESSION, synchronously.
Signal `user-error' when it does not exist."
  (condition-case err
      (leman-e2ee--api-sync session (leman-e2ee--account-data-endpoint session type)
                            :version "v3")
    (plz-error (user-error "Leman E2EE: no %s configured (%S)" type (cdr err)))))

(defun leman-e2ee--account-data-put (session type data)
  "Store the global account data DATA under TYPE on SESSION.
Signal `plz-error' when the request fails."
  (leman-e2ee--api-sync session (leman-e2ee--account-data-endpoint session type)
                        :method 'put
                        :version "v3"
                        :data (json-encode data))
  nil)

(defun leman-e2ee--backup-version-info (session)
  "Return the homeserver's current key backup version info for SESSION.
An alist with ~version~, ~algorithm~, and ~auth_data~ keys; signal
`user-error' when no backup exists."
  (let ((result (leman-api session "room_keys/version" :version "v3" :then 'sync)))
    (if (not (plz-error-p result))
        result
      (let ((status (when-let ((response (plz-error-response result)))
                      (plz-response-status response))))
        (if (equal status 404)
            (user-error "Leman E2EE: no key backup exists on the homeserver")
          (signal 'plz-error (list result)))))))

(defun leman-e2ee--unlock-backup-secret (session agent recovery-key encrypted default-key-id)
  "Unlock the m.megolm_backup.v1 secret's ENCRYPTED entries.
Try each entry (the default secret-storage key's first) with
RECOVERY-KEY: when it unlocks the entry's key's content, decrypt
the entry and check the backup decryption key against the
homeserver's current backup version; return it only when it
matches (entries holding an older version's key are skipped and
noted in the error).  Signal `user-error' when no entry qualifies;
DEFAULT-KEY-ID is the account's m.secret_storage.default_key's
key (may be nil)."
  (unless encrypted
    (user-error "Leman E2EE: the m.megolm_backup.v1 secret has no encrypted entries"))
  (let* ((entries (mapcar (lambda (entry) (cons (format "%s" (car entry)) (cdr entry)))
                          encrypted))
         (default (seq-find (lambda (entry)
                              (equal (car entry) default-key-id))
                            entries))
         (ordered (if default
                      (cons default (delete default entries))
                    entries))
         (reasons nil)
         (version-info (ignore-errors (leman-e2ee--backup-version-info session))))
    (when version-info
      (let ((backup-info `((algorithm . ,(alist-get 'algorithm version-info))
                           (auth_data . ,(alist-get 'auth_data version-info)))))
        (catch 'unlocked
          (dolist (entry ordered)
            (let* ((key-id (car entry))
                   (data (cdr entry))
                   (key-content (ignore-errors
                                  (leman-e2ee--account-data-get
                                   session (format "m.secret_storage.key.%s" key-id)))))
              (condition-case err
                  (progn
                    (unless key-content
                      (signal 'leman-e2ee-error
                              (list (format "secret-storage key %s is not stored on the account"
                                            key-id))))
                    (unless (leman-e2ee-ssss-check-key agent key-id recovery-key key-content)
                      (signal 'leman-e2ee-error
                              (list (format "the recovery key does not unlock secret-storage key %s"
                                            key-id))))
                    (let ((backup-recovery
                           (decode-coding-string
                            (base64-decode-string
                             (leman-e2ee-ssss-decrypt-secret
                              agent key-id recovery-key key-content
                              "m.megolm_backup.v1"
                              (alist-get 'iv data)
                              (alist-get 'ciphertext data)
                              (alist-get 'mac data)))
                            'utf-8)))
                      (if (equal (leman-e2ee-backup-verify
                                  agent backup-recovery backup-info)
                                 t)
                          (throw 'unlocked backup-recovery)
                        (push (format "the backup key stored under %s is for another backup version"
                                      key-id)
                              reasons))))
                (leman-e2ee-error (push (cadr err) reasons)))))
          (user-error
           "no entry of the m.megolm_backup.v1 secret holds the current backup version's key (%s)"
           (string-join (nreverse reasons) "; ")))))))

(defun leman-e2ee--enable-backup-and-import (session agent backup-recovery)
  "Enable AGENT's backup for BACKUP-RECOVERY and import its keys.
BACKUP-RECOVERY is the backup's decryption key (base58); the
backup version's info comes from the homeserver."
  (let* ((version-info (leman-e2ee--backup-version-info session))
         (version (alist-get 'version version-info))
         (backup-info `((algorithm . ,(alist-get 'algorithm version-info))
                        (auth_data . ,(alist-get 'auth_data version-info)))))
    (unless (equal (leman-e2ee-backup-verify agent backup-recovery backup-info) t)
      (user-error "Leman E2EE: the backup's decryption key does not match the current backup version"))
    (leman-e2ee-backup-enable agent backup-recovery version)
    (let* ((downloaded (leman-e2ee--api-sync session "room_keys/keys"
                                             :version "v3"
                                             :params (list (list "version" version))))
           (result (leman-e2ee-backup-import
                    agent backup-recovery (alist-get 'rooms downloaded))))
      (leman-message
       "Leman E2EE: restored %s of %s room keys from backup"
       (alist-get 'imported result) (alist-get 'total result))
      ;; The restored keys may decrypt events that were already
      ;; fetched and shown as undecryptable.
      (leman-e2ee--retry-decryption session))))

(defun leman-e2ee--re-store-backup-secret (session agent recovery-key backup-recovery)
  "Store BACKUP-RECOVERY under SESSION's default secret-storage key.
Add the entry when the m.megolm_backup.v1 secret has none for the
current default key, and replace the entry when it holds another
backup version's key (e.g. after another client rotated the
backup without re-storing the new key; other clients expect the
secret under the default key).  The default key is unlocked with
RECOVERY-KEY; when that fails, ask for the default key's recovery
key (empty answer skips)."
  (let* ((default-key-id (ignore-errors
                            (alist-get 'key
                                       (leman-e2ee--account-data-get
                                        session "m.secret_storage.default_key"))))
         (stored (ignore-errors
                   (alist-get 'encrypted
                              (leman-e2ee--account-data-get
                               session "m.secret_storage.secret.m.megolm_backup.v1"))))
         ;; json-read returns symbol keys; normalize to strings so
         ;; the lookup and replacement below work on real data.
         (stored (mapcar (lambda (entry)
                           (cons (format "%s" (car entry)) (cdr entry)))
                         stored))
         (entry (when default-key-id
                  (alist-get default-key-id stored nil nil #'equal)))
         (content (when default-key-id
                    (ignore-errors
                     (leman-e2ee--account-data-get
                      session (format "m.secret_storage.key.%s" default-key-id)))))
         ;; An entry that cannot be verified with the entered key
         ;; (a different secret-storage key's) is left alone.
         (needs-store
          (and default-key-id content
               (or (null entry)
                   (when (leman-e2ee-ssss-check-key agent default-key-id recovery-key content)
                     (when-let* ((version-info (ignore-errors
                                                (leman-e2ee--backup-version-info session)))
                                 (backup-info `((algorithm . ,(alist-get 'algorithm version-info))
                                                (auth_data . ,(alist-get 'auth_data version-info)))))
                       (let ((decrypted (condition-case _
                                            (decode-coding-string
                                             (base64-decode-string
                                              (leman-e2ee-ssss-decrypt-secret
                                               agent default-key-id recovery-key content
                                               "m.megolm_backup.v1"
                                               (alist-get 'iv entry)
                                               (alist-get 'ciphertext entry)
                                               (alist-get 'mac entry)))
                                             'utf-8)
                                          (leman-e2ee-error nil))))
                         (not (and decrypted
                                   (equal (leman-e2ee-backup-verify
                                           agent decrypted backup-info)
                                          t)))))))))
         (default-recovery
           (when needs-store
             (if (leman-e2ee-ssss-check-key agent default-key-id recovery-key content)
                 recovery-key
               (read-string
                (format "The backup key is not stored under the current default key %s; enter that key's recovery key to make it available to other clients (empty to skip): "
                        default-key-id))))))
    (when (and needs-store
               (> (length default-recovery) 0)
               (leman-e2ee-ssss-check-key agent default-key-id default-recovery content))
      (let ((encrypted (leman-e2ee-ssss-encrypt-secret
                        agent default-key-id default-recovery content
                        "m.megolm_backup.v1"
                        (base64-encode-string backup-recovery t))))
        (leman-e2ee--account-data-put
         session "m.secret_storage.secret.m.megolm_backup.v1"
         `((encrypted . ,(append (assoc-delete-all default-key-id stored #'equal)
                                 (list (cons default-key-id encrypted))))))
        (leman-message
         "Leman E2EE: stored the backup key under the current default secret-storage key")))))

(defun leman-e2ee--restore-backup (session agent &optional adopt)
  "Unlock and adopt the server-side key backup for SESSION.
Ask for a secret-storage recovery key; unlock the backup's
decryption key from secret storage (trying every stored
secret-storage key), or accept the backup's own recovery key
directly when no stored key matches.  Enable the backup, import
its room keys, and back up new keys afterwards (the pump).  With
ADOPT non-nil the messages speak of adopting an existing setup
(the command is `leman-e2ee-setup-backup' on an account that
already has one, e.g. from Element)."
  (let* ((secret (ignore-errors
                   (leman-e2ee--account-data-get
                    session "m.secret_storage.secret.m.megolm_backup.v1")))
         (encrypted (alist-get 'encrypted secret))
         (default-key-id (ignore-errors
                           (alist-get 'key
                                      (leman-e2ee--account-data-get
                                       session "m.secret_storage.default_key"))))
         (recovery-key
          (read-string
           (if default-key-id
               (format "Secret storage recovery key (the account's recovery key, e.g. Element's \"Recovery key\", for secret-storage key %s): "
                       default-key-id)
             "Secret storage recovery key: ")))
         (reasons nil)
         (backup-recovery
          (or (condition-case err
                  (leman-e2ee--unlock-backup-secret
                   session agent recovery-key encrypted default-key-id)
                (user-error (setq reasons (cons (apply #'format (cdr err)) reasons)) nil))
              ;; Fall back to the backup's own recovery key (Es...).
              (condition-case err
                  (let* ((version-info (leman-e2ee--backup-version-info session))
                         (backup-info `((algorithm . ,(alist-get 'algorithm version-info))
                                        (auth_data . ,(alist-get 'auth_data version-info)))))
                    (unless (equal (leman-e2ee-backup-verify
                                    agent recovery-key backup-info)
                                   t)
                      (user-error "the recovery key does not match the current backup version"))
                    recovery-key)
                (user-error (setq reasons (cons (apply #'format (cdr err)) reasons)) nil)
                (leman-e2ee-error (setq reasons (cons (cadr err) reasons)) nil)
                (plz-error (setq reasons (cons "no key backup exists on the homeserver" reasons)) nil)))))
    (cond
     (backup-recovery
      (leman-e2ee--enable-backup-and-import session agent backup-recovery)
      (leman-e2ee--re-store-backup-secret session agent recovery-key backup-recovery)
      (leman-e2ee--backup-pump session agent)
      (leman-message
       (if adopt
           "Leman E2EE: adopted the existing key backup"
         "Leman E2EE: keys restored from the key backup")))
     ;; Both unlock attempts failed, but the agent already holds the
     ;; current backup's key (it created or adopted the current
     ;; version before) and the entered key unlocks the default key:
     ;; re-store the backup key under the default key (healing a
     ;; stale secret, e.g. after another client rotated the backup)
     ;; and restore from the backup; no new version is needed.
     ((and default-key-id
           (let ((saved (ignore-errors (leman-e2ee-backup-recovery-key agent))))
             (when (and saved (not (equal saved "")))
               (let* ((version-info (ignore-errors
                                     (leman-e2ee--backup-version-info session)))
                      (backup-info (when version-info
                                     `((algorithm . ,(alist-get 'algorithm version-info))
                                       (auth_data . ,(alist-get 'auth_data version-info))))))
                 (and backup-info
                      (equal (leman-e2ee-backup-verify agent saved backup-info) t)
                      (let ((content (ignore-errors
                                      (leman-e2ee--account-data-get
                                       session (format "m.secret_storage.key.%s" default-key-id)))))
                        (and content
                             (leman-e2ee-ssss-check-key
                              agent default-key-id recovery-key content))))))))
      (let ((saved (leman-e2ee-backup-recovery-key agent)))
        (leman-e2ee--enable-backup-and-import session agent saved)
        (leman-e2ee--re-store-backup-secret session agent recovery-key saved)
        (leman-e2ee--backup-pump session agent))
      (leman-message
       "Leman E2EE: the backup key is stored under the default secret-storage key again; keys restored"))
     ;; Both unlock attempts failed.  When adopting (the account has
     ;; a default key) and the entered key DOES unlock the default
     ;; key, only the backup version's key is missing from secret
     ;; storage: offer a fresh version that keeps the recovery key.
     ((and adopt default-key-id
           (let ((content (ignore-errors
                            (leman-e2ee--account-data-get
                             session (format "m.secret_storage.key.%s" default-key-id)))))
             (and content
                  (leman-e2ee-ssss-check-key agent default-key-id recovery-key content))))
      (when (y-or-n-p
             "The current backup's key is not in secret storage.  Create a new backup version (your recovery key keeps working)? ")
        (leman-e2ee--create-backup-version session agent default-key-id recovery-key)))
     (t
      (user-error "Leman E2EE: %s (see M-x leman-e2ee-backup-dump for the stored state)"
                  (string-join (nreverse reasons) "; "))))))

(defun leman-e2ee--delete-backup-version (session version)
  "Delete the backup VERSION on SESSION's homeserver."
  (leman-e2ee--api-sync session (format "room_keys/version/%s" version)
                        :method 'delete
                        :version "v3"))

(defun leman-e2ee--ensure-version-current (session version)
  "Make the homeserver report VERSION as its current backup version.
Some homeservers (conduit, conduwuit and continuwuity before the
numeric-latest fix) derive the current version by comparing
version ids as strings; a shorter, older id like \"69562\" then
sorts after a newly created \"12151043\" and stays current
forever, and every upload to the new version is rejected.  With
the user's consent, delete such stale versions (the clients that
hold their room keys re-upload them to the new version); signal
`user-error' when the server keeps refusing."
  (catch 'done
    (dotimes (_ 4)
      (let ((current (condition-case err
                         (leman-e2ee--backup-version-info session)
                       ;; No version at all: ours is trivially current.
                       (user-error (throw 'done t))
                       (plz-error (signal (car err) (cdr err))))))
        (if (equal (alist-get 'version current) version)
            (throw 'done t)
          (let ((stale (alist-get 'version current)))
            (unless (y-or-n-p
                     (format "The homeserver keeps version %s current instead of the newly created %s (its version-id comparison is lexicographic; a known server bug).  Delete the stale version %s?  The clients holding its room keys will re-upload them to the new version. "
                             stale version stale))
              (user-error "Leman E2EE: the homeserver keeps backup version %s current instead of the new %s"
                          stale version))
            (leman-e2ee--delete-backup-version session stale)))))
    (user-error "Leman E2EE: the homeserver never made the new backup version %s current" version)))

(defun leman-e2ee--create-backup-version (session agent default-key-id default-recovery)
  "Create a fresh backup version keeping the existing default key.
The new backup's decryption key is stored under the account's
current default secret-storage key DEFAULT-KEY-ID (whose recovery
key DEFAULT-RECOVERY the user entered), so other clients keep
working.  Backs up the room keys afterwards."
  (let* ((created (leman-e2ee-backup-create agent))
         (recovery-key (alist-get 'recovery_key created))
         (existing-version (ignore-errors
                             (leman-e2ee--backup-version-info session)))
         (version (alist-get 'version
                             (leman-e2ee--api-sync session "room_keys/version"
                                                   :method 'post
                                                   :version "v3"
                                                   :data (json-encode
                                                          `((algorithm . ,(alist-get 'algorithm created))
                                                            (auth_data . ,(alist-get 'auth_data created)))))))
         (content (leman-e2ee--account-data-get
                   session (format "m.secret_storage.key.%s" default-key-id)))
         (encrypted (leman-e2ee-ssss-encrypt-secret
                     agent default-key-id default-recovery content
                     "m.megolm_backup.v1"
                     (base64-encode-string recovery-key t)))
         (stored (ignore-errors
                   (alist-get 'encrypted
                              (leman-e2ee--account-data-get
                               session "m.secret_storage.secret.m.megolm_backup.v1"))))
         ;; json-read returns symbol keys; normalize to strings so
         ;; that the stale entry for DEFAULT-KEY-ID is replaced
         ;; rather than duplicated (duplicate JSON object keys make
         ;; the stored value depend on the server's parser).
         (stored (mapcar (lambda (entry)
                           (cons (format "%s" (car entry)) (cdr entry)))
                         stored)))
    ;; Trust nothing: verify that the homeserver actually made the
    ;; new version current before pointing anything at it (some
    ;; servers returned the new version id while keeping an older
    ;; version current, which made the fresh key useless).
    (leman-e2ee--ensure-version-current session version)
    (leman-e2ee-backup-enable agent recovery-key version)
    (leman-e2ee--account-data-put
     session "m.secret_storage.secret.m.megolm_backup.v1"
     `((encrypted . ,(append (assoc-delete-all default-key-id stored #'equal)
                             (list (cons default-key-id encrypted))))))
    (leman-e2ee--backup-pump session agent)
    (leman-message
     "Leman E2EE: created backup version %s%s; room keys are being backed up"
     version
     (if existing-version
         (format " (replacing %s)" (alist-get 'version existing-version))
       ""))))

(defun leman-e2ee-setup-backup (session)
  "Set up key backup and secret storage for SESSION.
When the account already has a secret-storage default key (e.g.
set up in another client like Element), ask for that recovery key
and adopt the existing setup: unlock the backup's decryption key
from secret storage, enable the backup, and import its keys.
Otherwise create a fresh backup version and secret-storage
default key and display both recovery keys to save."
  (interactive (list (leman-complete-session)))
  (let ((agent (leman-session-e2ee session)))
    (unless agent
      (user-error "Leman E2EE: no agent running (try reconnecting)"))
    (condition-case err
        (let ((default-key-id (ignore-errors
                                (alist-get 'key
                                           (leman-e2ee--account-data-get
                                            session "m.secret_storage.default_key")))))
          (if default-key-id
              (leman-e2ee--restore-backup session agent 'adopt)
            (leman-e2ee--create-backup session agent)))
      (leman-e2ee-error
       (user-error "Leman E2EE: setting up key backup failed: %s" (cdr err)))
      (plz-error
       (user-error "Leman E2EE: setting up key backup failed: %S" (cdr err))))))

(defun leman-e2ee--create-backup (session agent)
  "Create a fresh backup and secret-storage default key for SESSION."
  (let* ((created (leman-e2ee-backup-create agent))
         (recovery-key (alist-get 'recovery_key created))
         (existing-version (ignore-errors
                             (leman-e2ee--backup-version-info session)))
         (version (alist-get 'version
                             (leman-e2ee--api-sync session "room_keys/version"
                                                   :method 'post
                                                   :version "v3"
                                                   :data (json-encode
                                                          `((algorithm . ,(alist-get 'algorithm created))
                                                            (auth_data . ,(alist-get 'auth_data created)))))))
         (ssss (leman-e2ee-ssss-create agent))
         (key-id (alist-get 'key_id ssss))
         (ssss-recovery (alist-get 'recovery_key ssss))
         (content (alist-get 'content ssss))
         (encrypted (leman-e2ee-ssss-encrypt-secret
                     agent key-id ssss-recovery content
                     "m.megolm_backup.v1"
                     (base64-encode-string recovery-key t))))
    (when existing-version
      (leman-message
       "Leman E2EE: replacing existing backup version %s (its backed-up keys stay in that version)"
       (alist-get 'version existing-version)))
    (leman-e2ee--ensure-version-current session version)
    (leman-e2ee-backup-enable agent recovery-key version)
    (leman-e2ee--account-data-put
     session (format "m.secret_storage.key.%s" key-id) content)
    (leman-e2ee--account-data-put
     session "m.secret_storage.default_key" `((key . ,key-id)))
    (leman-e2ee--account-data-put
     session "m.secret_storage.secret.m.megolm_backup.v1"
     `((encrypted . ((,key-id . ,encrypted)))))
    (leman-e2ee--backup-pump session agent)
    (with-output-to-temp-buffer "*Leman key backup*"
      (princ (format "Key backup is set up.  Save these recovery keys somewhere safe
\(e.g. a password manager); they cannot be shown again.

Secret storage recovery key (the one to enter on a new device
to restore old messages):

    %s

Backup recovery key (also stored in secret storage):

    %s

The room keys are being backed up in the background.
"
                   (leman-e2ee--format-recovery-key ssss-recovery)
                   (leman-e2ee--format-recovery-key recovery-key))))))

(defun leman-e2ee--restore-with-agent-key (session agent)
  "Restore SESSION's keys with AGENT's saved backup decryption key.
When the saved key matches the homeserver's current backup
version, enable the backup and import its room keys, and return
non-nil.  Return nil (without side effects) when the agent has no
saved key or it does not match, so the caller can fall back to
the secret-storage flow.  This also restores history on accounts
whose m.megolm_backup.v1 secret is stale (e.g. another client
rotated the backup without re-storing the new key)."
  (when-let ((saved (ignore-errors (leman-e2ee-backup-recovery-key agent)))
             ((not (equal saved ""))))
    (condition-case _
        (progn
          (leman-e2ee--enable-backup-and-import session agent saved)
          (leman-e2ee--backup-pump session agent)
          t)
      (user-error nil)
      (leman-e2ee-error nil)
      (plz-error nil))))

(defun leman-e2ee-restore-keys (session)
  "Restore SESSION's message history from the server-side key backup.
When the agent already holds the current backup's decryption key
(e.g. it set the backup up, or adopted it before), restore
directly with it.  Otherwise ask for the secret-storage recovery
key, unlock the backup's decryption key stored in secret storage,
enable the backup, and import its room keys.  Every secret-storage
key stored on the account is tried, so the recovery key of any of
them works (e.g. after the default key was changed in another
client)."
  (interactive (list (leman-complete-session)))
  (let ((agent (leman-session-e2ee session)))
    (unless agent
      (user-error "Leman E2EE: no agent running (try reconnecting)"))
    (or (leman-e2ee--restore-with-agent-key session agent)
        (condition-case err
            (progn
              (leman-e2ee--restore-backup session agent)
              t)
          (leman-e2ee-error
           (user-error "Leman E2EE: restoring keys failed: %s" (cdr err)))
          (plz-error
           (user-error "Leman E2EE: restoring keys failed: %S" (cdr err)))))))

(defun leman-e2ee-backup-info (session)
  "Display SESSION's key backup status."
  (interactive (list (leman-complete-session)))
  (let ((agent (leman-session-e2ee session)))
    (unless agent
      (user-error "Leman E2EE: no agent running (try reconnecting)"))
    (let ((status (leman-e2ee-backup-status agent))
          (counts (alist-get 'room_key_counts (leman-e2ee-backup-status agent))))
      (if (alist-get 'enabled status)
          (leman-message
           "Leman E2EE: key backup enabled (version %s): %s of %s room keys backed up"
           (alist-get 'version status)
           (alist-get 'backed_up counts)
           (alist-get 'total counts))
        (leman-message "Leman E2EE: key backup is not enabled (try M-x leman-e2ee-setup-backup)")))))

(defun leman-e2ee-publish-backup-key (session)
  "Store SESSION's backup decryption key under the default key.
For when the backup is already enabled (leman holds its
decryption key) but the m.megolm_backup.v1 secret has no entry
for the account's current default secret-storage key (e.g. after
another client changed the recovery key): asks for that key's
recovery key and adds the encrypted entry, so other clients can
unlock the backup again."
  (interactive (list (leman-complete-session)))
  (let ((agent (leman-session-e2ee session)))
    (unless agent
      (user-error "Leman E2EE: no agent running (try reconnecting)"))
    (condition-case err
        (let ((backup-recovery (leman-e2ee-backup-recovery-key agent)))
          (unless backup-recovery
            (user-error "Leman E2EE: the agent has no backup decryption key (restore the backup first)"))
          (leman-e2ee--re-store-backup-secret session agent "" backup-recovery)
          (leman-message
           "Leman E2EE: backup key published under the current default secret-storage key (or it was already stored)"))
      (leman-e2ee-error
       (user-error "Leman E2EE: publishing the backup key failed: %s" (cdr err)))
      (plz-error
       (user-error "Leman E2EE: publishing the backup key failed: %S" (cdr err))))))

(defun leman-e2ee-import-keys (session)
  "Import room keys from a key-export file into SESSION's agent.
The file uses the same format as Element's \"Export E2E room
keys\" (Settings -> Security & Privacy -> Encryption).  This
restores the ability to read old messages without touching any
recovery keys or backups; imported keys are backed up once a
backup is enabled."
  (interactive (list (leman-complete-session)))
  (let ((agent (leman-session-e2ee session)))
    (unless agent
      (user-error "Leman E2EE: no agent running (try reconnecting)"))
    (let* ((file (read-file-name "Key export file: " nil nil t))
           (content (with-temp-buffer
                      (insert-file-contents-literally file)
                      (buffer-string)))
           (passphrase (read-passwd "Passphrase of the key export: ")))
      (condition-case err
          (let ((result (leman-e2ee--import-keys agent content passphrase)))
            (leman-message "Leman E2EE: imported %s of %s room keys from %s"
                           (alist-get 'imported result)
                           (alist-get 'total result)
                           (file-name-nondirectory file))
            ;; The session's keys may decrypt events that were already
            ;; fetched and shown as undecryptable.
            (leman-e2ee--retry-decryption session))
        (leman-e2ee-error
         (user-error "Leman E2EE: importing keys failed: %s
(wrong passphrase or not a key-export file?)" (cdr err)))))))

(defun leman-e2ee-export-keys (session)
  "Export every room key SESSION's agent holds to a file.
The file uses the same format as Element's \"Export E2E room
keys\" and can be imported by other clients (or by leman with
`leman-e2ee-import-keys')."
  (interactive (list (leman-complete-session)))
  (let ((agent (leman-session-e2ee session)))
    (unless agent
      (user-error "Leman E2EE: no agent running (try reconnecting)"))
    (let ((passphrase (read-passwd "Passphrase for the key export: ")))
      (unless (> (length passphrase) 0)
        (user-error "Leman E2EE: an empty passphrase is not allowed"))
      (let ((confirmed (read-passwd "Confirm passphrase: ")))
        (unless (equal passphrase confirmed)
          (user-error "Leman E2EE: the passphrases do not match"))
        (condition-case err
            (let* ((keys (leman-e2ee--export-keys agent passphrase))
                   (file (read-file-name "Export keys to: " nil "~/leman-room-keys.txt")))
              (with-temp-file file
                (insert keys))
              (leman-message "Leman E2EE: exported room keys to %s" file))
          (leman-e2ee-error
           (user-error "Leman E2EE: exporting keys failed: %s" (cdr err))))))))

(defun leman-e2ee-backup-dump--key (session key-id)
  "Describe the secret-storage key KEY-ID of SESSION in the dump."
  (let ((content (ignore-errors
                   (leman-e2ee--account-data-get
                    session (format "m.secret_storage.key.%s" key-id)))))
    (if (not content)
        (princ (format "  key %s: NOT STORED on the account (no content)\n" key-id))
      (princ (format "  key %s: algorithm %s%s%s\n"
                     key-id
                     (or (alist-get 'algorithm content) "?")
                     (if (alist-get 'passphrase content)
                         ", passphrase-based (enter the recovery key, not the passphrase)"
                       "")
                     (if (and (alist-get 'iv content) (alist-get 'mac content))
                         ", iv/mac present"
                       ", NO iv/mac (its recovery key cannot be verified)"))))))

(defun leman-e2ee-backup-dump (session)
  "Dump SESSION's key-backup and secret-storage state.
Shows the account's secret-storage keys, the encrypted entries of
the m.megolm_backup.v1 secret (one entry per key that encrypted
it), the homeserver's current backup version and its public key,
and the agent's backup status.  Non-interactive diagnostics for
untangling key-backup state after other clients changed it."
  (interactive (list (leman-complete-session)))
  (let ((agent (leman-session-e2ee session)))
    (unless agent
      (user-error "Leman E2EE: no agent running (try reconnecting)"))
    (with-output-to-temp-buffer "*Leman backup state*"
      (princ (format "Key backup state for %s\n\n" (leman-user-id (leman-session-user session))))
      (let ((default (ignore-errors
                       (leman-e2ee--account-data-get
                        session "m.secret_storage.default_key")))
            (secret (ignore-errors
                      (leman-e2ee--account-data-get
                       session "m.secret_storage.secret.m.megolm_backup.v1")))
            (default-key-id nil))
        (if (not default)
            (princ "Default secret-storage key: none\n")
          (setq default-key-id (alist-get 'key default))
          (princ (format "Default secret-storage key: %s\n" default-key-id))
          (leman-e2ee-backup-dump--key session default-key-id))
        (princ "\n")
        (if (not secret)
            (princ "m.megolm_backup.v1 secret: NOT STORED (the backup's decryption key is not in secret storage)\n")
          (let ((entries (alist-get 'encrypted secret)))
            (princ (format "m.megolm_backup.v1 secret: %d encrypted entr%s\n"
                           (length entries)
                           (if (= (length entries) 1) "y" "ies")))
            (if (null entries)
                (princ "  (no entries)\n")
              (dolist (entry entries)
                (let ((key-id (format "%s" (car entry))))
                  (princ (format "  - entry encrypted for key %s\n" key-id))
                  (leman-e2ee-backup-dump--key session key-id))))))
        (princ "\n")
        (let ((version (ignore-errors (leman-e2ee--backup-version-info session))))
          (if (not version)
              (princ "Current backup version: none on the homeserver\n")
            (princ (format "Current backup version: %s (%s)\n  backup public key: %s\n"
                           (alist-get 'version version)
                           (or (alist-get 'algorithm version) "?")
                           (alist-get 'public_key (alist-get 'auth_data version))))))
        (princ "\n")
        (let ((status (ignore-errors (leman-e2ee-backup-status agent))))
          (if (not status)
              (princ "Agent backup status: unavailable\n")
            (princ (format "Agent: backup %s (version %s), %s of %s room keys backed up\n"
                           (if (alist-get 'enabled status) "ENABLED" "disabled")
                           (or (alist-get 'version status) "-")
                           (alist-get 'backed_up (alist-get 'room_key_counts status))
                           (alist-get 'total (alist-get 'room_key_counts status))))))
        (princ "
The recovery key asked for by setup/restore is the account's
secret-storage recovery key (a ~48 character Es... string, shown
as \"Recovery key\" by Element for the default key above).  The
backup public key of the current version must match the key
inside the m.megolm_backup.v1 secret; if the current version's
public key belongs to no stored entry, that version's key is not
recoverable from secret storage and the backup must be reset (in
Element: Settings -> Encryption) before a fresh setup.\n")))))

(defun leman-e2ee--encrypt-content (session room content)
  "Encrypt CONTENT for ROOM on SESSION, for sending.
Return (CONTENT . EVENT-TYPE): the encrypted content with event
type \"m.room.encrypted\", or the original content with
\"m.room.message\" when the room is not encrypted.  Signal
`leman-e2ee-error' if the room is encrypted but encryption fails
(failing closed: never send plaintext into an encrypted room)."
  (let ((agent (leman-session-e2ee session)))
    (if (and agent (leman-room--encrypted-p room))
        (let ((room-id (leman-room-id room))
              (members (hash-table-keys (leman-room-members room))))
          ;; Track the members' devices and complete the initial
          ;; keys/query before encrypting, else the room key would be
          ;; shared with nobody.  The send path must pump
          ;; synchronously: the retries below need the claims and
          ;; shares to have been performed and reported.
          (leman-e2ee-update-tracked-users agent members)
          (leman-e2ee--process-outgoing-requests-sync session)
          ;; Encrypt, retrying while the agent still needs key claims.
          (let ((response nil))
            (cl-loop for attempt from 1 upto 3
                     do (setf response (leman-e2ee-encrypt-event
                                        agent room-id "m.room.message" content members))
                     while (equal (alist-get 'status response) "claims_pending")
                     do (leman-e2ee--process-outgoing-requests-sync session))
            (if (equal (alist-get 'status response) "ok")
                ;; Send the key shares before (or with) the event.
                (progn (leman-e2ee--process-outgoing-requests-sync session)
                       (cons (alist-get 'content (alist-get 'event response))
                             "m.room.encrypted"))
              (signal 'leman-e2ee-error
                      (list "encrypt" "unable to encrypt after retries")))))
      ;; Not encrypted (or no agent): send as usual.  If the room's
      ;; timeline contains encrypted events, the room really is
      ;; encrypted but encryption isn't active for us: warn loudly
      ;; rather than silently sending plaintext into it.
      (when (cl-find "m.room.encrypted" (leman-room-timeline room)
                     :test #'equal :key #'leman-event-type)
        (leman-message "Leman E2EE: WARNING: room %s has encrypted messages, but this message will NOT be encrypted (no agent or room state missing encryption)"
                       (leman-room-id room)))
      (cons content "m.room.message"))))

;;; Functions

(defun leman-interrupted-sync-warning (session)
  "Display a warning that syncing of SESSION was interrupted."
  (display-warning
   'leman
   (format
    (substitute-command-keys
     "\\<leman-room-mode-map>Syncing of session <%s> was interrupted.  Use command `leman-room-sync' in a room buffer to retry.")
    (leman-user-id (leman-session-user session)))
   :error))

(defun leman-interrupted-sync-message (session)
  "Display a message that syncing of SESSION was interrupted."
  (message
   (substitute-command-keys
    "\\<leman-room-mode-map>Syncing of session <%s> was interrupted.  Use command `leman-room-sync' in a room buffer to retry.")
   (leman-user-id (leman-session-user session))))

(defun leman--run-idle-timer (&rest _ignore)
  "Run idle timer that updates read receipts.
To be called from `leman-after-initial-sync-hook'.  Timer is
stored in `leman-read-receipt-idle-timer'."
  (unless (timerp leman-read-receipt-idle-timer)
    (setf leman-read-receipt-idle-timer (run-with-idle-timer 3 t #'leman-room-read-receipt-idle-timer))))

(defun leman--stop-idle-timer (&rest _ignore)
  "Stop idle timer stored in `leman-read-receipt-idle-timer'.
To be called from `leman-disconnect-hook'."
  (unless leman-sessions
    (when (timerp leman-read-receipt-idle-timer)
      (cancel-timer leman-read-receipt-idle-timer)
      (setf leman-read-receipt-idle-timer nil))))

(defun leman-view-initial-rooms (session)
  "View rooms for SESSION configured in `leman-auto-view-rooms'."
  (when-let (rooms (alist-get (leman-user-id (leman-session-user session))
			      leman-auto-view-rooms nil nil #'equal))
    (dolist (alias/id rooms)
      (when-let (room (cl-find-if (lambda (room)
				    (or (equal alias/id (leman-room-canonical-alias room))
					(equal alias/id (leman-room-id room))))
				  (leman-session-rooms session)))
        (let ((leman-view-room-display-buffer-action leman-auto-view-room-display-buffer-action))
          (leman-view-room room session))))))

(defun leman--initial-transaction-id ()
  "Return an initial transaction ID for a new session."
  ;; We generate a somewhat-random initial transaction ID to avoid
  ;; potential transaction ID conflicts between sessions and clients.
  ;; See <https://github.com/alphapapa/ement.el/issues/36>.
  (cl-parse-integer
   (secure-hash 'sha256 (prin1-to-string (list (current-time) (system-name))))
   :end 8 :radix 16))

(defsubst leman--sync-messages-p (session)
  "Return non-nil if sync-related messages should be shown for SESSION."
  ;; For now, this seems like the best way.
  (or (not (leman-session-has-synced-p session))
      (not leman-auto-sync)))

(defun leman--hostname-uri (hostname)
  "Return the \".well-known\" URI for server HOSTNAME.
If no URI is found, prompt the user for the hostname."
  ;; FIXME: When fail-prompting, a URI should be returned, not just a hostname.
  ;; SPEC: <https://matrix.org/docs/spec/client_server/r0.6.1#id178> ("4.1   Well-known URI")
  (cl-labels ((fail-prompt ()
                (let ((input (read-string "Auto-discovery of server's well-known URI failed.  Input server hostname, or leave blank to use server name: ")))
                  (pcase input
                    ("" hostname)
                    (_ input))))
              (parse (string)
                (if-let* ((object (ignore-errors (json-read-from-string string)))
                          (url (map-nested-elt object '(m.homeserver base_url)))
                          ((string-match-p
                            (rx bos "http" (optional "s") "://" (1+ nonl))
                            url)))
                    url
                  ;; Parsing error: FAIL_PROMPT.
                  (fail-prompt))))
    (condition-case err
        (let ((response (plz 'get (concat "https://" hostname "/.well-known/matrix/client")
                          :as 'response :then 'sync)))
          (if (plz-response-p response)
              (pcase (plz-response-status response)
                (200 (parse (plz-response-body response)))
                (404 (fail-prompt))
                (_ (warn "Leman: `plz' request for .well-known URI returned unexpected code: %s"
                         (plz-response-status response))
                   (fail-prompt)))
            (warn "Leman: `plz' request for .well-known URI did not return a `plz' response")
            (fail-prompt)))
      (error (warn "Leman: `plz' request for .well-known URI signaled an error: %S" err)
             (fail-prompt)))))

(defun leman--sync-maybe-interrupt (session force)
  "Interrupt any outstanding sync for SESSION.
If FORCE is nil, signal an error instead."
  (when (map-elt leman-syncs session)
    (if force
        (condition-case err
            (delete-process (map-elt leman-syncs session))
          ;; Ensure the only error is the expected one from deleting the process.
          (leman-api-error (cl-assert (equal "curl process killed" (plz-error-message (cl-third err))))
                           (message "Leman: Forcing new sync")))
      (user-error "Leman: Already syncing this session"))))

(defun leman--sync-params (next-batch filter)
  "Return query parameters for a sync request for NEXT-BATCH and FILTER."
  ;; TODO: Document filter arg.
  (remove
   nil (list (list "full_state" (if next-batch "false" "true"))
             (when filter
               (list "filter" (json-encode filter)))
             (when next-batch
               (list "since" next-batch))
             (when next-batch
               (list "timeout" "30000")))))

(defun leman--sync-failed (session timeout plz-error)
  "Handle a failed sync request for SESSION.
TIMEOUT is the request's timeout, which is used when re-syncing.
PLZ-ERROR is the error passed by `plz'."
  (setf (map-elt leman-syncs session) nil)
  ;; TODO: plz probably needs nicer error handling.
  ;; Ideally we would use `condition-case', but since the error is
  ;; signaled in `plz--sentinel'...
  (pcase-let* (((cl-struct plz-error curl-error response) plz-error)
               (reason))
    (cond ((when (leman--response-revoked-p plz-error)
             ;; The token is gone (e.g. the session was removed from
             ;; another device): don't retry, report and clean up.
             (leman--session-revoked
              session (leman--response-soft-logout-p plz-error))
             (signal 'leman-api-session-revoked
                     (list "Leman: sync stopped: session signed out")))
           (setf reason "signed out"))
          ((when response
             (pcase (plz-response-status response)
               ((or 429 502) (setf reason "failed")))))
          ((pcase curl-error
             (`(28 . ,_) (setf reason "timed out")))))
    (if reason
        (if (not leman-auto-sync)
            (run-hook-with-args 'leman-interrupted-sync-hook session)
          (message "Leman: Sync %s (%s).  Syncing again..."
                   reason (leman-user-id (leman-session-user session)))
          ;; Set QUIET to allow the just-printed message to remain visible.
          (leman--sync session :timeout timeout :quiet t))
      ;; Unrecognized errors:
      (pcase curl-error
        (`(,code . ,message)
         (signal 'leman-api-error (list (format "Leman: Network error: %s: %s" code message)
                                        plz-error)))
        (_ (signal 'leman-api-error (list "Leman: Unrecognized network error" plz-error)))))))

(defun leman--sync-read-json (session sync-start-time)
  "Print a message, then parse the sync response for SESSION.
Called in the buffer holding the response; SYNC-START-TIME is the
time the request was sent, used for progress messages."
  (when (leman--sync-messages-p session)
    (message "Leman: Response arrived after %.2f seconds.  Reading %s JSON response..."
             (- (time-to-seconds) sync-start-time)
             (file-size-human-readable (buffer-size))))
  (let ((start-time (time-to-seconds)))
    (prog1 (leman--json-parse-buffer)
      (when (leman--sync-messages-p session)
        (message "Leman: Reading JSON took %.2f seconds"
                 (- (time-to-seconds) start-time))))))

(cl-defun leman--sync (session &key force quiet
                               (timeout 40) ;; Give the server an extra 10 seconds.
                               (filter leman-default-sync-filter))
  "Send sync request for SESSION.
If SESSION has a `next-batch' token, it's used.  If FORCE, first
delete any outstanding sync processes.  If QUIET, don't show a
message about syncing this time.  Cancel request after TIMEOUT
seconds.

FILTER may be an alist representing a raw event filter (i.e. not
a filter ID).  When unspecified, the value of
`leman-default-sync-filter' is used.  The filter is encoded with
`json-encode'.  To use no filter, specify FILTER as nil."
  ;; SPEC: <https://matrix.org/docs/spec/client_server/r0.6.1#id257>.
  ;; TODO: Filtering: <https://matrix.org/docs/spec/client_server/r0.6.1#filtering>.
  ;; TODO: Use a filter ID for default filter.
  ;; TODO: Optionally, automatically sync again when HTTP request fails.
  ;; TODO: Ensure that the process in (map-elt leman-syncs session) is live.
  (leman--sync-maybe-interrupt session force)
  (pcase-let* (((cl-struct leman-session next-batch) session)
               (params (leman--sync-params next-batch filter))
               (sync-start-time (time-to-seconds))
               ;; FIXME: Auto-sync again in error handler.
               (process (leman-api session "sync" :params params
                          :timeout timeout
                          :then (apply-partially #'leman--sync-callback session)
                          :else (lambda (plz-error)
                                  (leman--sync-failed session timeout plz-error))
                          :json-read-fn (lambda ()
                                          (leman--sync-read-json session sync-start-time)))))
    (when process
      (setf (map-elt leman-syncs session) process)
      (when (and (not quiet) (leman--sync-messages-p session))
        (leman-message "Sync request sent.  Waiting for response...")))))

(defun leman--sync-callback (session data)
  "Process sync DATA for SESSION.
Runs `leman-sync-callback-hook' with SESSION."
  (leman-debug (leman-user-id (leman-session-user session)))
  ;; Remove the sync first.  We already have the data from it, and the
  ;; process has exited, so it's safe to run another one.
  (setf (map-elt leman-syncs session) nil)
  ;; Send the sync's E2EE parts to the agent (this must happen before
  ;; the next-batch token is persisted, or to-device events like room
  ;; keys can be lost), then perform its outgoing requests.
  (leman-e2ee--sync-changes session data)
  (pcase-let* (((map rooms ('next_batch next-batch) ('account_data (map ('events account-data-events))))
                data)
               ((map ('join joined-rooms) ('invite invited-rooms) ('leave left-rooms)) rooms)
               (num-events (+
                            ;; HACK: In `leman--push-joined-room-events', we do something
                            ;; with each event 3 times, so we multiply this by 3.
                            ;; FIXME: That calculation doesn't seem to be quite right, because
                            ;; the progress reporter never seems to hit 100% before it's done.
                            (* 3 (cl-loop for (_id . room) in joined-rooms
                                          sum (length (map-nested-elt room '(state events)))
                                          sum (length (map-nested-elt room '(timeline events)))))
                            (cl-loop for (_id . room) in invited-rooms
                                     sum (length (map-nested-elt room '(invite_state events)))))))
    ;; Append account data events.
    ;; TODO: Since only one event of each type is allowed in account data (the spec
    ;; doesn't seem to make this clear, but see
    ;; <https://github.com/matrix-org/matrix-js-sdk/blob/d0b964837f2820940bd93e718a2450b5f528bffc/src/store/memory.ts#L292>),
    ;; we should store account-data events in a hash table or alist rather than just a
    ;; list of events.
    (cl-callf2 append (cl-coerce account-data-events 'list) (leman-session-account-data session))
    ;; Process invited and joined rooms.
    (leman-with-progress-reporter (:when (leman--sync-messages-p session)
                                         :reporter ("Leman: Reading events..." 0 num-events))
      ;; Left rooms.
      (mapc (apply-partially #'leman--push-left-room-events session) left-rooms)
      ;; Invited rooms.
      (mapc (apply-partially #'leman--push-invite-room-events session) invited-rooms)
      ;; Joined rooms.
      (mapc (apply-partially #'leman--push-joined-room-events session) joined-rooms))
    ;; TODO: Process "left" rooms (remove room structs, etc).
    ;; NOTE: We update the next-batch token before updating any room buffers.  This means
    ;; that any errors in updating room buffers (like for unexpected event formats that
    ;; expose a bug) could cause events to not appear in the buffer, but the user could
    ;; still dismiss the error and start syncing again, and the client could remain
    ;; usable.  Updating the token after doing everything would be preferable in some
    ;; ways, but it would mean that an event that exposes a bug would be processed again
    ;; on every sync, causing the same error each time.  It would seem preferable to
    ;; maintain at least some usability rather than to keep repeating a broken behavior.
    (setf (leman-session-next-batch session) next-batch)
    ;; Run hooks which update buffers, etc.
    (run-hook-with-args 'leman-sync-callback-hook session)
    (when leman-e2ee--room-keys-arrived-p
      ;; Room keys arrived with this sync: events that could not be
      ;; decrypted before (their placeholders are already in the
      ;; buffers) may decrypt now.
      (setf leman-e2ee--room-keys-arrived-p nil)
      (leman-e2ee--retry-decryption session))
    ;; Update the mode-line unread indicator.
    (leman--update-unread-indicator)
    ;; Show sync message if appropriate, and run after-initial-sync-hook.
    (when (leman--sync-messages-p session)
      (message (concat "Leman: Sync done."
                       (unless (leman-session-has-synced-p session)
                         (run-hook-with-args 'leman-after-initial-sync-hook session)
                         ;; Show tip after initial sync.
                         (setf (leman-session-has-synced-p session) t)
                         "  Use commands `leman-list-rooms' or `leman-view-room' to view a room."))))))

(defun leman--push-invite-room-events (session invited-room)
  "Push events for INVITED-ROOM into that room in SESSION."
  ;; TODO: Make leman-session-rooms a hash-table.
  (leman--push-joined-room-events session invited-room 'invite))

(defun leman--auto-sync (session)
  "If `leman-auto-sync' is non-nil, sync SESSION again."
  (when leman-auto-sync
    (leman--sync session)))

(defun leman--update-room-buffers (session)
  "Insert new events into SESSION's rooms which have buffers.
To be called in `leman-sync-callback-hook'."
  ;; TODO: Move this to leman-room.el, probably.
  ;; For now, we primitively iterate over the buffer list to find ones
  ;; whose mode is `leman-room-mode'.
  (let* ((buffers (cl-loop for room in (leman-session-rooms session)
                           for buffer = (map-elt (leman-room-local room) 'buffer)
                           when (buffer-live-p buffer)
                           collect buffer)))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (save-window-excursion
          ;; NOTE: When the buffer has a window, it must be the selected one
          ;; while calling event-insertion functions.  I don't know if this is
          ;; due to a bug in EWOC or if I just misunderstand something, but
          ;; without doing this, events may be inserted at the wrong place.
          (when-let ((buffer-window (get-buffer-window buffer)))
            (select-window buffer-window))
          (cl-assert leman-room)
          (when (leman-room-ephemeral leman-room)
            ;; Ephemeral events.
            (leman-room--process-events (leman-room-ephemeral leman-room))
            (setf (leman-room-ephemeral leman-room) nil))
          (when-let ((new-events (alist-get 'new-events (leman-room-local leman-room))))
            ;; HACK: Process these events in reverse order, so that later events (like reactions)
            ;; which refer to earlier events can find them.  (Not sure if still necessary.)
            (leman-room--process-events (reverse new-events))
            (setf (alist-get 'new-events (leman-room-local leman-room)) nil))
          (when-let ((new-events (alist-get 'new-account-data-events (leman-room-local leman-room))))
            ;; Account data events.  Do this last so, e.g. read markers can refer to message events we've seen.
            (leman-room--process-events new-events)
            (setf (alist-get 'new-account-data-events (leman-room-local leman-room)) nil)))))))

(cl-defun leman--push-joined-room-events (session joined-room &optional (status 'join))
  "Push events for JOINED-ROOM into that room in SESSION.
Also used for left rooms, in which case STATUS should be set to
`leave'."
  (pcase-let* ((`(,id . ,event-types) joined-room)
               (id (symbol-name id)) ; Really important that the ID is a STRING!
               ;; TODO: Make leman-session-rooms a hash-table.
               (room (or (cl-find-if (lambda (room)
                                       (equal id (leman-room-id room)))
                                     (leman-session-rooms session))
                         (car (push (make-leman-room :id id) (leman-session-rooms session)))))
               ((map summary state ephemeral timeline
                     ('invite_state (map ('events invite-state-events)))
                     ('account_data (map ('events account-data-events)))
                     ('unread_notifications unread-notifications))
                event-types)
               (latest-timestamp))
    (setf (leman-room-status room) status
          (leman-room-unread-notifications room) unread-notifications)
    ;; NOTE: The idea is that, assuming that events in the sync response are in
    ;; chronological order, we push them to the lists in the room slots in that order,
    ;; leaving the head of each list as the most recent event of that type.  That means
    ;; that, e.g. the room state events may be searched in order to find, e.g. the most
    ;; recent room name event.  However, chronological order is not guaranteed, e.g. after
    ;; loading older messages (the "retro" function; this behavior is in development).

    ;; MAYBE: Use queue.el to store the events in a DLL, so they could
    ;; be accessed from either end.  Could be useful.

    ;; Push the StrippedState events to the room's invite-state.  (These events have no
    ;; timestamp data.)  We also run the event hook, because for invited rooms, the
    ;; invite-state events include room name, topic, etc.
    (cl-loop for event across invite-state-events
             for event-struct = (leman--make-event event)
             do (push event-struct (leman-room-invite-state room))
             (run-hook-with-args 'leman-event-hook event-struct room session))

    ;; Save room summary.
    (dolist (parameter '(m.heroes m.joined_member_count m.invited_member_count))
      (when (alist-get parameter summary)
        ;; These fields are only included when they change.
        (setf (alist-get parameter (leman-room-summary room)) (alist-get parameter summary))))

    ;; Update account data.  According to the spec, only one of each event type is
    ;; supposed to be present in a room's account data, so we store them as an alist keyed
    ;; on their type.  (NOTE: We don't currently make them into event structs, but maybe
    ;; we should in the future.)
    (cl-loop for event across account-data-events
             for type = (alist-get 'type event)
             do (setf (alist-get type (leman-room-account-data room) nil nil #'equal) event))
    ;; But we also need to track just the new events so we can process those in a room
    ;; buffer (and for some reason, we do make them into structs here, but I don't
    ;; remember why).  FIXME: Unify this.
    (cl-callf2 append (mapcar #'leman--make-event account-data-events)
               (alist-get 'new-account-data-events (leman-room-local room)))

    ;; Push the new state and timeline events to the room's slots,
    ;; collecting their 'leman-event' structs (in their original order)
    ;; for running hooks below.
    (cl-macrolet ((push-events (type accessor)
                    ;; Push new events of TYPE to room's slot of ACCESSOR.
                    ;; Return a list of the event structs and the latest
                    ;; origin-server-ts pushed.
                     `(let ((ts 0) (event-structs nil))
                        (cl-loop for event across-ref (alist-get 'events ,type)
                                 do (setf event (leman-e2ee--decrypt-event-struct
                                                 session id event))
                                 ;; Skip events already known to the session
                                 ;; (e.g. re-delivered after a limited timeline,
                                 ;; or by a second concurrent sync), otherwise
                                 ;; they would be shown twice.
                                (unless (and (leman-event-id event)
                                             (gethash (leman-event-id event)
                                                      (leman-session-events session)))
                                  (push event event-structs)
                                  (push event (,accessor room))
                                  (when (leman--sync-messages-p session)
                                    (leman-progress-update))
                                  (when (> (leman-event-origin-server-ts event) ts)
                                    (setf ts (leman-event-origin-server-ts event)))))
                       ;; One would think that one should use `maximizing' here, but, completely
                       ;; inexplicably, it sometimes returns nil, even when every single value it's comparing
                       ;; is a number.  It's absolutely bizarre, but I have to do the equivalent manually.
                       (list (nreverse event-structs) ts))))
      (pcase-let* ((`(,state-event-structs ,state-ts)
                    (push-events state leman-room-state))
                   (`(,timeline-event-structs ,timeline-ts)
                    (push-events timeline leman-room-timeline)))
        (setf latest-timestamp (max state-ts timeline-ts))
        ;; NOTE: We also append the new events to the new-events list in the room's local
        ;; slot, which is used by `leman--update-room-buffers' to insert only new events.
        ;; FIXME: Does this also need to be done for invite-state events?
        (cl-callf2 append timeline-event-structs
                   (alist-get 'new-events (leman-room-local room)))
        ;; Update room's latest-timestamp slot.
        (when (> latest-timestamp (or (leman-room-latest-ts room) 0))
          (setf (leman-room-latest-ts room) latest-timestamp))
        (unless (leman-session-has-synced-p session)
          ;; Only set this token on initial sync, otherwise it would
          ;; overwrite earlier tokens from loading earlier messages.
          (setf (leman-room-prev-batch room) (alist-get 'prev_batch timeline)))
        ;; Run event hook for state and timeline events.
        (dolist (event-structs (list state-event-structs timeline-event-structs))
          (dolist (event event-structs)
            (run-hook-with-args 'leman-event-hook event room session)
            (when (leman--sync-messages-p session)
              (leman-progress-update))))))
    ;; Ephemeral events (do this after state and timeline hooks, so those events will be
    ;; in the hash tables).
    (cl-loop for event across (alist-get 'events ephemeral)
             for event-struct = (leman--make-event event)
             do (push event-struct (leman-room-ephemeral room))
             (leman--process-event event-struct room session))
    (when (leman-session-has-synced-p session)
      ;; NOTE: We don't fill gaps in "limited" requests on initial
      ;; sync, only in subsequent syncs, e.g. after the system has
      ;; slept and awakened.
      ;; NOTE: When not limited, the read value is `:json-false', so
      ;; we must explicitly compare to t.
      (when (eq t (alist-get 'limited timeline))
	;; Timeline was limited: start filling gap.  We start the
	;; gap-filling, retrieving up to the session's current
	;; next-batch token (this function is not called when retrieving
	;; older messages, so the session's next-batch token is only
	;; evaluated once, when this chain begins, and then that token
	;; is passed to repeated calls to `leman-room-retro-to-token'
	;; until the gap is filled).
	(leman-room-retro-to-token room session (alist-get 'prev_batch timeline)
				   (leman-session-next-batch session))))))
(defun leman--push-left-room-events (session left-room)
  "Push events for LEFT-ROOM into that room in SESSION."
  (leman--push-joined-room-events session left-room 'leave))

(defun leman--make-event (event)
  "Return `leman-event' struct for raw EVENT list.
Adds sender to `leman-users' when necessary."
  (pcase-let* (((map content type unsigned redacts
                     ('event_id id) ('origin_server_ts ts)
                     ('sender sender-id) ('state_key state-key))
                event)
               (sender (or (gethash sender-id leman-users)
                           (puthash sender-id (make-leman-user :id sender-id)
                                    leman-users))))
    ;; MAYBE: Handle other keys in the event, such as "room_id" in "invite" events.
    (make-leman-event :id id :sender sender :type type :content content :state-key state-key
                      :origin-server-ts ts :unsigned unsigned
                      ;; Since very few events will be redactions and have this key, we
                      ;; record it in the local slot alist rather than as another slot on
                      ;; the struct.
                      :local (when redacts
                               (leman-alist 'redacts redacts)))))

(defun leman--put-event (event _room session)
  "Put EVENT on SESSION's events table."
  (puthash (leman-event-id event) event (leman-session-events session)))

;; FIXME: These functions probably need to compare timestamps to
;; ensure that older events that are inserted at the head of the
;; events lists aren't used instead of newer ones.

;; TODO: These two functions should be folded into event handlers.

;;;;; Reading/writing sessions

;; TODO: Use `persist' and/or `multisession'.

(defun leman--read-sessions ()
  "Return saved sessions alist read from disk.
Returns nil if unable to read `leman-sessions-file'."
  (cl-labels ((plist-to-session (plist)
                (pcase-let* (((map (:user user-data) (:server server-data)
                                   (:token token) (:transaction-id transaction-id))
                              plist)
                             (user (apply #'make-leman-user user-data))
                             (server (apply #'make-leman-server server-data))
                             (session (make-leman-session :user user :server server
                                                          :token token :transaction-id transaction-id)))
                  (setf (leman-session-events session) (make-hash-table :test #'equal))
                  session)))
    (when (file-exists-p leman-sessions-file)
      (pcase-let* ((read-circle t)
                   (sessions (with-temp-buffer
                               (insert-file-contents leman-sessions-file)
                               (read (current-buffer)))))
        (prog1
            (cl-loop for (id . plist) in sessions
                     collect (cons id (plist-to-session plist)))
          (message "Leman: Read sessions."))))))

(defun leman--write-sessions (sessions-alist)
  "Write SESSIONS-ALIST to disk."
  ;; We only record the slots we need.  We record them as a plist
  ;; so that changes to the struct definition don't matter.
  ;; NOTE: If we ever persist more session data (like room data, so we
  ;; could avoid doing an initial sync next time), we should limit the
  ;; amount of session data saved (e.g. room history could grow
  ;; forever on-disk, which probably isn't what we want).

  ;; NOTE: This writes all current sessions, even if there are multiple active ones and only one
  ;; is being disconnected.  That's probably okay, but it might be something to keep in mind.
  (cl-labels ((session-plist (session)
                (pcase-let* (((cl-struct leman-session user server token transaction-id) session)
                             ((cl-struct leman-user (id user-id) username) user)
                             ((cl-struct leman-server (name server-name) uri-prefix) server))
                  (list :user (list :id user-id
                                    :username username)
                        :server (list :name server-name
                                      :uri-prefix uri-prefix)
                        :token token
                        :transaction-id transaction-id))))
    (message "Leman: Writing sessions...")
    (with-temp-file leman-sessions-file
      (pcase-let* ((print-level nil)
                   (print-length nil)
                   ;; Very important to use `print-circle', although it doesn't
                   ;; solve everything.  Writing/reading Lisp data can be tricky...
                   (print-circle t)
                   (sessions-alist-plist (cl-loop for (id . session) in sessions-alist
                                                  collect (cons id (session-plist session)))))
        (prin1 sessions-alist-plist (current-buffer))))
    ;; Ensure permissions are safe.
    (chmod leman-sessions-file #o600)))

(defun leman--kill-emacs-hook ()
  "Function to be added to `kill-emacs-hook'.
Writes Leman session to disk when enabled."
  (ignore-errors
    ;; To avoid interfering with Emacs' exit, We must be careful that
    ;; this function handles errors, so just ignore any.
    (when (and leman-save-sessions
               leman-sessions)
      (leman--write-sessions leman-sessions))))

;;;;; Session revocation

(defun leman--session-revoked-cleanup (session &optional soft-logout)
  "Stop SESSION's background work after its token was revoked.
Added to `leman-session-revoked-hook'.  On a hard logout (the
default), also discard SESSION's E2EE crypto store: the spec
requires that persisted encryption keys and device information
are not reused after the server has destroyed the session."
  ;; Stop an outstanding sync (the failing sync already deregistered
  ;; itself; be safe).
  (when-let ((process (map-elt leman-syncs session)))
    (when (process-live-p process)
      (delete-process process))
    (setf (map-elt leman-syncs session) nil))
  ;; Typing notifications repeat on a timer.
  (when leman-room-typing-timer
    (when (timerp leman-room-typing-timer)
      (cancel-timer leman-room-typing-timer))
    (setf leman-room-typing-timer nil))
  ;; Read receipts repeat on an idle timer.
  (when leman-read-receipt-idle-timer
    (when (timerp leman-read-receipt-idle-timer)
      (cancel-timer leman-read-receipt-idle-timer))
    (setf leman-read-receipt-idle-timer nil))
  ;; The E2EE agent is useless without a session.
  (when-let ((agent (leman-session-e2ee session)))
    (leman-e2ee-stop agent)
    (setf (leman-session-e2ee session) nil))
  ;; On a hard logout, discard the crypto store (spec: persisted
  ;; encryption keys and device information must not be reused).
  (unless soft-logout
    (when-let* ((user-id (leman-user-id (leman-session-user session)))
                (device-id (leman-session-device-id session)))
      (leman-e2ee--discard-store user-id device-id)))
  ;; Forget the dead token so restarts don't reuse it.
  (setf (leman-session-token session) nil)
  ;; Drop the dead session from the session list, so reconnecting
  ;; prompts a fresh login instead of resuming the revoked session.
  (setf leman-sessions (cl-remove session leman-sessions :key #'cdr :test #'eq))
  (when leman-save-sessions
    (leman--write-sessions leman-sessions)))

(add-hook 'leman-session-revoked-hook #'leman--session-revoked-cleanup)

;;;;; Event handlers

(defvar leman-event-handlers nil
  "Alist mapping event types to functions which process an event of each type.
Each function is called with three arguments: the event, the
room, and the session.  These handlers are run regardless of
whether a room has a live buffer.")

(defun leman--process-event (event room session)
  "Process EVENT for ROOM in SESSION.
Uses handlers defined in `leman-event-handlers'.  If no handler
is defined for EVENT's type, does nothing and returns nil.  Any
errors signaled during processing are demoted in order to prevent
unexpected errors from arresting event processing and syncing."
  (when-let ((handler (alist-get (leman-event-type event) leman-event-handlers nil nil #'equal)))
    ;; We demote any errors that happen while processing events, because it's possible for
    ;; events to be malformed in unexpected ways, and that could cause an error, which
    ;; would stop processing of other events and prevent further syncing.  See,
    ;; e.g. <https://github.com/alphapapa/ement.el/pull/61>.
    (with-demoted-errors "Leman (leman--process-event): Error processing event: %S"
      (funcall handler event room session))))

(defmacro leman-defevent (type &rest body)
  "Define an event handling function for events of TYPE, a string.
Around the BODY, the variable `event' is bound to the event being
processed, `room' to the room struct in which the event occurred,
and `session' to the session.  Adds function to
`leman-event-handlers', which see."
  (declare (indent defun))
  `(setf (alist-get ,type leman-event-handlers nil nil #'string=)
         (lambda (event room session)
           ,(concat "`leman-' handler function for " type " events.")
           ,@body)))

;; I love how Lisp macros make it so easy and concise to define these
;; event handlers!

(leman-defevent "m.room.avatar"
  (when leman-room-avatars
    ;; If room avatars are disabled, we don't download avatars at all.  This
    ;; means that, if a user has them disabled and then reenables them, they will
    ;; likely need to reconnect to cause them to be displayed in most rooms.
    (if-let ((url (alist-get 'url (leman-event-content event))))
        (plz-run
         (plz-queue leman-images-queue
           ;; NOTE: Authenticated media endpoint: servers like Conduit
           ;; reject the unauthenticated download URL.
           'get (leman--mxc-to-authenticated-url url session) :as 'binary :noquery t
           :headers (list (cons "Authorization"
                                (concat "Bearer " (leman-session-token session))))
           :then (lambda (data)
                   (when leman-room-avatars
                     ;; MAYBE: Store the raw image data instead of using create-image here.
                     (let ((image (create-image data nil 'data-p
                                                :ascent 'center
                                                :max-width leman-room-avatar-max-width
                                                :max-height leman-room-avatar-max-height)))
                       (if (not image)
                           (progn
                      (display-warning 'leman (format "Room avatar seems unreadable:  ROOM-ID:%S  AVATAR-URL:%S"
                                                      (leman-room-id room) (leman--mxc-to-authenticated-url url session)))
                             (setf (leman-room-avatar room) nil
                                   (alist-get 'room-list-avatar (leman-room-local room)) nil))
                         (when (fboundp 'imagemagick-types)
                           ;; Only do this when ImageMagick is supported.
                           ;; FIXME: When requiring Emacs 27+, remove this (I guess?).
                           (setf (image-property image :type) 'imagemagick))
                         ;; We set the room-avatar slot to a propertized string that
                         ;; displays as the image.  This seems the most convenient thing to
                         ;; do.  We also unset the cached room-list-avatar so it can be
                         ;; remade.
                         (setf (leman-room-avatar room) (propertize " " 'display image)
                               (alist-get 'room-list-avatar (leman-room-local room)) nil)))))))
      ;; Unset avatar.
      (setf (leman-room-avatar room) nil
            (alist-get 'room-list-avatar (leman-room-local room)) nil))))

(leman-defevent "m.room.create"
  (ignore session)
  (pcase-let* (((cl-struct leman-event (content (map type))) event))
    (when type
      (setf (leman-room-type room) type))))

(leman-defevent "m.room.member"
  "Put/update member on `leman-users' and room's members table."
  (ignore session)
  (pcase-let* (((cl-struct leman-room members) room)
               ((cl-struct leman-event state-key
                           (content (map displayname membership
                                         ('avatar_url avatar-url))))
                event)
               (user (or (gethash state-key leman-users)
                         (puthash state-key
                                  (make-leman-user
                                   :id state-key :avatar-url avatar-url
                                   ;; NOTE: The spec doesn't seem to say whether the
                                   ;; displayname in the member event applies only to
                                   ;; the room or is for the user generally, so we'll
                                   ;; save it in the struct anyway.
                                   ;; FIXME: This is probably wrong: it probably means
                                   ;; overwriting the global displayname with any
                                   ;; room-specific one that was most recently processed.
                                   :displayname displayname)
                                  leman-users))))
    (pcase membership
      ("join"
       (puthash state-key user members)
       (if displayname
           ;; NOTE: This handler is only called for new events, not when retrieving old events.
           ;; Therefore it's safe to update the cached displayname from such an event.
           (puthash user displayname (leman-room-displaynames room))
         ;; No displayname set for this room: recalculate.
         (leman--user-displayname-in room user 'recalculate)))
      (_ (remhash state-key members)
         (remhash user (leman-room-displaynames room))))))

(leman-defevent "m.room.name"
  (ignore session)
  (pcase-let* (((cl-struct leman-event (content (map name))) event))
    (when name
      ;; Recalculate room name and cache in slot.
      (setf (leman-room-display-name room) (leman--room-display-name room)))))

(leman-defevent "m.room.topic"
  (ignore session)
  (pcase-let* (((cl-struct leman-event (content (map topic))) event))
    (when topic
      (setf (leman-room-topic room) topic))))

(leman-defevent "m.receipt"
  (ignore session)
  (pcase-let (((cl-struct leman-event content) event)
              ((cl-struct leman-room (receipts room-receipts)) room))
    (cl-loop for (event-id . receipts) in content
             do (cl-loop for (user-id . receipt) in (alist-get 'm.read receipts)
                         ;; Users may not have been "seen" yet, so although we'd
                         ;; prefer to key on the user struct, we key on the user ID.
                         ;; Same for events, unfortunately.
                         ;; NOTE: The JSON map keys are converted to symbols by `json-read'.
                         ;; MAYBE: (Should we keep them that way?  It would use less memory, I guess.)
                         do (puthash (symbol-name user-id)
                                     (cons (symbol-name event-id) (alist-get 'ts receipt))
                                     room-receipts)))))

(leman-defevent "m.space.child"
  ;; SPEC: v1.2/11.35.
  (pcase-let* ((space-room room)
               ((cl-struct leman-session rooms) session)
               ((cl-struct leman-room (id parent-room-id)) space-room)
               ((cl-struct leman-event (state-key child-room-id) (content (map via))) event)
               (child-room (cl-find child-room-id rooms :key #'leman-room-id :test #'equal)))
    (if via
        ;; Child being declared: add it.
        (progn
          (cl-pushnew child-room-id (alist-get 'children (leman-room-local space-room)) :test #'equal)
          (when child-room
            ;; The user is also in the child room: link the parent space-room in it.
            ;; FIXME: On initial sync, if the child room hasn't been processed yet, this will fail.
            (cl-pushnew parent-room-id (alist-get 'parents (leman-room-local child-room)) :test #'equal)))
      ;; Child being disowned: remove it.
      (setf (alist-get 'children (leman-room-local space-room))
            (delete child-room-id (alist-get 'children (leman-room-local space-room))))
      (when child-room
        ;; The user is also in the child room: unlink the parent space-room in it.
        (setf (alist-get 'parents (leman-room-local child-room))
              (delete parent-room-id (alist-get 'parents (leman-room-local child-room))))))))

(leman-defevent "m.room.canonical_alias"
  (ignore session)
  (pcase-let (((cl-struct leman-event (content (map alias))) event))
    (setf (leman-room-canonical-alias room) alias)))

(defun leman--link-children (session)
  "Link child rooms in SESSION.
To be called after initial sync."
  ;; On initial sync, when processing m.space.child events, the child rooms may not have
  ;; been processed yet, so we link them again here.
  (pcase-let (((cl-struct leman-session rooms) session))
    (dolist (room rooms)
      (pcase-let (((cl-struct leman-room (id parent-id) (local (map children))) room))
        (when children
          (dolist (child-id children)
            (when-let ((child-room (cl-find child-id rooms :key #'leman-room-id :test #'equal)))
              (cl-pushnew parent-id (alist-get 'parents (leman-room-local child-room)) :test #'equal))))))))

;;;;; Transient

(require 'transient)

;; These files are not required by leman.el (leman-tabulated-room-list
;; requires leman, so it cannot be required here), but their commands
;; are autoloaded.
(declare-function leman-list-rooms "leman-room-list")
(declare-function leman-tabulated-room-list "leman-tabulated-room-list")
(declare-function leman-directory "leman-directory")
(declare-function leman-room--escape-% "leman-room")

;;;###autoload
(transient-define-prefix leman-transient ()
  "Transient for Leman, callable from any buffer."
  [:pad-keys t
             ["Session"
              ("c" "Connect" leman-connect)
              ("d" "Disconnect" leman-disconnect)
              ("K" "Kill Leman buffers" leman-kill-buffers)
              ("P" "Set display name" leman-set-display-name)
              ("S" "Sync now" leman-room-sync)]
             ["Rooms"
              ("l" "List rooms" leman-list-rooms)
              ("t" "List rooms (tabulated)" leman-tabulated-room-list)
              ("v" "View room" leman-view-room)
              ("j" "Join room" leman-room-join)
              ("N" "Create room" leman-create-room)
              ("V" "View space" leman-view-space)]]
  [:pad-keys t
             ["Room actions"
              ("i" "Invite user" leman-invite-user)
              ("T" "Set topic" leman-room-set-topic)
              ("f" "Tag room" leman-tag-room)
              ("s" "Set notification state" leman-room-set-notification-state)
              ("m" "Mark read to point" leman-room-mark-read)
              ("L" "Leave room" leman-room-leave)
              ("F" "Forget room" leman-forget-room)]
             ["Notifications"
              ("n" "Notifications" leman-notifications)
              ("M" "Mentions" leman-notify-switch-to-mentions-buffer)
              ("B" "Notifications buffer" leman-notify-switch-to-notifications-buffer)
              ("u" "Ignore user" leman-ignore-user)]]
  [:pad-keys t
             ["Misc"
              ("D" "Room directory" leman-directory)
              ("o" "Occur search in room" leman-room-occur)
              ("r" "Room transient" leman-room-transient)
              ("C" "Flush colors" leman-room-flush-colors)
              ("q" "Quit" transient-quit-one)]])

;;;;; Unread indicator

(defcustom leman-unread-indicator-max-rooms 3
  "Number of room names shown inline in the unread indicator.
If nil, the indicator shows only counts; in either case, its
help-echo lists every room with unread notifications."
  :type '(choice (const :tag "Counts only" nil)
                 (natnum :tag "Number of room names")))

(defvar leman-unread-indicator-string nil
  "String shown in the mode line by `leman-unread-indicator-mode'.
Updated by `leman--update-unread-indicator'.")
(put 'leman-unread-indicator-string 'risky-local-variable t)

(defvar leman-unread-indicator-keymap
  (let ((map (make-sparse-keymap)))
    ;; Bind down- events so that the global keymap won't "shine
    ;; through".
    (define-key map [mode-line down-mouse-1] #'ignore)
    (define-key map [mode-line mouse-1] #'leman-unread-indicator-click)
    map)
  "Keymap for clicks on the unread indicator.")

(defun leman--unread-rooms ()
  "Return conses of (ROOM . SESSION) for joined unread rooms.
Sorted by notification count, most first."
  (sort
   (cl-loop for (_id . session) in leman-sessions
            append (cl-loop for room in (leman-session-rooms session)
                            for notifications = (map-elt (leman-room-unread-notifications room)
                                                         'notification_count 0)
                            when (and (eq 'join (leman-room-status room))
                                      (> notifications 0))
                            collect (cons room session)))
   (lambda (a b)
     (> (map-elt (leman-room-unread-notifications (car a)) 'notification_count 0)
        (map-elt (leman-room-unread-notifications (car b)) 'notification_count 0)))))

(defun leman--unread-help-echo ()
  "Return a help-echo string summarizing rooms with unread counts."
  (concat
   (string-join
    (cl-loop for (room . _session) in (leman--unread-rooms)
             for notifications = (map-elt (leman-room-unread-notifications room) 'notification_count 0)
             collect (format "%s: %d%s"
                             (or (leman-room-display-name room) (leman-room-id room))
                             notifications
                             (if-let ((highlights (map-elt (leman-room-unread-notifications room) 'highlight_count 0)))
                                 (format " (%d highlights)" highlights)
                               "")))
    "\n")
   "\n(mouse-1: view room with most unread notifications)"))

(defun leman--unread-room-names (max)
  "Return up to MAX unread room names with their counts."
  (cl-loop for i from 1
           for (room . _session) in (leman--unread-rooms)
           while (<= i max)
           collect (format "%s %s"
                           ;; Escape the name: the indicator string is
                           ;; interpreted as a mode-line format string, so
                           ;; unescaped "%s" in a room name would be treated
                           ;; as invalid %-constructs (displayed, e.g., as
                           ;; "*invalid*").
                           (leman-room--escape-%
                            (or (leman-room-display-name room)
                                (leman-room-id room)))
                           (map-elt (leman-room-unread-notifications room) 'notification_count 0))))

(defun leman-unread-indicator-click (_event)
  "View the room with the most unread notifications."
  (interactive "e")
  (if-let* ((rooms (leman--unread-rooms))
            (found (car rooms)))
      (pcase-let ((`(,room . ,session) found))
        (leman-view-room room session))
    (message "No unread rooms")))

(defun leman--update-unread-indicator ()
  "Update `leman-unread-indicator-string'.
To be called after syncs and when read markers are moved."
  (setf leman-unread-indicator-string
        (if-let* ((rooms (leman--unread-rooms))
                  (notifications (cl-loop for (room . _) in rooms
                                          sum (map-elt (leman-room-unread-notifications room) 'notification_count 0)))
                  (highlights (cl-loop for (room . _) in rooms
                                       sum (map-elt (leman-room-unread-notifications room) 'highlight_count 0))))
            (propertize
             (concat
              (propertize (format "✉ %d" notifications) 'face 'bold)
              (when (> highlights 0)
                (propertize (format " @%d" highlights) 'face 'leman-room-mention))
              (when-let* ((max-rooms leman-unread-indicator-max-rooms)
                          (names (leman--unread-room-names max-rooms)))
                (concat " (" (string-join names ", ") ")")))
             'help-echo (leman--unread-help-echo)
             'mouse-face 'mode-line-highlight
             'local-map leman-unread-indicator-keymap)
          "")))

(define-minor-mode leman-unread-indicator-mode
  "Show unread notification counts in the mode line.
Counts are updated after each sync and when read markers are
moved.  Highlights (i.e. mentions) are shown in parentheses."
  :global t
  :group 'leman
  (if leman-unread-indicator-mode
      (progn
        ;; Register in `global-mode-string' (shown in the default
        ;; `mode-line-misc-info') and directly in `mode-line-misc-info',
        ;; in case the user's configuration omits `global-mode-string'
        ;; from it.
        (add-to-list 'global-mode-string 'leman-unread-indicator-string)
        (add-to-list 'mode-line-misc-info 'leman-unread-indicator-string)
        (leman--update-unread-indicator))
    (setq global-mode-string (delq 'leman-unread-indicator-string global-mode-string)
          mode-line-misc-info (delq 'leman-unread-indicator-string mode-line-misc-info))
    (setf leman-unread-indicator-string nil)))

;;;;; Savehist compatibility

;; See <https://github.com/alphapapa/ement.el/issues/216>.

(defvar savehist-save-hook)

(with-eval-after-load 'savehist
  ;; TODO: Consider using a symbol property on our commands and checking that rather than
  ;; symbol names; would avoid consing.
  (defun leman--savehist-save-hook ()
    "Remove all `leman-' commands from `command-history'.
Because when `savehist' saves `command-history', it includes the
interactive arguments passed to the command, which in our case
includes large data structures that should never be persisted!"
    (setf command-history
          (cl-remove-if (pcase-lambda (`(,command . ,_))
                          (cl-typecase command
                            (symbol (string-match-p (rx bos "leman-") (symbol-name command)))))
                        command-history)))
  (cl-pushnew 'leman--savehist-save-hook savehist-save-hook))

;;;; Footer

(provide 'leman)

;;; leman.el ends here
