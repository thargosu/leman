;;; leman-api.el --- Matrix API library              -*- lexical-binding: t; -*-

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

(require 'json)
(require 'url-parse)
(require 'url-util)

(require 'plz)

(require 'leman-macros)
(require 'leman-structs)

;;;; Variables


;;;; Customization


;;;; Commands


;;;; Functions

(cl-defun leman-api (session endpoint
                             &key then data params queue
                             (content-type "application/json")
                             (data-type 'text)
                             (else #'leman-api-error) (method 'get)
                             ;; FIXME: What's the right term for the URL part after "/_matrix/"?
                             (endpoint-category "client")
                             (json-read-fn #'json-read)
                             ;; NOTE: Hard to say what the default timeouts
                             ;; should be.  Sometimes the matrix.org homeserver
                             ;; can get slow and respond a minute or two later.
                             (connect-timeout 10) (timeout 60)
                             (version "r0"))
  "Make API request on SESSION to ENDPOINT.
The request automatically uses SESSION's server, URI prefix, and
access token.

These keyword arguments are passed to `plz', which see: THEN,
DATA (passed as BODY), QUEUE (passed to `plz-queue', which see),
DATA-TYPE (passed as BODY-TYPE), ELSE, METHOD,
JSON-READ-FN (passed as AS), CONNECT-TIMEOUT, TIMEOUT.

Other arguments include PARAMS (used as the URL's query
parameters), ENDPOINT-CATEGORY (added to the endpoint URL), and
VERSION (added to the endpoint URL).

Note that most Matrix requests expect JSON-encoded data, so
usually the DATA argument should be passed through
`json-encode'."
  (declare (indent defun))
  (pcase-let* (((cl-struct leman-session server token) session)
               ((cl-struct leman-server uri-prefix) server)
               ((cl-struct url type host portspec) (url-generic-parse-url uri-prefix))
               (path (format "/_matrix/%s/%s/%s" endpoint-category version endpoint))
               ;; NOTE: `url-build-query-string' wants proper-list items
               ;; like ("key" "value"), but params decoded from JSON
               ;; objects are dotted pairs like (key . "value"); accept
               ;; both by rewriting dotted pairs as one-element lists.
               (params (mapcar (lambda (param)
                                 (if (listp (cdr param))
                                     param
                                   (list (car param) (cdr param))))
                               params))
               (query (url-build-query-string params))
               (filename (concat path "?" query))
               (url (url-recreate-url
                     (url-parse-make-urlobj type nil nil host portspec filename nil data t)))
               (headers (leman-alist "Content-Type" content-type))
               (plz-args))
      (when token
        ;; Almost every request will require a token (only a few, like checking login flows, don't),
        ;; so we simplify the API by using the token automatically when the session has one.
        (push (cons "Authorization" (concat "Bearer " token)) headers))
      ;; A revoked session (e.g. signed out from another device) must
      ;; not produce any more network traffic.
      (when (leman-session-revoked-p session)
        (signal 'leman-api-session-revoked
                (list "Leman: session signed out (access token revoked); reconnect with M-x leman-connect")))
      ;; Detect revoked tokens on every response path, then run the
      ;; caller's handler.
      (when else
        (let ((callback else))
          (setf else (lambda (plz-error)
                       (when (leman--response-revoked-p plz-error)
                         (leman--session-revoked
                          session (leman--response-soft-logout-p plz-error)))
                       (if (eq callback #'leman-api-error)
                           (leman-api-error plz-error url)
                         (funcall callback plz-error))))))
      (setf plz-args (list method url :headers headers :body data :body-type data-type
                           :as json-read-fn :then then :else else
                           :connect-timeout connect-timeout :timeout timeout :noquery t))
    ;; Omit `then' from debugging because if it's a partially applied
    ;; function on the session object, which may be very large, it
    ;; will take a very long time to print into the warnings buffer.
    ;;  (leman-debug (current-time) method url headers)
    (if queue
        (plz-run
         (apply #'plz-queue queue plz-args))
      (apply #'plz plz-args))))

(define-error 'leman-api-error "Leman API error" 'error)

(define-error 'leman-api-session-revoked "Leman session signed out" 'leman-api-error)

(defvar leman--revoked-sessions (make-hash-table :weakness 'key :test #'eq)
  "Sessions whose access token was revoked.
E.g. the session was signed out from another device's session
list.")

(defun leman-session-revoked-p (session)
  "Return non-nil if SESSION's access token was revoked."
  (and session (gethash session leman--revoked-sessions)))

(defcustom leman-session-revoked-hook nil
  "Hooks run when a session's access token is found revoked.
Each hook function receives the session and a SOFT-LOGOUT flag:
non-nil when the server allows the client to reuse its persisted
state (e.g. the crypto store), nil for a hard logout where all
persisted state must be discarded."
  :type 'hook
  :group 'leman)

(defun leman--session-revoked (session &optional soft-logout)
  "Record that SESSION's access token was revoked and warn the user.
SOFT-LOGOUT is the server's `soft_logout' flag (nil for a hard
logout).  Idempotent: only the first call for a session takes
effect."
  (unless (gethash session leman--revoked-sessions)
    (puthash session t leman--revoked-sessions)
    (display-warning
     'leman
     (format "Leman: the session of %s was signed out (its access token was revoked, e.g. the session was removed from another device's session list).  Reconnect with `M-x leman-connect'."
             (leman-user-id (leman-session-user session))))
    (run-hook-with-args 'leman-session-revoked-hook session soft-logout)))

(defun leman--response-soft-logout-p (plz-error)
  "Return the `soft_logout' flag of a revoked PLZ-ERROR response.
Defaults to nil (a hard logout) when the flag is absent."
  (pcase-let (((cl-struct plz-error response) plz-error))
    (when (plz-response-p response)
      (let ((json-object (ignore-errors
                           (json-read-from-string
                            (plz-response-body response)))))
        (alist-get 'soft_logout json-object)))))

(defun leman--response-revoked-p (plz-error)
  "Return non-nil if PLZ-ERROR reports a revoked access token."
  (pcase-let (((cl-struct plz-error response) plz-error))
    (when (plz-response-p response)
      (and (= (plz-response-status response) 401)
           (let ((json-object (ignore-errors
                                (json-read-from-string
                                 (plz-response-body response)))))
             (member (alist-get 'errcode json-object)
                     '("M_UNKNOWN_TOKEN" "M_USER_DEACTIVATED")))))))

(defun leman-api-error (plz-error &optional url)
  "Signal an Leman API error for PLZ-ERROR.
If URL, include it in the error message."
  ;; This feels a little messy, but it seems to be reasonable.
  (pcase-let* (((cl-struct plz-error response
                           (message plz-message) (curl-error `(,curl-exit-code . ,curl-message)))
                plz-error)
               (status (when (plz-response-p response)
                         (plz-response-status response)))
               (body (when (plz-response-p response)
                       (plz-response-body response)))
               (json-object (when body
                              (ignore-errors
                                (json-read-from-string body))))
               (error-message (format "%S: %s"
                                      (or curl-exit-code status)
                                      (or (when json-object
                                            (alist-get 'error json-object))
                                          curl-message
                                          plz-message))))
    (when url
      (setf error-message (concat error-message " [" url "]")))
    (signal 'leman-api-error (list error-message))))

;;;; Footer

(provide 'leman-api)

;;; leman-api.el ends here
