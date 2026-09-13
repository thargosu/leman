;;; leman-structs.el --- Leman structs               -*- lexical-binding: t; -*-

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

(require 'cl-lib)

;;;; Structs

(cl-defstruct leman-user
  id displayname account-data
  (color nil :documentation "Color in which to display user's name.")
  (message-color nil :documentation "Color in which to display user's messages.")
  (username nil
            ;; NOTE: Not exactly according to spec, I guess, but useful for now.
            :documentation "Username part of user's Matrix ID.")
  (avatar-url nil :documentation "MXC URL to user's avatar.")
  (avatar nil :documentation "One-space string with avatar image in display property."))

(cl-defstruct leman-event
  id sender content origin-server-ts type unsigned state-key
  receipts
  ;; The local slot is an alist used by the local client only.
  local)

(cl-defstruct leman-server
  name uri-prefix)

(cl-defstruct leman-session
  user server token transaction-id rooms next-batch
  device-id initial-device-display-name has-synced-p
  account-data
  ;; Hash table of all seen events, keyed on event ID.
  events
  ;; E2EE agent (a `leman-e2ee' struct), or nil.
  e2ee
  ;; Flow IDs of incoming E2EE verification requests already
  ;; announced to the user.
  e2ee-announced-requests)

(cl-defstruct leman-room
  id display-name prev-batch
  summary state timeline ephemeral account-data unread-notifications
  latest-ts topic canonical-alias avatar status type invite-state
  (members (make-hash-table :test #'equal) :documentation "Hash table mapping joined user IDs to user structs.")
  ;; The local slot is an alist used by the local client only.
  local
  (receipts (make-hash-table :test #'equal))
  (displaynames (make-hash-table) :documentation "Hash table mapping users to their displayname in this room."))

;;;; Variables


;;;; Customization


;;;; Commands


;;;; Functions


;;;; Footer

(provide 'leman-structs)

;;; leman-structs.el ends here
