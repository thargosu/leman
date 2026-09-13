;;;; leman-notify-tests.el --- Tests for leman-notify.el    -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the notification layer, especially the D-Bus
;; availability re-check (Emacs daemons often start before the
;; desktop's notification service).

;;; Code:

(require 'ert)

(require 'leman-notify)

(ert-deftest leman-notify--dbus-recheck-retries-and-flips-on ()
  (let ((leman-notify-dbus-p nil)
        (leman-notify-dbus-next-check nil)
        (leman-notify-dbus-retry-interval 300)
        (pings 0))
    (cl-letf (((symbol-function #'dbus-get-unique-name) #'identity)
              ((symbol-function #'dbus-ping)
               (lambda (&rest _) (cl-incf pings) t)))
      ;; The load-time detection failed: a retry pings and flips on.
      (should (leman-notify--dbus-recheck))
      (should (eq leman-notify-dbus-p t))
      (should (= 1 pings))
      ;; Retries are rate-limited: no more pings within the interval.
      (should (leman-notify--dbus-recheck))
      (should (= 1 pings)))))

(ert-deftest leman-notify--dbus-recheck-stays-off-on-failure ()
  (let ((leman-notify-dbus-p nil)
        (leman-notify-dbus-next-check nil)
        (leman-notify-dbus-retry-interval 300))
    (cl-letf (((symbol-function #'dbus-get-unique-name) #'identity)
              ((symbol-function #'dbus-ping) (lambda (&rest _) nil)))
      (should-not (leman-notify--dbus-recheck))
      (should (null leman-notify-dbus-p))
      (should leman-notify-dbus-next-check)
      ;; A retry within the interval does not ping again.
      (should-not (leman-notify--dbus-recheck)))))

(ert-deftest leman-notify--dbus-recheck-noop-when-already-on ()
  (let ((leman-notify-dbus-p t)
        (leman-notify-dbus-next-check nil)
        (pings 0))
    (cl-letf (((symbol-function #'dbus-ping)
               (lambda (&rest _) (cl-incf pings) t)))
      (should (leman-notify--dbus-recheck))
      (should (= 0 pings)))))

;;;; Footer

(provide 'leman-notify-tests)

;;;; leman-notify-tests.el ends here
