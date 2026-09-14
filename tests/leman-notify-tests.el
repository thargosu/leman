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
        (probes 0))
    (cl-letf (((symbol-function #'leman-notify--dbus-service-available-p)
               (lambda () (cl-incf probes) t)))
      ;; The load-time detection failed: a retry probes and flips on.
      (should (leman-notify--dbus-recheck))
      (should (eq leman-notify-dbus-p t))
      (should (= 1 probes))
      ;; Retries are rate-limited: no more probes within the interval.
      (should (leman-notify--dbus-recheck))
      (should (= 1 probes)))))

(ert-deftest leman-notify--dbus-recheck-stays-off-on-failure ()
  (let ((leman-notify-dbus-p nil)
        (leman-notify-dbus-next-check nil)
        (leman-notify-dbus-retry-interval 300))
    (cl-letf (((symbol-function #'leman-notify--dbus-service-available-p)
               (lambda () nil)))
      (should-not (leman-notify--dbus-recheck))
      (should (null leman-notify-dbus-p))
      (should leman-notify-dbus-next-check)
      ;; A retry within the interval does not probe again.
      (should-not (leman-notify--dbus-recheck)))))

(ert-deftest leman-notify--dbus-recheck-noop-when-already-on ()
  (let ((leman-notify-dbus-p t)
        (leman-notify-dbus-next-check nil)
        (probes 0))
    (cl-letf (((symbol-function #'leman-notify--dbus-service-available-p)
               (lambda () (cl-incf probes) t)))
      (should (leman-notify--dbus-recheck))
      (should (= 0 probes)))))

(ert-deftest leman-notify--notifications-notify-app-icon-fallback ()
  "The room avatar is shown when available, the Leman icon otherwise."
  (let* ((avatar (propertize " " 'display '(image :data "avatar-bytes")))
         (event (make-leman-event
                 :sender (make-leman-user :id "@alice:example.org")
                 :content '((body . "hello"))))
         notified)
    (cl-letf (((symbol-function #'notifications-notify)
               (lambda (&rest args) (setq notified args)))
              ((symbol-function #'leman-notify--temp-file)
               (lambda (content &rest _) content)))
      (pcase-dolist (`(,room . ,expected)
                     (cons (cons (make-leman-room :display-name "Room" :avatar avatar)
                                 "avatar-bytes")
                           (cons (cons (make-leman-room :display-name "Room")
                                       leman-notify-app-icon)
                                 nil)))
        (leman-notify--notifications-notify event room nil)
        (should (equal (plist-get notified :app-icon) expected))))))

 ;;;; Footer

(provide 'leman-notify-tests)

;;;; leman-notify-tests.el ends here
