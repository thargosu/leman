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

(ert-deftest leman-notify--notifications-notify-on-action ()
  "Desktop notifications carry a default action which shows the
event in Emacs; other action keys do nothing."
  (let* ((event (make-leman-event
                 :sender (make-leman-user :id "@alice:example.org")
                 :content '((body . "hello"))))
         (room (make-leman-room :id "!room:example.org" :display-name "Room"))
         (notified nil)
         (activated nil))
    (cl-letf (((symbol-function #'notifications-notify)
               (lambda (&rest args) (setq notified args)))
              ((symbol-function #'leman-notify-show)
               (lambda (ev rm _session)
                 (setf activated (list (leman-event-id ev) (leman-room-id rm)))))
              ((symbol-function #'leman-notify--temp-file)
               (lambda (content &rest _) content)))
      (leman-notify--notifications-notify event room nil)
      (should (equal (plist-get notified :actions) '("default" "Show in Emacs")))
      ;; Clicking the notification activates the "default" action.
      (funcall (plist-get notified :on-action) 42 "default")
      (should (equal activated (list (leman-event-id event) "!room:example.org")))
      ;; Other action keys (there are none, but be safe) do not.
      (setf activated nil)
      (funcall (plist-get notified :on-action) 42 "other")
      (should-not activated))))

(ert-deftest leman-notify-show-displays-event ()
  "Activating a notification raises a frame, shows the room, and
moves point to the event; a missing event says so without erroring."
  (let* ((room (make-leman-room :id "!room:example.org"))
         (event (make-leman-event :id "$e1"))
         (missing (make-leman-event :id "$missing"))
         raised
         shown
         (jumps nil))
    (with-current-buffer (get-buffer-create " *leman-notify-show-test*")
      (setf (alist-get 'buffer (leman-room-local room)) (current-buffer)))
    (unwind-protect
        (cl-letf (((symbol-function #'leman-notify--raise-frame)
                   (lambda () (setf raised t)))
                  ((symbol-function #'leman-view-room)
                   (lambda (_room _session) (setf shown t)))
                  ((symbol-function #'leman-room-goto-event)
                   (lambda (event)
                     (if (equal (leman-event-id event) "$missing")
                         (error "Event not found in buffer: %S" (leman-event-id event))
                       (push (leman-event-id event) jumps)))))
          (leman-notify-show event room nil)
          (should raised)
          (should shown)
          (should (equal jumps '("$e1")))
          ;; A missing event does not propagate its error.
          (setf raised nil shown nil)
          (leman-notify-show missing room nil)
          (should raised)
          (should shown))
      (kill-buffer " *leman-notify-show-test*"))))

 ;;;; Footer

(provide 'leman-notify-tests)

;;;; leman-notify-tests.el ends here
