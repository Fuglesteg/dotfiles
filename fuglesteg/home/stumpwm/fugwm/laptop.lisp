(uiop:define-package :fugwm/laptop
  (:use :cl
        :stumpwm
        :fugwm/utils)
  (:export
   :*ml-screen-brightness-string*
   :ml-update-screen-brightness
   :ml-screen-brightness-on-click
   :init-battery-level-thread
   :ml-battery-level))

(in-package :fugwm/laptop)

;;; Screen brightness

(defun screen-brightness ()
  (parse-integer (run-shell-command "light" t)
                 :junk-allowed t))

(defun (setf screen-brightness) (new-value)
  (let ((new-value (clamp new-value 1 100)))
    (run-shell-command (format nil "light -S ~a"
                               new-value)
                       t))
  (ml-update-screen-brightness))

(defcommand screen-brightness-increase () ()
  (incf (screen-brightness) 2))

(defcommand screen-brightness-decrease () ()
  (decf (screen-brightness) 2))

(defcommand screen-brightness-set (brightness) ((:number "New brightness: "))
  (setf (screen-brightness) brightness))

(defcommand screen-brightness-current () ()
  (message (screen-brightness)))

(defvar *screen-brightness* 10)
(defun screen-brightness-update ()
  (setf *screen-brightness* (screen-brightness)))

(defvar *ml-screen-brightness-string* "")

(defun ml-screen-brightness ()
  (format-with-on-click-id (format nil "󰃞 ~a" *screen-brightness*) :ml-screen-brightness-on-click nil))

(defun ml-update-screen-brightness ()
  (screen-brightness-update)
  (setf *ml-screen-brightness-string* (ml-screen-brightness))
  (stumpwm::update-all-mode-lines))

(defun ml-screen-brightness-on-click (code id &rest rest)
  (declare (ignorable id rest))
  (case (stumpwm::decode-button-code code)
    (:wheel-up (screen-brightness-increase))
    (:wheel-down (screen-brightness-decrease))))

;;; Battery level

(defun battery-level ()
  (parse-integer (uiop:read-file-string #P"/sys/class/power_supply/BAT1/capacity")))

(defvar *battery-level* 100)

(defun update-battery-level ()
  (setf *battery-level* (battery-level)))

(defun ml-battery-level ()
  (format nil "󱊣 ~a%" *battery-level*))

(defun init-battery-level-thread ()
  (sb-thread:make-thread
   (lambda ()
     (loop
       (update-battery-level)
       (sleep 60)))
   :name "Update battery level"))
