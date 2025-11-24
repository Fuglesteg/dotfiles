(uiop:define-package #:fugwm/media
  (:use :cl
        :stumpwm
        :fugwm/utils)
  (:export
   :ml-update-media-string-hard
   :ml-update-media-string
   :ml-update-media-title
   :ml-update-player-status
   :*ml-media-string*
   :ml-player-on-click
   :ml-mic-on-click
   :ml-volume-on-click
   :init-update-media-thread))

(in-package :fugwm/media)

(defcommand play-pause () ()
  (run-shell-command "playerctl play-pause")
  (ml-update-player-status))

(defcommand next-track () ()
  (run-shell-command "playerctl next")
  (ml-update-media-title))

(defcommand previous-track () ()
  (run-shell-command "playerctl previous")
  (ml-update-media-title))

(defcommand increase-volume () ()
  (run-shell-command "pamixer --increase 1")
  (ml-update-volume))

(defcommand decrease-volume () ()
  (run-shell-command "pamixer --decrease 1")
  (ml-update-volume))

(defcommand toggle-mute () ()
  (run-shell-command "pamixer --toggle-mute")
  (ml-update-volume))

(defcommand toggle-mic-mute () ()
  (run-shell-command (format nil "pactl set-source-mute ~a toggle" (get-default-source)))
  (ml-update-mic))

;;; Mode line

;; Background thread for updating mode-line elements
(defun init-update-media-thread ()
  (sb-thread:make-thread (lambda ()
                           (loop
                             (sleep 5)
                             (ml-update-media-title)
                             (ml-update-player-status)))
                         :name "update-media-title"))

(defvar *ml-media-string* "")
(defvar *ml-volume* "")
(defvar *ml-mic* "")
(defvar *ml-player-status* "")
(defvar *ml-media-title* "")

;;; MIC
(defun get-microphone-icon (status)
  (string-case status
    ("no" "")
    ("yes" "")))

(defun ml-mic ()
  (let ((microphone-icon (get-microphone-icon (get-microphone-status))))
    (format-with-on-click-id (concat microphone-icon " ") :ml-mic-on-click nil)))

(defun ml-mic-on-click (code id &rest rest)
  (declare (ignore id rest))
  (let ((button (stumpwm::decode-button-code code)))
    (when (eq button :left-button)
      (toggle-mic-mute))))

(defun get-default-source ()
  (remove #\Newline (run-shell-command "pactl get-default-source" t)))

(defun get-microphone-status ()
  (string-left-trim "Mute: " (remove #\Newline (run-shell-command (format nil "pactl get-source-mute ~a" (get-default-source)) t))))

(defun ml-update-mic ()
  (setf *ml-mic* (ml-mic))
  (ml-update-media-string))

;;; VOLUME
(defun get-volume-icon (volume)
  (string-case volume
    ("0%" "󰝟")
    ("muted" "󰝟")
    ("100%" "󰕾")
    (t "󰖀")))

(defun get-volume ()
  (remove #\Newline (run-shell-command "pamixer --get-volume-human" t)))

(defun format-volume (volume)
  (format nil "~a ~a " (get-volume-icon volume) volume))

(defun ml-volume ()
  (format-with-on-click-id (format-volume (get-volume)) :ml-volume-on-click nil))

(defun ml-volume-on-click (code id &rest rest)
  (declare (ignore id rest))
  (let ((button (stumpwm::decode-button-code code)))
    (case button
      ((:left-button)
       (toggle-mute))
      ((:wheel-up)
       (increase-volume))
      ((:wheel-down)
       (decrease-volume)))))

(defun ml-update-volume ()
  (setf *ml-volume* (ml-volume))
  (ml-update-media-string))

;;; Player status
(defun get-playing-status-icon (status)
  (string-case status
    ("Playing" "")
    ("Paused" "")
    (t "")))

(defun ml-player-status ()
  (format-with-on-click-id
   (concat (get-playing-status-icon (remove #\Newline (run-shell-command "playerctl status" t))) " ")
   :ml-player-on-click nil))

(defun ml-update-player-status ()
  (setf *ml-player-status* (ml-player-status))
  (ml-update-media-string))

;;; Media title
(defun ml-media-title ()
  (format-with-on-click-id (trim-string (remove #\Newline (run-shell-command "playerctl metadata title" t)) 20) :ml-player-on-click nil))

(defun ml-update-media-title ()
  (setf *ml-media-title* (ml-media-title))
  (ml-update-media-string))

(defun ml-player-on-click (code id &rest rest)
  (declare (ignore id rest))
  (let ((button (stumpwm::decode-button-code code)))
    (case button
      ((:left-button)
       (play-pause))
      ((:wheel-up)
       (previous-track))
      ((:wheel-down)
       (next-track)))))

;;; Media string
(defun ml-update-media-string ()
  (setf *ml-media-string* (ml-media-string))
  (stumpwm::update-all-mode-lines))

(defun ml-media-string ()
  (format nil "^2(^n ~a~a^2|^n ^2~a^n~a ^2)" *ml-mic* *ml-volume* *ml-player-status* *ml-media-title*))

(defun ml-update-media-string-hard ()
  (ml-update-volume)
  (ml-update-mic)
  (ml-update-player-status)
  (ml-update-media-title))
