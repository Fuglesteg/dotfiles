(uiop:define-package :fugwm/mode-line
  (:use :cl
        :stumpwm
        :fugwm/utils
        :fugwm/commands
        :fugwm/media)
  (:export
   :init-mode-line))

(in-package :fugwm/mode-line)

(defvar *ml-time-string* "")

(defun ml-get-time-string ()
  (multiple-value-bind (seconds minutes hours day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore seconds))
    (format nil "^2󰸗 ~2,'0d.~2,'0d.~2,'0d | ^n ~2,'0d:~2,'0d" day month year hours minutes)))

(defun init-time-string-thread ()
  (sb-thread:make-thread (lambda ()
                           (loop
                             (sleep 1)
                             (setf *ml-time-string* (ml-get-time-string))))
                         :name "update-time-string"))

;;; Window click
(defun window-renumber (window new-number &key (group (current-group)))
  (let ((existing-window (find new-number
                               (group-windows group)
                               :key #'window-number
                               :test #'=)))
    (when existing-window
      (setf (window-number existing-window) (window-number window))
      (setf (window-number (group-current-window group)) new-number))))

(defun ml-window-on-click (code id &rest rest)
  (declare (ignore rest))
  (let ((button (stumpwm::decode-button-code code))
        (window (stumpwm::window-by-id id)))
    (case button
      ((:left-button)
       (when window
         (stumpwm::focus-all window)))
      ((:middle-button)
       (delete-window window))
      ((:wheel-up)
       (window-renumber window (clamp (1+ (window-number window)) 0 20)))
      ((:wheel-down)
       (window-renumber window (clamp (1- (window-number window)) 0 20))))))

(defun init-mode-line ()
  (init-time-string-thread)
  (init-update-media-thread)

  (let ((mode-line-bg-color (nth 1 *colors*))
        (mode-line-fg-color (nth 0 *colors*)))
    (setf *mode-line-background-color* mode-line-bg-color)
    (setf *mode-line-border-color* mode-line-bg-color)
    (setf *mode-line-foreground-color* mode-line-fg-color))

  (setf *mode-line-highlight-template* "^R~A^r")
  (setf *group-format* " %t ")
  (setf *window-format* " %n %20t%m")
  (setf *mode-line-timeout* 1)

  ;; Register on-click handlers
  (register-ml-on-click-id :ml-on-click-focus-window #'ml-window-on-click)
  (register-ml-on-click-id :ml-volume-on-click #'ml-volume-on-click)
  (register-ml-on-click-id :ml-player-on-click #'ml-player-on-click)
  (register-ml-on-click-id :ml-mic-on-click #'ml-mic-on-click)

  (ml-update-media-string-hard)

  ;; Tray
  (setf stumptray::*tray-win-background* (second *colors*))
  (setf stumptray::*tray-viwin-background* (second *colors*))
  (setf stumptray::*tray-hiwin-background* (second *colors*))
  (setf stumptray::*tray-cursor-color* (first *colors*))
  ;(stumptray:stumptray)

  (setf *screen-mode-line-format*
        (list "^2( ^n%g^2 )^n "       ; groups
              "%W"                    ; windows
              "^>"                    ; right align
              '(:eval *ml-time-string*)
              " "
              '(:eval *ml-media-string*)
              "%T"))                   ; stumptray

  ; Enable the mode line
  (enable-mode-line (current-screen) (current-head) t))
