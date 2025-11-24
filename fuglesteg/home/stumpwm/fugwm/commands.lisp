(uiop:define-package :fugwm/commands
  (:use :cl
        :stumpwm
        :fugwm/utils))

(in-package :fugwm/commands)

(defcommand set-random-wallpaper () ()
  (run-shell-command "wallpaper_directory=$HOME/wallpapers
    image=$(find $wallpaper_directory/* | sort -R | tail --lines=1)
    feh --bg-max \"$image\""))

(defcommand bluetooth-off () ()
  (run-shell-command "bluetoothctl power off" t))

(defcommand start-micros () ()
  (asdf:load-system :micros)
  (uiop:symbol-call :micros :create-server :dont-close t :port 7777))

(defcommand bluetooth-on () ()
  (run-shell-command "bluetoothctl power on" t))

(defcommand screensaver-off () ()
  (run-shell-command "xset -dpms" t)
  (run-shell-command "xset s off" t))

(defcommand screensaver-on () ()
  (run-shell-command "xset +dpms" t)
  (run-shell-command "xset s on" t))

(defcommand connect-tv () ()
  (run-shell-command "xrandr --output HDMI-A-0 --primary --mode 3840x2160 --rate 120 --left-of DisplayPort-2" t)
  (run-shell-command "pacmd set-default-sink 11" t)
  (screensaver-off)
  (bluetooth-on)
  (sleep 4)
  (refresh-heads)
  (refresh-heads))

(defcommand disconnect-tv () ()
  (run-shell-command "xrandr --output HDMI-A-0 --off" t)
  (sleep 4)
  (refresh-heads))

(defcommand steam-big-picture () ()
  (run-shell-command "flatpak kill com.valvesoftware.Steam" t)
  (run-shell-command "flatpak run com.valvesoftware.Steam -bigpicture"))

(defcommand screensaver () ()
  (run-shell-command "xscreensaver-command -activate"))

(defcommand run-program () ()
  (run-shell-command "rofi -show drun"))

(defcommand screenshot () ()
  (sb-thread:make-thread
   (lambda ()
     (run-shell-command "flameshot gui" t)
     (fclear)
     (pull-hidden-other))))

(defun screen-brightness ()
  (parse-integer (run-shell-command "acpilight -get" t)))

(defun (setf screen-brightness) (brightness)
  (run-shell-command (format nil "acpilight -set ~a" brightness)
                     nil))

(defcommand screen-brightness-down () ()
  (decf (screen-brightness)))

(defcommand screen-brightness-up () ()
  (incf (screen-brightness)))

(defun get-all-windows ()
  "Get all windows in all groups"
  (act-on-matching-windows (w) (stumpwm::title-re-p w "") w))

(defun format-window (window)
  (format nil "~9,a | ~a" (group-name (slot-value window 'group)) (slot-value window 'title)))

(defun select-from-windows ()
  (let* ((windows (get-all-windows))
         (windows-list (loop for window in windows collect (list (format-window window) window))))
    (second (select-from-menu (current-screen) windows-list))))

(defcommand find-window () ()
  (let ((selected-window (select-from-windows)))
    (when selected-window
      (gselect (slot-value (slot-value selected-window 'group) 'stumpwm::name))
      (really-raise-window selected-window))))

(defcommand pull-window () ()
  (let ((selected-window (select-from-windows)))
    (when selected-window
      (move-windows-to-group (list selected-window) (current-group))
      (focus-window selected-window))))

(defcommand start-replay-buffer () ()
  (run-shell-command "obs --startreplaybuffer --minimize-to-tray"))

(defcommand toggle-float () ()
  (if (subtypep (class-of (current-window)) 'stumpwm::float-window)
      (unfloat-this)
      (float-this)))

(defcommand loadguixrc () ()
  (load (merge-pathnames ".dotfiles/fuglesteg/home/stumpwm/init.lisp"
                         (user-homedir-pathname))))

(defcommand repack-group-numbers () ()
  (let ((groups (sort (copy-seq
                       (remove-if-not (lambda (group)
                                        (<= 0 (group-number group)))
                                      (screen-groups (current-screen))))
                      #'<
                      :key #'group-number)))
    (loop for group in groups
          for group-number from 1
          do (setf (group-number group) group-number))))
