(uiop:define-package :fugwm
  (:use :cl
        :stumpwm
        :fugwm/utils)
  (:export
   :init))

(in-package :fugwm)

;;; Hooks
(defun on-window-destroy (frame-to-frame)
  (declare (ignore frame-to-frame))
  (repack-window-numbers))

;; Programs
(defprogram-shortcut term
  :command "exec alacritty"
  :map *root-map*
  :key (kbd "t"))

(defprogram-shortcut browser
  :command "exec firefox"
  :map *root-map*
  :key (kbd "b"))

(defun init ()
  (setf stumpwm:*colors*
        '("#ffffff"
          "#111111"
          "#adadad"
          "#91ba74"
          "#61afef"
          "#fabd2f"
          "#c678dd"
          "#cc4a0e"
          "#259fb6"))

  (update-color-map (current-screen))

  ;; Fonts
  (let ((mononoki (sdl-fonts:load-font
                   (concat (getenv "HOME")
                           "/.guix-home/profile/share/fonts/truetype/MononokiNerdFont-Bold.ttf")
                   16
                   :hinting :mono)))
    (set-font mononoki))

  ;; General settings
  (setf *message-window-gravity* :center
        *input-window-gravity* :center
        *mouse-focus-policy* :click
        *message-window-padding* 10
        *message-window-y-padding* 10
        stumpwm::*window-number-map* "qwertyuiop"
        stumpwm::*group-number-map* "123456789")

  (setf *window-border-style* :tight
        *normal-border-width* 2)

  (grename "Main")

  (let ((msg-bg-color (nth 1 *colors*))
        (msg-fg-color (nth 0 *colors*))
        (msg-border-color (nth 0 *colors*)))
    (set-bg-color msg-bg-color)
    (set-fg-color msg-fg-color)
    (set-border-color msg-border-color))

  (setf *startup-message* "^5 Welcome Home :)
^0 Happy Hacking!")

  (run-shell-command "picom -b &
xsetroot -cursor_name left_ptr &
xrandr --output DisplayPort-2 --rate 144 --primary --output HDMI-A-0 --off &
xrdb -merge ~/.Xresources" t)

  (add-hook *destroy-window-hook* 'on-window-destroy)

  (fugwm/commands:set-random-wallpaper)

  (fugwm/mode-line:init-mode-line)
  (fugwm/keys:init-key-preferences)

  ; Head gaps run along the 4 borders of the monitor(s)
  (setf swm-gaps:*head-gaps-size* 0)

  ; Inner gaps run along all the 4 borders of a window
  (setf swm-gaps:*inner-gaps-size* 5)

  ; Outer gaps add more padding to the outermost borders of a window (touching the screen border)
  (setf swm-gaps:*outer-gaps-size* 20)

  (swm-gaps:toggle-gaps-on))
