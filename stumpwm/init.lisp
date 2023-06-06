;; TODO: Pull window from all windows
;; TODO: Font
;; TODO: Hide mouse
;; TODO: Override screen timeout when media is playing
;; TODO: Norwegian keys

;; General settings
(setf *message-window-gravity* :center
      *input-window-gravity* :center
      *mouse-focus-policy* :click
      stumpwm::*window-number-map* "123456789asdfjkl;")

;; Groups
(when *initializing*
    (grename "Web")
    (gnewbg "Code")
    (gnewbg "General")
    (gnewbg "Gaming"))

;; Colors
(setf *colors*
      '("#ffffff"        ; ^0 ; White
	"#131220"        ; ^1 ; Dark Blue
	"#f72f33"        ; ^2 ; Red
	"#689d6a"        ; ^3 ; Light Green
	"#62bfef"        ; ^4 ; Light Blue
        "#fabd2f"        ; ^5 ; Yellow / Help map keys
	"#a644bf"        ; ^6 ; Magenta
	"#cc4a0e"        ; ^7 ; Brown
	"#56b6c2"))      ; ^8 ; Cyan

(update-color-map (current-screen))

(defparameter *msg-bg-color* (nth 1 *colors*))
(defparameter *msg-fg-color* (nth 0 *colors*))
(defparameter *msg-border-color* (nth 0 *colors*))

(setf *window-format* "%m%n%s%20t")

;; Mode Line
(defparameter *mode-line-bg-color* (nth 1 *colors*))
(defparameter *mode-line-fg-color* (nth 0 *colors*))
(setf *mode-line-background-color* *mode-line-bg-color*)
(setf *mode-line-border-color* *mode-line-bg-color*)
(setf *mode-line-foreground-color* *mode-line-fg-color*)
; Default value: %a %b %e %K:%M%S
;(setf *time-modeline-string* "^2^f1^f0^n %H:%M")
(setf *time-modeline-string* "%a %b %e %k:%M:%S")

(setf *mode-line-timeout* 1)

(defvar *mode-line-media-string* "")

(defun set-mode-line-media-string ()
  (sb-thread:make-thread 
    (lambda ()
	(let ((volume-string (remove #\Newline (run-shell-command "pamixer --get-volume-human" t)))
	       (player-status (remove #\Newline (run-shell-command "playerctl status" t)))
	       (media-title (remove #\Newline (run-shell-command "playerctl metadata title" t))))
	    (setf *mode-line-media-string* (format nil "[^5 Volume: ~a ^n|^3 ~a:^4 ~a ^n]" volume-string player-status media-title)))))
  *mode-line-media-string*)

(setf *screen-mode-line-format*
      (list "^8[%g]^n "       ; groups
	    "%W"              ; windows
	    "^>"              ; right align
;;	    "%S"              ; swank status
;;	    "%B"              ; battery percentage
	    "%d "
	    '(:eval (set-mode-line-media-string))
))            ; time/date

;; Enable the mode line
(enable-mode-line (current-screen) (current-head) t)

(setf *startup-message* "^5 Welcome Home :)
^0 Happy Hacking!")

(set-bg-color *msg-bg-color*)
(set-fg-color *msg-fg-color*)
(set-border-color *msg-border-color*)

(defcommand set-random-wallpaper () ()
    (run-shell-command "wallpaper_directory=$HOME/wallpapers
    image=$(find $wallpaper_directory/* | sort -R | tail --lines=1)
    feh --bg-max \"$image\""))

; Source x profile
(run-shell-command "source ~/.xprofile")

; Environment variables
(defun setenv (var value)
  (setf (getenv var) value))

(setenv "BROWSER" "firefox")
(setenv "TERMINAL" "alacritty")
(setenv "XDG_DATA_DIRS" 
	(format nil "~a:~a" 
		(getenv "XDG_DATA_DIRS") 
		(format nil "~a~a" (getenv "HOME") "/.local/share/flatpak/exports/share")))

(when *initializing*
    (set-random-wallpaper))

(defcommand screensaver () ()
	    (run-shell-command "xscreensaver-command -activate"))

(defcommand find-window () ()
	    (run-shell-command "rofi -show window"))

(defcommand run-program () ()
  (run-shell-command "rofi -show drun"))

(defcommand play-pause () ()
    (run-shell-command "playerctl play-pause"))

(defcommand next-track () ()
    (run-shell-command "playerctl next"))

(defcommand previous-track () ()
    (run-shell-command "playerctl previous"))

(defcommand increase-volume () ()
    (run-shell-command "pamixer --increase 1"))

(defcommand decrease-volume () ()
    (run-shell-command "pamixer --decrease 1"))

(defcommand toggle-mute () ()
    (run-shell-command "pamixer --toggle-mute"))

(defcommand screenshot () ()
    (sb-thread:make-thread (lambda () 
	(run-shell-command "flameshot gui" t)
	(fclear)
	(pull-hidden-other))))

;; Menus of all windows in all groups
;; Use title-re-p to check
(defun find-window-native ()
  (let* (
	 (windows (act-on-matching-windows (w) (stumpwm::title-re-p w "") w)) 
	 (window-titles (loop for window in windows collect (slot-value window 'title)))
	 (selected-window-title (select-from-menu (current-screen) window-titles)))
    ; (selected-window (find windows :test (lambda (title window) (string= (car title) (slot-value window 'title))))))
    (run-or-raise "" `(:title ,(format nil "~a" selected-window-title)) T T)))

;; Key bindings
(set-prefix-key (kbd "s-space"))

; top level prefix keys
(defun r-define-key (key command)
    (define-key *root-map* (kbd key) command))

; top level keys
(defun t-define-key (key command)
    (define-key *top-map* (kbd key) command))

(defun tr-define-key (key command)
    (t-define-key (concat "s-" key) command)
    (r-define-key key command))

(defun set-window-focus-keys (left down up right)
    (tr-define-key left "move-focus left")
    (tr-define-key down "move-focus down")
    (tr-define-key up "move-focus up")
    (tr-define-key right "move-focus right"))

(defmacro set-gselect-prefix (prefix)
  `(progn
      ,@(loop for i from 1 to 9
            collect `(t-define-key ,(format nil "~a-F~a" prefix i) ,(format nil "gselect ~a" i)))))

(defmacro set-wselect-prefix (prefix)
  `(progn
      ,@(loop for i from 1 to 9
            collect `(t-define-key ,(format nil "~a-~a" prefix i) ,(format nil "select-window-by-number ~a" i)))))

(set-gselect-prefix "M")
(set-wselect-prefix "M")
(set-window-focus-keys "h" "j" "k" "l")
(tr-define-key "s" "vsplit")
(tr-define-key "v" "hsplit")
(tr-define-key "d" "remove-split")
(r-define-key "space" "run-program")
(r-define-key "w" "find-window")
(tr-define-key "F6" "play-pause")
(tr-define-key "F7" "next-track")
(tr-define-key "F5" "previous-track")
(tr-define-key "F3" "increase-volume")
(tr-define-key "F2" "decrease-volume")
(tr-define-key "F1" "toggle-mute")
(t-define-key "s-x" "screenshot")
