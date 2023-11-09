;; TODO: Hide mouse
;; TODO: Override screen timeout when media is playing
;; TODO: Norwegian keys

;; Colors
(setf *colors*
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

;; Quicklisp
(when *initializing*
    (load (merge-pathnames "quicklisp/setup.lisp"
			   (user-homedir-pathname))))

; Fixes problem in Guix
(require :asdf)
(asdf:clear-output-translations)
(asdf:initialize-output-translations
 '(:output-translations
   :enable-user-cache
   :ignore-inherited-configuration))

(set-module-dir "~/.stumpwm.d/modules")
(init-load-path *module-dir*)

;; Fonts
(require :clx-truetype)
(load-module "ttf-fonts")
(when *initializing*
    ; (require :ttf-fonts)
    (setf xft:*font-dirs* '("/run/current-system/profile/share/fonts/"))
    (setf xft:*font-dirs* (list (concat (getenv "HOME") "/.guix-profile/share/fonts/")))
    ; ; (setf xft:*font-dirs* '("/usr/share/fonts/"))
    (setf clx-truetype:+font-cache-filename+ (concat (getenv "HOME") "/.fonts/font-cache.sexp"))
    (xft:cache-fonts)
    (set-font (make-instance 'xft:font :family "Mononoki Nerd Font" :subfamily "Bold" :size 16)))


;; General settings
(setf *message-window-gravity* :center
      *input-window-gravity* :center
      *mouse-focus-policy* :click
      *message-window-padding* 10
      *message-window-y-padding* 10
      stumpwm::*window-number-map* "qwertyuiop"
      stumpwm::*group-number-map* "123456789")

;; Groups
(when *initializing*
    (grename "󰖟 Web")
    (gnewbg "󰯂 Code")
    (gnewbg "󰗃 General")
    (gnewbg " Gaming"))

(if *initializing*
  (print "Initializing")
  (print "Not Initializing"))

(defparameter *msg-bg-color* (nth 1 *colors*))
(defparameter *msg-fg-color* (nth 0 *colors*))
(defparameter *msg-border-color* (nth 0 *colors*))


;; Mode Line
(defparameter *mode-line-bg-color* (nth 1 *colors*))
(defparameter *mode-line-fg-color* (nth 0 *colors*))
(setf *mode-line-background-color* *mode-line-bg-color*)
(setf *mode-line-border-color* *mode-line-bg-color*)
(setf *mode-line-foreground-color* *mode-line-fg-color*)
; Default value: %a %b %e %K:%M%S
(setf *time-modeline-string* "^2󰸗 %a %b %e | ^n %k:%M:%S")
(setf *mode-line-highlight-template* "^R~A^r")
(setf *group-format* " %t ")
(setf *window-format* " %n %20t%m")

(setf *mode-line-timeout* 1)

(defvar *mode-line-media-string* "")

(defun get-volume-icon (volume)
  (or
    (cdr
      (assoc
	volume
	'(("0%" . "󰝟")
	  ("muted" . "󰝟")
	  ("100%" . "󰕾"))
	:test #'equal))
    "󰖀"))

(defun format-volume (volume)
  (format nil "~a ~a" (get-volume-icon volume) volume))

(defun get-playing-status-icon (status)
  (or
    (cdr
      (assoc
	status
	'(("Playing" . "")
	  ("Paused" . ""))
	:test #'equal))
    ""))

(defun trim-string (string-to-trim desired-length)
  (if (< (length string-to-trim) desired-length)
    string-to-trim
    (concat (subseq string-to-trim 0 desired-length) "-")))

(defun set-mode-line-media-string ()
  (sb-thread:make-thread 
    (lambda ()
	(let ((volume-string 
		(format-volume (remove #\Newline (run-shell-command "pamixer --get-volume-human" t))))
	       (player-status (get-playing-status-icon (remove #\Newline (run-shell-command "playerctl status" t))))
	       (media-title (trim-string (remove #\Newline (run-shell-command "playerctl metadata title" t)) 20)))
	    (setf *mode-line-media-string* 
		  (format nil "^2[^n ~a ^2|^n ^2~a ^n ~a ^2]" volume-string player-status media-title)))))
  *mode-line-media-string*)

(setf *screen-mode-line-format*
      (list "^2[ ^n%g^2 ]^n "       ; groups
	    "%W"              ; windows
	    "^>"              ; right align
;;	    "%S"              ; swank status
;;	    "%B"              ; battery percentage
	    "%d "
	    '(:eval (set-mode-line-media-string))
	    "%T"))

; Enable the mode line
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

;; Hooks
(defun on-window-destroy (frame-to-frame)
  (repack-window-numbers))

(add-hook *destroy-window-hook* 'on-window-destroy)
; Source x profile
(run-shell-command "source ~/.xprofile")

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
(defun get-all-windows ()
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

(defun set-window-move-keys (prefix left down up right)
  (flet ((prepend-prefix (value) (format nil "~a-~a" prefix value)))
  (tr-define-key (prepend-prefix left) "move-window left")
  (tr-define-key (prepend-prefix down) "move-window down")
  (tr-define-key (prepend-prefix up) "move-window up")
  (tr-define-key (prepend-prefix right) "move-window right")))

(defmacro set-gselect-prefix (prefix)
  `(progn
      ,@(loop for i across stumpwm::*group-number-map*
            collect `(t-define-key ,(format nil "~a-~a" prefix i) ,(format nil "gselect ~a" i)))))

(defmacro set-wselect-prefix (prefix)
  `(progn
      ,@(loop for i across stumpwm::*window-number-map*
            collect `(t-define-key ,(format nil "~a-~a" prefix i) ,(format nil "select-window-by-number ~a" i)))))

(defcommand hello () ()
	    (window-send-string "ø"))


(set-gselect-prefix "M")
(set-wselect-prefix "M")
(set-window-focus-keys "h" "j" "k" "l")
(set-window-move-keys "M" "h" "j" "k" "l")
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
(tr-define-key "p" "pull-window")
(tr-define-key "f" "toggle-float")
(tr-define-key "a" "toggle-always-on-top")
(r-define-key "F10" "hello")

(defprogram-shortcut term
		     :command "exec alacritty"
		     :map *root-map*
		     :key (kbd "t"))

(defprogram-shortcut browser
		     :command "exec firefox"
		     :map *root-map*
		     :key (kbd "b"))

(defcommand loadguixrc () ()
	(load (merge-pathnames ".config/guix/home/stumpwm/init.lisp"
			   (user-homedir-pathname))))

;; Tray
(ql:quickload :xembed)
(load-module "stumptray")
(setf stumptray::*tray-win-background* (second *colors*))
(setf stumptray::*tray-viwin-background* (second *colors*))
(setf stumptray::*tray-hiwin-background* (second *colors*))
(setf stumptray::*tray-cursor-color* (first *colors*))
(when *initializing*
    (stumptray:stumptray))

;; Gaps
(load-module "swm-gaps")
(swm-gaps:toggle-gaps-on)
