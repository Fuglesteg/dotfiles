;; TODO: Hide mouse
;; TODO: Override screen timeout when media is playing
;; TODO: Norwegian keys


;; Quicklisp
(when *initializing*
    (load (merge-pathnames "quicklisp/setup.lisp"
			   (user-homedir-pathname))))

(setf *group-format* " %t ")
(setf *window-format* " %n %20t%m")

(require :asdf)
(asdf:clear-output-translations)
(asdf:initialize-output-translations
 '(:output-translations
   :enable-user-cache
   :ignore-inherited-configuration))

(require :clx-truetype)
(set-module-dir "~/.stumpwm.d/modules")
(init-load-path *module-dir*)
(load-module "ttf-fonts")
(when *initializing*
    ;; Fonts
    ; (require :ttf-fonts)
    ; ; (setf xft:*font-dirs* '(concat (getenv "HOME") "/.guix-profile/share/fonts/"))
    (setf xft:*font-dirs* '("/run/current-system/profile/share/fonts/"))
    ; ; (setf xft:*font-dirs* '("/usr/share/fonts/"))
    (setf clx-truetype:+font-cache-filename+ (concat (getenv "HOME") "/.fonts/font-cache.sexp"))
    (xft:cache-fonts)
    (set-font (make-instance 'xft:font :family "mononoki" :subfamily "Italic" :size 12)))

;; Tray
(ql:quickload :xembed)
(load-module "stumptray")
(when *initializing*
    (stumptray:stumptray))

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
    (grename "Web")
    (gnewbg "Code")
    (gnewbg "General")
    (gnewbg "Gaming"))

(if *initializing*
  (print "Initializing")
  (print "Not Initializing"))

;; Colors
(setf *colors*
      '("#ffffff"        ;
	"#111111"        ;
        "#adadad"        ;
        "#91ba74"        ;
	"#61afef"        ;
        "#fabd2f"        ;
	"#c678dd"        ;
	"#cc4a0e"        ;
	"#259fb6"))      ;

(update-color-map (current-screen))

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
	    (setf *mode-line-media-string* (format nil "^2[^n Volume: ~a ^2|^n ~a:^n ~a ^2]" volume-string player-status media-title)))))
  *mode-line-media-string*)

(setf *screen-mode-line-format*
      (list "^2[^n%g^2]^n "       ; groups
	    "%W"              ; windows
	    "^>"              ; right align
;;	    "%S"              ; swank status
;;	    "%B"              ; battery percentage
	    "%d "
	    '(:eval (set-mode-line-media-string))
	    "    "))

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

;; Hooks
(defun on-window-destroy (frame-to-frame)
  (repack-window-numbers))

(add-hook *destroy-window-hook* 'on-window-destroy)
; Source x profile
(run-shell-command "source ~/.xprofile")

; Environment variables
; (defun setenv (var value)
;   (setf (getenv var) value))

; (setenv "BROWSER" "firefox")
; (setenv "TERMINAL" "alacritty")
; (setenv "XDG_DATA_DIRS" 
; 	(format nil "~a:~a" 
; 		(getenv "XDG_DATA_DIRS") 
; 		(format nil "~a~a" (getenv "HOME") "/.local/share/flatpak/exports/share")))

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
  (format nil "~a | ~a" (group-name (slot-value window 'group)) (slot-value window 'title)))

(defcommand find-window () ()
  (let* (
	 (windows (get-all-windows)) 
	 (window-titles (loop for window in windows collect (slot-value window 'title)))
	 (selected-window-title (select-from-menu (current-screen) window-titles)))
    (run-or-raise "" `(:title ,(format nil "~a" selected-window-title)) T T)))

(defcommand pull-window () ()
  (let* (
	 (windows (get-all-windows))
	 (window-titles (loop for window in windows collect (list (format-window window) window)))
	 (selected-window-title (select-from-menu (current-screen) window-titles)))
    (when selected-window-title
	(move-windows-to-group (list (second selected-window-title)) (current-group))
	(focus-window (second selected-window-title)))))

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
