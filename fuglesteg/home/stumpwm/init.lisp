;; TODO: Hide mouse - look at guix home service
;; TODO: Override screen timeout when media is playing

(in-package :stumpwm-user)

;; Require sb-cltl2 with pathname because some weird reason
(require :sb-cltl2 '(#P"/home/andy/.guix-profile/lib/sbcl/contrib/sb-cltl2.fasl"))

;; UTIL
(defmacro string-case (expression &rest forms)
  `(cond
     ,@(loop for form in forms collect
                `((string= ,expression ,(first form)) ,(second form)))))

(defun trim-string (string-to-trim desired-length)
  (if (< (length string-to-trim) desired-length)
      string-to-trim
      (concat (subseq string-to-trim 0 desired-length) "-")))

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

(when *initializing*
  (sb-thread:make-thread (lambda ()
                           (loop
                             (sleep 5)
                             (ml-update-media-title)
                             (ml-update-player-status)))
                         :name "update-media-title"))

; Fixes problem in Guix
#|
(require :asdf)
(asdf:clear-output-translations)

(asdf:initialize-output-translations
  '(:output-translations
     :enable-user-cache
     :ignore-inherited-configuration))
|#

(asdf:initialize-source-registry
  '(:source-registry
    (:tree "/home/andy/.guix-home/profile/share/common-lisp/")
    ;(:tree "/run/current-system/profile/share/common-lisp/")
    ;(:tree "/home/andy/.guix-profile/share/common-lisp/")
     :inherit-configuration))

;; Fonts
; ASDF throws a bunch of errors for some reason
(handler-bind ((uiop/lisp-build:compile-file-error (lambda (condition) 
                                                     (invoke-restart 'asdf:accept))))
  (asdf:load-system :ttf-fonts :force t))

(when *initializing*
    (setf xft:*font-dirs* `(,(concat (getenv "HOME") "/.guix-home/profile/share/fonts")))
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

(defvar *ml-media-string* "")
(defvar *ml-volume* "")
(defvar *ml-mic* "")
(defvar *ml-player-status* "")
(defvar *ml-media-title* "")

(setf *mode-line-timeout* 1)

;; MIC
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

;; VOLUME
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

;; Player status
(defun get-playing-status-icon (status)
  (or
   (cdr
    (assoc
     status
     '(("Playing" . "")
       ("Paused" . ""))
     :test #'equal))
   ""))

(defun ml-player-status ()
  (format-with-on-click-id (concat (get-playing-status-icon (remove #\Newline (run-shell-command "playerctl status" t))) " ") :ml-player-on-click nil))

(defun ml-update-player-status ()
  (setf *ml-player-status* (ml-player-status))
  (ml-update-media-string))

;; Media title
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

;; Media string
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

;; Register on-click handlers
(when *initializing*
  (register-ml-on-click-id :ml-volume-on-click #'ml-volume-on-click)
  (register-ml-on-click-id :ml-player-on-click #'ml-player-on-click)
  (register-ml-on-click-id :ml-mic-on-click #'ml-mic-on-click))

(when *initializing* (ml-update-media-string-hard))

(setf *screen-mode-line-format*
      (list "^2( ^n%g^2 )^n "       ; groups
	    "%W"                    ; windows
	    "^>"                    ; right align
            "%d "                   ; date and time
	    '(:eval *ml-media-string*)
	    "%T"))                  ; stumptray

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
(t-define-key "s-F4" "toggle-mic-mute")
(r-define-key "=" "balance-frames")

;; Norwegian keys

(asdf:load-system :stump-regkey)
(stump-regkey:register-keysym (first (xlib:character->keysyms #\å)))
(xlib:display-finish-output *display*)
(t-define-key "M-[" "window-send-string å")
(t-define-key "M-{" "window-send-string Å")
(stump-regkey:register-keysym (first (xlib:character->keysyms #\ø)))
(xlib:display-finish-output *display*)
(t-define-key "M-;" "window-send-string ø")
(t-define-key "M-:" "window-send-string Ø")
(stump-regkey:register-keysym (first (xlib:character->keysyms #\æ)))
(t-define-key "M-'" "window-send-string æ")
(t-define-key "M-\"" "window-send-string Æ")


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
(require "stumptray")
(setf stumptray::*tray-win-background* (second *colors*))
(setf stumptray::*tray-viwin-background* (second *colors*))
(setf stumptray::*tray-hiwin-background* (second *colors*))
(setf stumptray::*tray-cursor-color* (first *colors*))
(when *initializing*
    (stumptray:stumptray))

;; Gaps
(require "swm-gaps")
;; Head gaps run along the 4 borders of the monitor(s)
(setf swm-gaps:*head-gaps-size* 0)

;; Inner gaps run along all the 4 borders of a window
(setf swm-gaps:*inner-gaps-size* 5)

;; Outer gaps add more padding to the outermost borders of a window (touching the screen border)
(setf swm-gaps:*outer-gaps-size* 20)

(swm-gaps:toggle-gaps-on)


; initial keycode 47 (semicolon)
; (59 58 59 58 0 0 0)
