(uiop:define-package :fugwm/keys
  (:use :cl :stumpwm :fugwm/utils)
  (:export
   :init-key-preferences))

(in-package :fugwm/keys)

;; Programs
(defprogram-shortcut term
  :command "exec alacritty"
  :map *root-map*
  :key (kbd "t"))

(defprogram-shortcut browser
  :command "exec firefox"
  :map *root-map*
  :key (kbd "b"))

;; top level prefix keys
(defun r-define-key (key command)
  (define-key *root-map* (kbd key) command))

;; top level keys
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

(defun set-gselect-prefix (prefix)
  (loop for i across stumpwm::*group-number-map*
        do (t-define-key (format nil "~a-~a" prefix i)
                         (format nil "gselect ~a" i))))

(defun set-wselect-prefix (prefix)
  (loop for i across stumpwm::*window-number-map*
        do (t-define-key (format nil "~a-~a" prefix i)
                         (format nil "select-window-by-number ~a" i))))

;; Norwegian keys
(defcommand register-norwegian-keys () ()
  (stump-regkey:register-character #\å)
  (stump-regkey:register-character #\ø)
  (stump-regkey:register-character #\æ))

(defun init-key-preferences ()
  (set-prefix-key (kbd "s-space"))
  (register-norwegian-keys)

  (t-define-key "M-[" "window-send-string å")
  (t-define-key "M-{" "window-send-string Å")
  (t-define-key "M-;" "window-send-string ø")
  (t-define-key "M-:" "window-send-string Ø")
  (t-define-key "M-'" "window-send-string æ")
  (t-define-key "M-\"" "window-send-string Æ")

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

  ;; Input

  (define-key *input-map* (kbd "Tab") 'stumpwm::input-complete-forward)
  (define-key *input-map* (kbd "S-Tab") 'stumpwm::input-complete-backward)
  (define-key *input-map* (kbd "C-j") 'stumpwm::input-complete-forward)
  (define-key *input-map* (kbd "C-k") 'stumpwm::input-complete-backward)
  (define-key *input-map* (kbd "C-h") 'stumpwm::input-backward-char)
  (define-key *input-map* (kbd "C-l") 'stumpwm::input-forward-char)

  (define-key *menu-map* (kbd "C-j") 'stumpwm::menu-down)
  (define-key *menu-map* (kbd "C-k") 'stumpwm::menu-up)
  (define-key *menu-map* (kbd "Tab") 'stumpwm::menu-down)
  (define-key *menu-map* (kbd "S-Tab") 'stumpwm::menu-up))
