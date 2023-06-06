(in-package :lem-user)

(lem-vi-mode:vi-mode)

(defvar *leader* "Space")

(defun imap (key action)
  "Map to insert mode"
  (define-key lem-vi-mode:*insert-keymap* key action))

(defun nmap (key action)
  "Map to normal mode"
  (define-key lem-vi-mode:*command-keymap* key action))

(defun lmap (key action)
  "Map to leader"
  (nmap (format nil "~a ~a" *leader* key) action))

(defun hello ()
  "Test"
  (format t "Hello"))

(lmap "w v" 'lem:split-active-window-horizontally)
(lmap "w s" 'lem:split-active-window-vertically)
(lmap "w h" 'lem:window-move-left)
(lmap "w j" 'lem:window-move-down)
(lmap "w k" 'lem:window-move-up)
(lmap "w l" 'lem:window-move-right)
(lmap "w d" 'lem:delete-active-window)

(lmap "b" 'lem/list-buffers:list-buffers)

(lmap "Space" 'lem:find-file)

(lmap "f s" 'lem:save-current-buffer)
(lmap "f S" 'lem:save-some-buffers)

(lmap "Tab" 'lem:switch-to-buffer)

;(lmap ";" 'lem:comment)

(lmap "e r" 'lem-lisp-mode:lisp-eval-defun)
(lmap "e e" 'lem-lisp-mode:lisp-eval-last-expression)
;(lmap "e f" (lambda () (lem-lisp-mode:lisp-load-file (lem:buffer-filename))))
;(lmap "e d" (lambda () (lem:execute-command "lisp-load-file")))
(lmap "l" 'lem-paredit-mode:paredit-forward)
(lmap "h" 'lem-paredit-mode:paredit-backward)
(lmap "L" 'lem-paredit-mode:paredit-slurp)
(lmap "H" 'lem-paredit-mode:paredit-barf)
