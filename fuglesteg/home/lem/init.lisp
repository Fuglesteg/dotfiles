(in-package :lem-user)

;(sdl2-ffi.functions:sdl-set-window-opacity
 ;(lem-sdl2/display:display-window (lem-sdl2/display:current-display)) 0.1)

(lem-vi-mode:vi-mode)

(defvar *leader* "Space")

(defun imap (key action)
  "Map to insert mode"
  (define-key lem-vi-mode:*insert-keymap* key action))

(defun nmap (key action)
  "Map to normal mode"
  (define-key lem-vi-mode:*normal-keymap* key action))

(defun lmap (key action)
  "Map to leader"
  (nmap (format nil "~a ~a" *leader* key) action))

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


(lmap "Tab" 'lem:previous-buffer)

(lmap ";" 'lem/language-mode::comment-or-uncomment-region)

(lmap "e r" 'lem-lisp-mode:lisp-eval-defun)
(lmap "e e" 'lem-lisp-mode:lisp-eval-last-expression)
(lmap "e f" 'lem-lisp-mode:lisp-compile-and-load-file)

(nmap "c-L" 'lem-paredit-mode:paredit-slurp)
(nmap "c-H" 'lem-paredit-mode:paredit-barf)

;; Font
(let ((font-regular #P"/home/andy/.guix-profile/share/fonts/truetype/MononokiNerdFont-Regular.ttf")
      (font-bold #P"/home/andy/.guix-profile/share/fonts/truetype/MononokiNerdFont-Bold.ttf"))
  (when (and (uiop:file-exists-p font-regular)
             (uiop:file-exists-p font-bold))
    (lem-sdl2/display:change-font (lem-sdl2/display:current-display) 
                                  (lem-sdl2/font:make-font-config 
                                   :latin-normal-file font-regular
                                   :latin-bold-file font-bold
                                   :cjk-normal-file font-regular
                                   :cjk-bold-file font-bold))))