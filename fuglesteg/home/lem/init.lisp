(in-package :lem-user)

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

(defun vmap (key action)
  "Map to visual mode"
  (define-key lem-vi-mode:*visual-keymap* key action))

(define-command hover () ()
  (if (eq 'lem-lisp-mode/internal:lisp-mode 
          (lem:current-major-mode-at-point (current-point)))
      (lem-lisp-mode/internal::describe-symbol (symbol-string-at-point (current-point)))
      (lem-lsp-mode::lsp-hover)))

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

(vmap (format nil "~a ;" *leader*) 'lem/language-mode::comment-or-uncomment-region)

(lmap "e r" 'lem-lisp-mode:lisp-eval-defun)
(lmap "e e" 'lem-lisp-mode/eval::lisp-eval-at-point)
(lmap "e f" 'lem-lisp-mode:lisp-compile-and-load-file)

(lmap "T" 'lem/frame-multiplexer:frame-multiplexer-create-with-new-buffer-list)

(lmap "1" 'lem/frame-multiplexer:frame-multiplexer-switch-0)
(lmap "2" 'lem/frame-multiplexer:frame-multiplexer-switch-1)
(lmap "3" 'lem/frame-multiplexer:frame-multiplexer-switch-2)
(lmap "4" 'lem/frame-multiplexer:frame-multiplexer-switch-3)
(lmap "5" 'lem/frame-multiplexer:frame-multiplexer-switch-4)
(lmap "6" 'lem/frame-multiplexer:frame-multiplexer-switch-5)
(lmap "7" 'lem/frame-multiplexer:frame-multiplexer-switch-6)
(lmap "8" 'lem/frame-multiplexer:frame-multiplexer-switch-7)
(lmap "9" 'lem/frame-multiplexer:frame-multiplexer-switch-8)

(nmap "g d" 'lem/language-mode:find-definitions)
(nmap "g r" 'lem/language-mode:find-references)
(nmap "g h" 'hover)

(nmap "c-L" 'lem-paredit-mode:paredit-slurp)
(nmap "c-H" 'lem-paredit-mode:paredit-barf)

;;; Search

(defvar *last-used-collector*)

(defmethod initialize-instance :after ((collector lem/peek-source::collector) &key)
  (setf *last-used-collector* collector))

(define-command open-last-search () ()
  (lem/peek-source::display *last-used-collector*))

(lmap "s r" 'open-last-search)
(lmap "s g" 'lem/grep::project-grep)

;;; SDL2 frontend

(defun lem-sdl2-p ()
  (boundp 'lem-sdl2/display::*display*))

;(sdl2-ffi.functions:sdl-set-window-opacity
; (lem-sdl2/display:display-window (lem-sdl2/display:current-display)) 1.0)

;; Font
(when (lem-sdl2-p)
  (let ((font-regular #P"/home/andy/.guix-home/profile/share/fonts/truetype/MononokiNerdFont-Regular.ttf")
        (font-bold #P"/home/andy/.guix-home/profile/share/fonts/truetype/MononokiNerdFont-Bold.ttf"))
    (when (and (uiop:file-exists-p font-regular)
               (uiop:file-exists-p font-bold))
      (lem-sdl2/display:change-font (lem-sdl2/display:current-display) 
                                    (lem-sdl2/font:make-font-config 
                                     :latin-normal-file font-regular
                                     :latin-bold-file font-bold
                                     :cjk-normal-file font-regular
                                     :cjk-bold-file font-bold)))))