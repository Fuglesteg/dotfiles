(in-package :lem-user)

(import 'lem-vi-mode/options:option-value)
(import 'lem-vi-mode/options:get-option)

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
  (if (or (mode-active-p (current-buffer) 'lem-lisp-mode:lisp-mode)
          (mode-active-p (current-buffer) 'lem-lisp-mode/internal::lisp-repl-mode))
      (lem-lisp-mode/internal::describe-symbol (symbol-string-at-point (current-point)))
      (lem-lsp-mode::lsp-hover)))

(mode-active-p (current-buffer) 'lem-lisp-mode:lisp-mode)

(setf (option-value (get-option "scrolloff")) 8)

(lmap "w v" 'lem:split-active-window-horizontally)
(lmap "w s" 'lem:split-active-window-vertically)
(lmap "w h" 'lem:window-move-left)
(lmap "w j" 'lem:window-move-down)
(lmap "w k" 'lem:window-move-up)
(lmap "w l" 'lem:window-move-right)
(lmap "w d" 'lem:delete-active-window)

(lmap "b" 'lem/list-buffers:list-buffers)

(lmap "Space" 'lem-core/commands/project:project-find-file)

(lmap "f s" 'lem:save-current-buffer)
(lmap "f S" 'lem:save-some-buffers)
(lmap "f p" 'lem-core/commands/project:project-root-directory)
(lmap "f f" 'lem/directory-mode/commands:find-file-directory)

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
(nmap "g D" 'lem-lsp-mode/lsp-mode::lsp-type-definition)
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


;;; Modes

;; Vue-mode
(setf lem-vue-mode:*vue-language-server-location* (merge-pathnames ".local/lib/node_modules/@vue/language-server"
                                                                   (uiop/cl:user-homedir-pathname)))

;;; Visual

(load-theme "circus")

(setf (variable-value 'lem-lisp-mode/paren-coloring:paren-coloring :global)
      t)

(defun lem-sdl2-p ()
  (boundp 'lem-sdl2/display::*display*))

;; Broken opacity
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

#+nil
(define-command lisp-quickload (system-name)
    ((prompt-for-symbol-name "System: " (buffer-package (current-buffer))))
  (check-connection)
  (eval-with-transcript `(,(uiop:find-symbol* :quickload :quicklisp) ,(string system-name))))

#+nil
(defmacro with-source-registry (source-tree &body body)
  `(let (asdf/source-registry:*source-registry*
         (asdf/system-registry:*registered-systems* (make-hash-table :test 'equal))
         asdf:*source-registry-parameter*
         (asdf:*default-source-registries* nil))
     (asdf:initialize-source-registry ,source-tree)
     ,@body))

#+nil
(defun unloaded-systems ()
  (remove-if (lambda (system)
               (member system (asdf:already-loaded-systems)
                       :test #'string=))
             (asdf:registered-systems)))

#+nil
(define-command load-local-asdf () ()
  (lem-lisp-mode:lisp-eval
   '(progn
     (require 'asdf)
     (with-source-registry
         `(:source-registry
           (:tree ,(first (directory #P".")))
           (:ignore-inherited-configuration))
       (asdf:registered-systems)))))

#+nil
(let ((asdf:*default-source-registries* nil))
  (with-source-registry
      (:source-registry
       (:directory #P"/home/andy/code/cl-guile")
       (:ignore-inherited-configuration))
    (asdf:registered-systems)))
