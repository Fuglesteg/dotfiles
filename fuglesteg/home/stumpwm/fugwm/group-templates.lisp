(uiop:define-package :fugwm/group-templates
  (:use :cl :stumpwm :fugwm/utils))

(in-package :fugwm/group-templates)

(defclass group-template ()
  ((name
    :accessor group-template-name
    :initarg :name
    :initform "group"
    :type string)
   (group
    :accessor group-template-group
    :initarg :group)))

(defmethod initialize-instance :after ((group-template group-template) &key &allow-other-keys)
  (initialize-group group-template))

(defgeneric initialize-group (group-template))

(defmethod initialize-group :before ((group-template group-template))
  (with-accessors ((name group-template-name) (group group-template-group)) group-template
    (setf group (gnew name))))

(defclass nif-vue (group-template) ())

(defmethod initialize-group ((nif-vue nif-vue))
  (with-accessors ((name group-template-name)) nif-vue
    (run-shell-command (format nil "alacritty -e guix shell node -- bash -sc 'cd ~~/code/NIF/~a && ~~/.dotfiles/fuglesteg/home/services/scripts/tmux-vue'" name))
    (run-shell-command "firefox --new-window https://dev.minidrett.no")
    (hsplit)))

(defun group-templates ()
  (sb-mop:class-direct-subclasses (find-class 'group-template)))

(defcommand gnew-from-template () ()
  (let ((template (cdr (select-from-menu (current-screen) (mapcar
                                                           (lambda (template)
                                                             `(,(string (class-name template)) . ,template))
                                                           (group-templates))))))
    (make-instance template :name (read-one-line (current-screen) "Group name: "))))
