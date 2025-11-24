;;;; Config file for stumpwm
;;;; TODO: Hide mouse - look at guix home service
;;;; TODO: Override screen timeout when media is playing
;;;; TODO: Group templates
;;;; TODO: Guix integrations:
;;;;     - Profile per group
;;;;     - Load packages into group

(in-package :stumpwm-user)

; Require sb-cltl2 with pathname because some weird reason
(require :sb-cltl2 '(#P"/home/andy/.guix-home/profile/lib/sbcl/contrib/sb-cltl2.fasl"))

; Initialize asdf source to guix home dependencies
(asdf:initialize-source-registry
  `(:source-registry
    (:tree ,(concat (getenv "HOME") "/.guix-home/profile/share/common-lisp/"))
     :inherit-configuration))

; Add hostname to *features*
(let ((host (intern (string-upcase (uiop:hostname)) "KEYWORD"))
      (laptops (list :kip :kit)))
  (when (member host laptops)
    (pushnew :laptop *features*))
  (pushnew host *features*))

(asdf:load-asd (merge-pathnames "stumpwm/fugwm/fugwm.asd"
                                (parse-namestring (concatenate 'string
                                                               (uiop:getenv "XDG_CONFIG_HOME")
                                                               "/"))))
(asdf:load-system :fugwm)
(fugwm:init)
