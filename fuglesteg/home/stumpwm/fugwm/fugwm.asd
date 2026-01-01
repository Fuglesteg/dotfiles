(asdf:defsystem :fugwm
  :depends-on (:stumpwm
               :stump-regkey
               :sdl-fonts
               :swm-gaps
               :stumptray)
  :serial t
  :components ((:file "utils")
               (:file "commands")
               (:file "media")
               (:file "laptop" :if-feature :laptop)
               (:file "keys")
               (:file "mode-line")
               (:file "group-templates")
               (:file "fugwm")))
