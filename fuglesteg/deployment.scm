(define-module (fuglesteg deployment)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (fuglesteg kit))

(list (machine
       (operating-system kit)
       (environment managed-host-environment)
       (configuration (machine-ssh-configuration
                       (host-name "kit")
                       (system "x86_64-linux")
                       (system "andy")
                       (identity "/home/andy/.ssh/id_rsa")))))
