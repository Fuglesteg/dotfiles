#|(define-module (fuglesteg home wsl)
               #:use-module (gnu)
               #:use-module (gnu home)
               #:use-module (gnu home services)
               #:use-module (gnu home services syncthing)
               #:use-module (gnu packages)
               #:use-module (gnu services)
               #:use-module (fuglesteg home services development))
|#

(use-modules (fuglesteg home services development)
             (gnu)                                    
             (gnu home)
             (gnu home services)
             (gnu home services syncthing)
             (gnu packages)
             (gnu services))

(use-modules (fuglesteg home services development))

(define-public wsl-home 
               (home-environment
                 (services
                   (list 
                     (service home-syncthing-service-type)
                     (service fuglesteg-development-service-type)))))

wsl-home
