(define-module (fuglesteg home services packup)
  #:use-module (gnu services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages base)
  #:use-module (fuglesteg packages packup))

(define-public fuglesteg-packup-service-type
  (service-type
   (name 'fuglesteg-packup)
   (default-value #f)
   (description "Backup timed service using packup")
   (extensions
    (list (service-extension
           home-shepherd-service-type
           (lambda (config)
             (shepherd-timer '(packup)
                             #~(calendar-event #:hours '(5)
                                               #:minutes '(0)
                                               #:days-of-week '(monday))
                             #~(#$(file-append packup "/bin/packup")))))))))