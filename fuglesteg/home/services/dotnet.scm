(define-module (fuglesteg home services dotnet)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (sijo packages dotnet))

(define (home-dotnet-profile-service config)
  (list dotnet-10))

(define (home-dotnet-activation-service config)
  #~(invoke #$(file-append dotnet-10 "/bin/dotnet")
            "tool" "install" "--global" "--prerelease" "roslyn-language-server"))


(define (home-dotnet-variables-service config)
  `(("DOTNET_ENVIRONMENT" . "Development")))

(define-public fuglesteg-dotnet-service-type
  (service-type
   (name 'fuglesteg-dotnet)
   (default-value #f)
   (description "Dotnet development setup")
   (extensions
    (list (service-extension
           home-profile-service-type
           home-dotnet-profile-service)
          (service-extension
           home-environment-variables-service-type
           home-dotnet-variables-service)
          (service-extension
           home-activation-service-type
           home-dotnet-activation-service)))))
