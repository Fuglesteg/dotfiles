(use-modules 
  (guix packages)
  (guix download)
  ((guix licenses) #:prefix license:)
  (guix build-system font))

(define-public font-nerd-mononoki
  (package
    (name "font-nerd-mononoki")
    (version "v3.0.2")
    (source (origin
             (method url-fetch)
             (uri (string-append 
                    "https://github.com/ryanoasis/nerd-fonts/releases/download/"
                    version
                    "/Mononoki.zip"))
             (sha256
              (base32
               "13mj185jnwnipwh0gwl36p6jkdjxz034vnfmc6nwgdsrci1yxpa1"))))
    (build-system font-build-system)
    (home-page "https://dejavu-fonts.github.io/")
    (synopsis "Vera font family derivate with additional characters")
    (description "DejaVu provides an expanded version of the Vera font family
aiming for quality and broader Unicode coverage while retaining the original
Vera style.  DejaVu currently works towards conformance to the Multilingual
European Standards (MES-1 and MES-2) for Unicode coverage.  The DejaVu fonts
provide serif, sans and monospaced variants.")
    (license license:gpl3+)))

font-nerd-mononoki
