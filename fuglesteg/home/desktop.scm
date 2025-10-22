(define-module (fuglesteg home desktop)
  #:use-module (gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system asdf)
  #:use-module (guix git-download)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 binary-ports)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services syncthing)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (gnu home services shepherd)
  #:use-module (guix profiles)
  #:use-module (guix packages)
  #:use-module (nongnu packages chromium)
  #:use-module (nongnu packages chrome)
  #:use-module (nongnu packages mozilla)
  #:use-module (fuglesteg packages fonts)
  #:use-module (fuglesteg packages stumpwm)
  #:use-module (fuglesteg packages packup)
  #:use-module (fuglesteg home services development))

(use-package-modules wm vim video certs base gl lisp tmux rust-apps
                     terminals image-viewers xdisorg xorg tls
                     pulseaudio music image compton glib linux
                     web-browsers pdf freedesktop lisp-xyz sdl
                     package-management gnome-xyz syncthing gnuzilla)

(define sbcl-stumpwm-sdl-fonts
  (let ((commit "sdl2-ttf")
        (revision "1"))
    (package
     (name "sbcl-stumpwm-sdl-fonts")
     (version (git-version "0.0.1" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Fuglesteg/stumpwm-contrib.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08j4l0zar14jx5wzr2k0m8inxwjbj41gz1082ljg3f7yacc0li5v"))))
     (build-system asdf-build-system/sbcl)
     (inputs
      (list stumpwm
            sdl2
            sdl2-ttf
            sbcl-cffi))
     (arguments
      (list #:asd-systems ''("sdl-fonts")
            #:phases
            #~(modify-phases %standard-phases
                             (add-after 'unpack 'patch-sdl-dependencies
                                        (lambda _
                                          (substitute* "util/sdl-fonts/sdl-fonts.lisp"
                                                       (("libSDL2-2.0.so.0")
                                                        (string-append #$sdl2 "/lib/libSDL2.so"))
                                                       (("libSDL2_ttf-2.0.so.0")
                                                        (string-append #$sdl2-ttf "/lib/libSDL2_ttf.so"))))))))
     (home-page "https://github.com/stumpwm/stumpwm-contrib")
     (synopsis "StumpWM extra modules")
     (description "This package provides extra modules for StumpWM.")
     (license (list license:gpl2+ license:gpl3+ license:bsd-2)))))
  
(define desktop-packages (list obs rofi vlc xclip stumpwm sbcl-stumpwm-ttf-fonts
                               sbcl-stumpwm-sdl-fonts
                               sbcl-stumpwm-swm-gaps sbcl-stumpwm-stumptray sbcl-clx-xembed
                               sbcl-stumpwm-stump-regkey feh google-chrome-stable
                               zathura zathura-pdf-mupdf mupdf sbcl xset
                               xrandr nyxt firefox pavucontrol pulseaudio xrdb
                               pamixer playerctl flameshot bluez
                               p11-kit xdg-desktop-portal xdg-desktop-portal-gtk
                               picom flatpak-xdg-utils flatpak xdg-utils
                               xsetroot hackneyed-x11-cursors
                               bibata-cursor-theme alacritty))

(define-public desktop-home
               (home-environment
                 (packages desktop-packages)
                 (services
                   (list 
                     (service home-syncthing-service-type)
                     (service fuglesteg-development-service-type)
                     (simple-service 'fuglesteg-packup-service
                                     home-shepherd-service-type
                                     (list
                                      (shepherd-timer '(packup)
                                                      #~(calendar-event #:hours '(5)
                                                                        #:minutes '(0)
                                                                        #:days-of-week '(monday))
                                                      #~(#$(file-append packup "/bin/packup"))
                                                      #:documentation "Runs the packup command every week")))
                     (service home-redshift-service-type
                              (home-redshift-configuration
                                (location-provider 'manual)
                                (latitude 59.91)
                                (longitude 10.75)))
                     (service home-dbus-service-type)
                     (simple-service 'desktop-environment-variables
                                     home-environment-variables-service-type
                                     `(("BROWSER" . "firefox")
                                       ("TERMINAL" . "alacritty")
                                       ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share")))
                     (simple-service 'home-files 
                                     home-files-service-type
                                     `((".latexmkrc" ,(local-file "./latexmkrc"))
                                       (".xsession" ,(file-append stumpwm "/bin/stumpwm"))))
                     (simple-service 'config-files 
                                     home-xdg-configuration-files-service-type
                                     `(("alacritty" ,(local-file "./alacritty" #:recursive? #t))
                                       ("stumpwm" ,(local-file "./stumpwm" #:recursive? #t))
                                       ("nyxt" ,(local-file "./nyxt" #:recursive? #t))
                                       ("picom" ,(local-file "./picom" #:recursive? #t))))))))

desktop-home
