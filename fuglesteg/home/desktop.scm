(define-module (fuglesteg home desktop)
  #:use-module (gnu)
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
  #:use-module (fuglesteg home services packup)
  #:use-module (fuglesteg home services development))

(use-package-modules wm vim video certs base gl lisp tmux rust-apps
                     terminals image-viewers xdisorg xorg tls
                     pulseaudio music image compton glib linux
                     web-browsers pdf freedesktop lisp-xyz
                     package-management gnome-xyz syncthing gnuzilla)

(define desktop-packages (list obs rofi vlc xclip stumpwm sbcl-stumpwm-ttf-fonts
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
