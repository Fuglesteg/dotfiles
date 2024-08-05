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
               #:use-module (guix profiles)
               #:use-module (guix packages)
               #:use-module (nongnu packages chromium)
               #:use-module (nongnu packages mozilla)
               #:use-module (fuglesteg packages fonts)
               #:use-module (fuglesteg packages lem)
               #:use-module (fuglesteg packages stumpwm)
               #:use-module (fuglesteg home services development))

(use-package-modules wm vim video certs base gl lisp tmux rust-apps 
                     terminals image-viewers xdisorg xorg 
                     pulseaudio music image compton
                     web-browsers pdf freedesktop
                     package-management gnome-xyz syncthing gnuzilla)

(define desktop-packages (list obs rofi vlc xclip stumpwm sbcl-stumpwm-ttf-fonts
                               sbcl-stumpwm-swm-gaps sbcl-stumpwm-stumptray sbcl-clx-xembed
                               sbcl-stumpwm-stump-regkey
                               zathura zathura-pdf-mupdf mupdf
                               xrandr nyxt firefox pavucontrol pulseaudio
                               pamixer playerctl flameshot icedove
                               picom flatpak-xdg-utils flatpak
                               xsetroot hackneyed-x11-cursors xdg-utils
                               bibata-cursor-theme alacritty))

;; TODO: Service that symlinks ~/.xsession to ~/.guix-home/profile/bin/stumpwm
(define-public desktop-home
               (home-environment
                 (packages desktop-packages)
                 (services
                   (list 
                     (service home-syncthing-service-type)
                     (service fuglesteg-development-service-type)
                     (simple-service 'set-environment-variables
                                     home-environment-variables-service-type
                                     `(("BROWSER" . "firefox")
                                       ("TERMINAL" . "alacritty")
                                       ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share")))
                     (simple-service 'home-files 
                                     home-files-service-type
                                     `((".latexmkrc" ,(local-file "./latexmkrc"))))
                     (simple-service 'config-files 
                                     home-xdg-configuration-files-service-type
                                     `(("alacritty" ,(local-file "./alacritty" #:recursive? #t))
                                       ("stumpwm" ,(local-file "./stumpwm" #:recursive? #t))
                                       ("nyxt" ,(local-file "./nyxt" #:recursive? #t))
                                       ("picom" ,(local-file "./picom" #:recursive? #t))))))))

desktop-home
