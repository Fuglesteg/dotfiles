;(define-module (fuglesteg home-services desktop)
(use-modules (gnu)
             (ice-9 rdelim)
             (ice-9 binary-ports)
             (gnu home)
             (gnu home services)
             (gnu home services desktop)
             (gnu home services syncthing)
             (gnu home services shells)
             (gnu packages)
             (gnu services)
             (guix gexp)
             (guix profiles)
             (guix packages)
             (nongnu packages chromium)
             (nongnu packages mozilla)
             (fuglesteg packages fonts)
             (fuglesteg packages lem)
             (fuglesteg packages stumpwm))

(use-package-modules fonts wm vim video certs version-control linux base
                     gl lisp tmux rust-apps terminals image-viewers
                     xdisorg xorg pulseaudio music image compton
                     web-browsers lisp-xyz pdf freedesktop
                     package-management gnome-xyz curl syncthing
                     readline admin compression gnuzilla)

(define development-packages (list curl zoxide syncthing
                                   feh rlwrap ripgrep
                                   htop neovim fzf glibc-locales
                                   git zoxide xrandr
                                   tmux unzip eza lem font-nerd-mononoki))

;; TODO: Service that symlinks ~/.xsession to ~/.guix-home/profile/bin/stumpwm
(home-environment
 (packages (append development-packages))
  (services
    (list 
      (service home-syncthing-service-type)
      (simple-service 'set-environment-variables
                      home-environment-variables-service-type
                      `(("BROWSER" . "firefox")
                        ("TERMINAL" . "alacritty")
                        ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share")
                        ("VISUAL" . "nvim")
                        ("GUIX_LOCPATH" . "$HOME/.guix-home/profile/lib/locale")
                        ("PATH" . ,(string-append "$PATH:" "$HOME/.local/bin:"
                                          "$HOME/.cargo/bin:"
                                          "$HOME/.local/share/bob/nvim-bin"))
                        ("EDITOR" . "nvim")))
      (simple-service 'home-files 
                      home-files-service-type
                      `((".tmux.conf" ,(local-file "./tmux.conf"))
                        (".tmux-set-colors.conf" ,(local-file "./tmux-set-colors.conf"))
                        (".vimrc" ,(local-file "./vimrc"))
                        (".latexmkrc" ,(local-file "./latexmkrc"))
                        (".lem/init.lisp" ,(local-file "./lem/init.lisp"))
                        (".gitconfig" ,(local-file "./gitconfig"))
                        (".inputrc" ,(local-file "./inputrc"))))
      (simple-service 'config-files 
                      home-xdg-configuration-files-service-type
                      `(("nvim" ,(local-file "./nvim" #:recursive? #t))
                        ("alacritty" ,(local-file "./alacritty" #:recursive? #t))
                        ("stumpwm" ,(local-file "./stumpwm" #:recursive? #t))
                        ("nyxt" ,(local-file "./nyxt" #:recursive? #t))
                        ("picom" ,(local-file "./picom" #:recursive? #t))))
      (service home-bash-service-type
               (home-bash-configuration
                 (aliases `(("apt" . "sudo apt")
                            ("apti" . "sudo apt install")
                            ("cc" . "gcc")
                            ("dcd" . "docker compose down")
                            ("dcu" . "docker compose up")
                            ("dps" . "docker ps")
                            ("l" . "ls -CF")
                            ("la" . "ls -A")
                            ("ll" . "ls -alF")
                            ("ls" . "eza -l --icons")
                            ("neovide" . "neovide --multigrid")
                            ("sshf" . ,(string-append "ssh " (call-with-input-file "./fuglesteg-server-ip.secret" read-line)))))
                 (bashrc (list (local-file "./bashrc" "bashrc")))
                 (bash-profile (list (local-file
                                       "./bash_profile"
                                       "bash_profile"))))))))
