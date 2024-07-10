;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu home services)
             (gnu packages)
             (gnu packages vim)
             (gnu packages curl)
             (gnu packages base)
             (gnu packages tmux)
             (gnu packages admin)
             (gnu packages readline)
             (gnu packages terminals)
             (gnu packages rust-apps)
             (gnu services)
             (gnu home services syncthing)
             (guix gexp)
             (gnu home services shells))

(home-environment
  ;; Below is the list of packages that will show up in your
  ;; Home profile, under ~/.guix-home/profile.
  (packages (list curl
                  zoxide
                  rlwrap
                  ripgrep
                  htop
                  neovim
                  fzf
                  tmux
                  eza
                  alacritty
                  glibc-locales
                  (load "mononoki.scm")))

  ;; Below is the list of Home services.  To search for available
  ;; services, run 'guix home search KEYWORD' in a terminal.
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
                        (".bash_aliases" ,(local-file "./bash_aliases"))))
      (simple-service 'config-files 
                      home-xdg-configuration-files-service-type
                      `(("nvim" ,(local-file "./nvim" #:recursive? #t))
                        ("alacritty" ,(local-file "./alacritty" #:recursive? #t))
                        ("stumpwm" ,(local-file "./stumpwm" #:recursive? #t))
                        ("nyxt" ,(local-file "./nyxt" #:recursive? #t))
                        ("picom" ,(local-file "./picom" #:recursive? #t))))
      (service home-bash-service-type
               (home-bash-configuration
                 (bashrc (list (local-file "./bashrc" "bashrc")))
                 (bash-profile (list (local-file
                                       "./bash_profile"
                                       "bash_profile"))))))))
