(define-module (fuglesteg home services development)
               #:use-module (ice-9 binary-ports)
               #:use-module (ice-9 rdelim)
               #:use-module (gnu)
               #:use-module (gnu home services)
               #:use-module (gnu home services shells)
               #:use-module (gnu packages)
               #:use-module (fuglesteg packages fonts)
               #:use-module (fuglesteg packages vim)
               #:export (fuglesteg-development-service-type))

(use-package-modules version-control base rust-apps web
                     tmux terminals curl syncthing text-editors
                     readline admin compression)

(define (home-development-profile-service config)
  (list curl syncthing coreutils
        rlwrap ripgrep
        htop neovim fzf glibc-locales
        git zoxide fd
        tmux unzip eza lem font-nerd-mononoki))

(define (home-development-variables-service config)
  `(("VISUAL" . "nvim")
    ("GUIX_LOCPATH" . "$HOME/.guix-home/profile/lib/locale")
    ("PATH" . ,(string-append "$PATH:" "$HOME/.local/bin:"))
    ("EDITOR" . "nvim")))

(define (home-development-files-service config)
                      `((".tmux.conf" ,(local-file "../tmux.conf"))
                        (".tmux-set-colors.conf" ,(local-file "../tmux-set-colors.conf"))
                        (".vimrc" ,(local-file "../vimrc"))
                        (".lem/init.lisp" ,(local-file "../lem/init.lisp"))
                        (".gitconfig" ,(local-file "../gitconfig"))
                        (".inputrc" ,(local-file "../inputrc"))
                        (".sbclrc" ,(local-file "../sbclrc"))))

(define (home-development-xdg-configuration-files-service config)
  `(("nvim" ,(local-file "../nvim" #:recursive? #t))))

(define (home-development-bash-configuration config)
  (home-bash-extension
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
               ("sshf" . ,(let ((filename "./fuglesteg-server-ip.secret"))
                            (if (file-exists? filename)
                                (string-append "ssh " (call-with-input-file filename read-line))
                                "echo \"Not configured with server address\"")))
               ("sbcl" . "rlwrap sbcl --noinform")))
    (bashrc (list (local-file "../bashrc" "bashrc")))
    (bash-profile (list (local-file
                          "../bash_profile"
                          "bash_profile")))))

(define-public fuglesteg-development-service-type
               (service-type
                  (name 'fuglesteg-development)
                  (default-value #f)
                  (description "Development files and packages")
                  (extensions
                    (list (service-extension
                            home-profile-service-type
                            home-development-profile-service)
                          (service-extension
                            home-environment-variables-service-type
                            home-development-variables-service)
                          (service-extension
                            home-files-service-type
                            home-development-files-service)
                          (service-extension
                            home-xdg-configuration-files-service-type
                            home-development-xdg-configuration-files-service)
                          (service-extension
                            home-bash-service-type
                            home-development-bash-configuration)))))
