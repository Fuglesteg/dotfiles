(define-module (fuglesteg home services development)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages)
  #:use-module (gnu packages nss)
  #:use-module (fuglesteg packages fonts)
  #:use-module (fuglesteg packages vim)
  #:use-module (fuglesteg packages lem)
  #:export (fuglesteg-development-service-type))

(use-package-modules version-control base rust-apps web ssh node
                     tmux terminals curl syncthing text-editors
                     readline admin compression certs)

(define (home-development-profile-service config)
  (list curl syncthing coreutils node
        rlwrap ripgrep nss-certs openssh
        htop neovim fzf glibc-locales
        git zoxide fd nvim-telescope-fzf-native
        tmux unzip eza lem-latest font-nerd-mononoki))

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

(define blink-cmp-fuzzy
  (origin (method url-fetch)
          (uri "https://github.com/Saghen/blink.cmp/releases/download/v1.5.1/x86_64-unknown-linux-gnu.so")
          (sha256 (base32 "1hy2csh6bmhpp20dw0g0r0931gwps4qfwvfc5zicd7xjw32s7jrl"))))

(define (home-development-xdg-data-files-service config)
  `(("nvim/lazy/blink.cmp/target/release/libblink_cmp_fuzzy.so" ,blink-cmp-fuzzy)
    ("nvim/lazy/telescope-fzf-native.nvim/build/libfzf.so"
     ,(file-append nvim-telescope-fzf-native "/share/lib/libfzf.so"))))

(define (home-development-bash-configuration config)
  (home-bash-extension
   (aliases `(("dcd" . "docker compose down")
              ("dcu" . "docker compose up")
              ("dps" . "docker ps")
              ("tree" . "eza --icons --tree")
              ("ls" . "eza -l --icons")
              ("info" . "info --vi-keys")
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

(define (home-development-activation-service config)
  #~(invoke "npm" "i" "-g"
                      "typescript" "typescript-language-server"
                      "@vue/language-server" "@vue/typescript-plugin"))

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
           home-xdg-data-files-service-type
           home-development-xdg-data-files-service)
          (service-extension
           home-bash-service-type
           home-development-bash-configuration)
          (service-extension
           home-activation-service-type
           home-development-activation-service)))))
