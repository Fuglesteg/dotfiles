;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu packages)
             (gnu services)
             (guix gexp)
             (gnu home services shells))

(home-environment
  ;; Below is the list of packages that will show up in your
  ;; Home profile, under ~/.guix-home/profile.
  (packages (specifications->packages (list "curl"
                                            "font-mononoki"
                                            "python"
                                            "obs"
                                            "emacs"
                                            "syncthing"
                                            "mesa"
                                            "rlwrap"
                                            "xclip"
                                            "xpdf"
                                            "zathura-pdf-mupdf"
                                            "mupdf"
                                            "ungoogled-chromium"
                                            "ripgrep"
                                            "htop"
                                            "xrandr"
                                            "nyxt"
                                            "firefox"
                                            "pavucontrol"
                                            "pulseaudio-qt"
                                            "pulseaudio"
                                            "neovim"
                                            "fzf"
                                            "flatpak-xdg-utils"
                                            "flatpak"
                                            "xsetroot"
                                            "hackneyed-x11-cursors"
                                            "xdg-utils"
                                            "bibata-cursor-theme"
                                            "tmux"
                                            "unzip"
                                            "exa"
                                            "alacritty")))

  ;; Below is the list of Home services.  To search for available
  ;; services, run 'guix home search KEYWORD' in a terminal.
  (services
   (list (service home-bash-service-type
                  (home-bash-configuration
                   (aliases '(("apt" . "sudo apt")
                              ("apti" . "sudo apt install")
                              ("cc" . "gcc")
                              ("dcd" . "docker compose down")
                              ("dcu" . "docker compose up")
                              ("dps" . "docker ps")
                              ("l" . "ls -CF")
                              ("la" . "ls -A")
                              ("ll" . "ls -alF")
                              ("ls" . "exa -l --icons")
                              ("neovide" . "neovide --multigrid")
                              ("sshf" . "ssh andy@fuglesteg.mywire.org")))
                   (bashrc (list (local-file "./.bashrc" "bashrc")))
                   (bash-profile (list (local-file
                                        "./.bash_profile"
                                        "bash_profile"))))))))
