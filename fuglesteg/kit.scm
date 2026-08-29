(define-module (fuglesteg kit)
  #:use-module (fuglesteg home desktop)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system nss)
  #:use-module (gnu services desktop)
  #:use-module (gnu services ssh)
  #:use-module (gnu services pm)
  #:use-module (gnu services guix)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu services linux)
  #:use-module (gnu services containers)
  #:use-module (gnu services xorg)
  #:use-module (gnu services docker)
  #:use-module (gnu services dbus)
  #:use-module (gnu services networking)
  #:use-module (nongnu packages nvidia)
  #:use-module (nongnu services nvidia)
  #:use-module (nongnu packages linux))

(operating-system
 (kernel linux)
 (firmware (cons* linux-firmware
                  intel-microcode
                  broadcom-bt-firmware
                  broadcom-sta
                  %base-firmware))
 (kernel-arguments (list "modprobe.blacklist=b43,b43legacy,ssb,bcm43xx,brcm80211,brcmfmac,brcmsmac,bcma,nouveau"
                         "nvidia_drm.modeset=1"))
 (kernel-loadable-modules (list broadcom-sta))
 (locale "en_US.utf8")
 (timezone "Europe/Oslo")
 (keyboard-layout (keyboard-layout "no"))
 (host-name "kit")
 (groups (cons* (user-group (name "sudo"))
                %base-groups))
 (users (cons* (user-account
                (name "andy")
                (comment "Andreas Fuglesteg Dale")
                (group "users")
                (home-directory "/home/andy")
                (supplementary-groups (list "wheel" "netdev" "audio" "kvm" "video" "sudo")))
               (user-account
                (name "dineo")
                (comment "Dineo Joy Seabi")
                (group "users")
                (home-directory "/home/dineo")
                (supplementary-groups (list "netdev" "audio" "video")))
               %base-user-accounts))
 (name-service-switch %mdns-host-lookup-nss)
 (packages (cons*
            gnome-software
            xdg-desktop-portal
            xdg-desktop-portal-gtk
            xdg-desktop-portal-gnome
            xdg-desktop-portal-kde
            xdg-dbus-proxy
            xdg-utils
            p11-kit
            flatpak
            flatpak-xdg-utils
            shared-mime-info
            (list glib "bin")
            hackneyed-x11-cursors
            bibata-cursor-theme
            %base-packages))
 (services (cons* (service guix-home-service-type
                           `(("andy" ,desktop-home)))
                  (service nvidia-service-type)
                  (service openssh-service-type
                           (openssh-configuration
                            (password-authentication? #f)
                            (authorized-keys
                             `(("andy" ,(local-file "k80.pub"))))))
                  (service docker-service-type)
                  (service containerd-service-type)
                  (service gnome-desktop-service-type)
                  ; Required for rootless podman
                  (service iptables-service-type)
                  (service rootless-podman-service-type
                           (rootless-podman-configuration
                            (subgids
                             (list (subid-range (name "dineo"))
                                   (subid-range (name "andy"))))
                            (subuids
                             (list (subid-range (name "dineo"))
                                   (subid-range (name "andy"))))))
                  (service tlp-service-type)
                  (service bluetooth-service-type)
                  (modify-services %desktop-services
                                   (guix-service-type
                                    config =>
                                    (guix-configuration
                                     (inherit config)
                                     (substitute-urls
                                      (append (list "https://substitutes.nonguix.org")
                                              %default-substitute-urls))
                                     (authorized-keys
                                      (append (list (plain-file "non-guix.pub"
                                                                "(public-key
                                                                   (ecc
                                                                    (curve Ed25519)
                                                                    (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
                                              %default-authorized-guix-keys))))
                                   (gdm-service-type
                                    config =>
                                    (gdm-configuration
                                     (inherit config)
                                     (wayland? #t)
                                     (auto-suspend? #f))))))
 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets (list "/boot/efi"))
              (keyboard-layout keyboard-layout)))
 (swap-devices (list (swap-space
                      (target (file-system-label "SWAP")))))
 (file-systems (cons* (file-system
                       (mount-point "boot/efi")
                       (device (file-system-label "BOOT"))
                       (type "vfat"))
                      (file-system
                       (mount-point "/")
                       (device (file-system-label "ROOT"))
                       (type "ext4"))
                      %base-file-systems)))
