(define-module (fuglesteg k80)
               #:use-module (gnu)
               #:use-module (gnu home)
               #:use-module (gnu packages avahi)
               #:use-module (gnu services)
               #:use-module (gnu services xorg)
               #:use-module (gnu system pam)
               #:use-module (gnu services networking)
               #:use-module (gnu services dbus)
               #:use-module (gnu services vpn)
               #:use-module (gnu services guix)
               #:use-module (gnu services avahi)
               #:use-module (gnu services sound)
               #:use-module (gnu system setuid)
               #:use-module (gnu system nss)
               #:use-module (guix packages)
               #:use-module (guix git-download)
               #:use-module (guix channels)
               #:use-module (nongnu packages linux)
               #:use-module (fuglesteg home desktop))

(use-service-modules cups desktop networking ssh linux
                     docker xorg)

(use-package-modules fonts vim video certs docker networking
                     xdisorg kde-graphics libusb nfs
                     gl xorg version-control linux)

(define-public k80
               (operating-system
                 (kernel linux)
                 (firmware (cons* linux-firmware
                                  broadcom-bt-firmware
                                  amd-microcode
                                  amdgpu-firmware
                                  %base-firmware))
                 (locale "en_US.utf8")
                 (timezone "Europe/Oslo")
                 (keyboard-layout (keyboard-layout "us"))
                 (host-name "K80")
                 (groups (cons* (user-group (name "sudo"))
                                %base-groups))
                 (users (cons* (user-account
                                 (name "andy")
                                 (comment "Andreas Fuglesteg Dale")
                                 (group "users")
                                 (home-directory "/home/andy")
                                 (supplementary-groups '("wheel" "netdev" "audio" "kvm"
                                                         "video" "docker" "sudo" "input")))
                               %base-user-accounts))
                 (name-service-switch %mdns-host-lookup-nss)
                 (packages (cons* git
                                  vim
                                  v4l2loopback-linux-module ; Used for virtual camera in OBS
                                  avahi
                                  xf86-video-amdgpu
                                  mesa
                                  xf86-input-wacom
                                  krita
                                  %base-packages))
                 (kernel-loadable-modules (list v4l2loopback-linux-module))
                 (initrd-modules (cons "kvm_amd" %base-initrd-modules)) ; Virual machine kernel module
                 (services
                   (cons*
                     (service kernel-module-loader-service-type
                              '("v4l2loopback" "amdgpu" "kvm"))
                     (service openssh-service-type)
                     (service cups-service-type)
                     (service bluetooth-service-type)
                     (service docker-service-type)
                     (service containerd-service-type)
                     (service static-networking-service-type
                              (list (static-networking
                                     (addresses (list (network-address
                                                       (device "enp4s0")
                                                       (value "192.168.0.80/24"))))
                                     (routes (list (network-route
                                                    (destination "default")
                                                    (gateway "192.168.0.1"))))
                                     (name-servers '("1.0.0.1" "1.1.1.1")))))
                     (service wireguard-service-type
                              (wireguard-configuration
                               ; Fun Eel
                               (interface "wg-mullvad-osl")
                               (private-key "/home/andy/wg/mullvad-private.key")
                               (addresses '("10.74.79.129/32" "fc00:bbbb:bbbb:bb01::b:4f80/128"))
                               (dns '("10.64.0.1"))
                               (peers
                                (list
                                 (wireguard-peer
                                  (name "no-osl-001")
                                  (endpoint "176.125.235.71:51820")
                                  (public-key "jOUZjMq2PWHDzQxu3jPXktYB7EKeFwBzGZx56cTXXQg=")
                                  (allowed-ips '("0.0.0.0/0")))))))
                     (service wireguard-service-type
                              (wireguard-configuration
                               (interface "wg-k8")
                               (private-key "/home/andy/wg/k8-private.key")
                               (addresses '("10.0.0.80/32"))
                               (peers
                                (list
                                 (wireguard-peer
                                  (name "K8")
                                  (preshared-key "/home/andy/wg/k8-preshared.key")
                                  (endpoint "k8.fuglesteg.net:51820")
                                  (public-key "Z2Inejk/94vVSbaEwJm5y+undnfOR8ADky9zidVTHEU=")
                                  (allowed-ips '("10.0.0.8/24"))
                                  (keep-alive 25))))))
                     (service guix-home-service-type
                              `(("andy" ,desktop-home)))
                     (simple-service 'guix-moe guix-service-type
                                     (guix-extension
                                      (authorized-keys
                                       (list (plain-file "guix-moe-old.pub"
                                                         "(public-key (ecc (curve Ed25519) (q #374EC58F5F2EC0412431723AF2D527AD626B049D657B5633AAAEBC694F3E33F9#)))")
                                             ;; New key since 2025-10-29.
                                             (plain-file "guix-moe.pub"
                                                         "(public-key (ecc (curve Ed25519) (q #552F670D5005D7EB6ACF05284A1066E52156B51D75DE3EBD3030CD046675D543#)))")))
                                      (substitute-urls
                                       '("https://cache-cdn.guix.moe"))))
                     (service gnome-keyring-service-type)
                     #;(simple-service 'guix-nonguix guix-service-type
                                     (guix-extension
                                      (authorized-keys
                                       (list (plain-file "non-guix.pub"
                                                         "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))")))
                                       (substitute-urls
                                        (list "https://substitutes.nonguix.org"))))
                     (simple-service 'k8-host
                                     hosts-service-type
                                     (list (host "10.0.0.8" "k8")))
                     ;; For Sims 3 (lol)
                     (service pam-limits-service-type
                              (list
                               (pam-limits-entry "andy" 'hard 'nofile 524288)))
                     (udev-rules-service 'keychron-rules
                                         (udev-rule "99-keychron.rules"
                                                          (string-append "KERNEL==\"hidraw*\", SUBSYSTEM==\"hidraw\", ATTRS{idVendor}==\"3434\", ATTRS{idProduct}==\"d048\","
                                                                         "MODE=\"0660\", GROUP=\"users\", TAG+=\"uaccess\", TAG+=\"udev-acl\"")))
                     (service slim-service-type
                              (slim-configuration
                               (xorg-configuration
                               (xorg-configuration
                                (resolutions '((3840 2160) (1920 1080)))
                                (extra-config '("Section \"Device\""
                                                "Identifier \"AMD\""
                                                "Driver \"amdgpu\""
                                                "Option \"TearFree\" \"true\""
                                                "Option \"DRI\" \"3\""
                                                "Option \"VariableRefresh\" \"true\""
                                                "EndSection"))))))
                     ;;; Stolen from %desktop-services
                     (simple-service 'mtp udev-service-type (list libmtp))
                     ;; Add udev rules and default backends for scanners.
                     (service sane-service-type)


                     ;; Allow desktop users to also mount NTFS and NFS file systems
                     ;; without root.
                     (simple-service 'mount-setuid-helpers privileged-program-service-type
                                     (map file-like->setuid-program
                                          (list (file-append nfs-utils "/sbin/mount.nfs")
                                                (file-append ntfs-3g "/sbin/mount.ntfs-3g"))))

                     ;; The global fontconfig cache directory can sometimes contain
                     ;; stale entries, possibly referencing fonts that have been GC'd,
                     ;; so mount it read-only.
                     fontconfig-file-system-service

                     ;; Sudo "wheel" polkit service
                     polkit-wheel-service

                     ;; The D-Bus clique.
                     (service avahi-service-type)
                     (service udisks-service-type)
                     (service upower-service-type)
                     (service accountsservice-service-type)
                     (service cups-pk-helper-service-type)
                     (service colord-service-type)
                     (service geoclue-service-type)
                     (service polkit-service-type)
                     (service elogind-service-type)
                     (service dbus-root-service-type)

                     (service ntp-service-type)

                     (service x11-socket-directory-service-type)

                     (service pulseaudio-service-type)
                     (service alsa-service-type)
                     %base-services))
                     (bootloader (bootloader-configuration
                                           (bootloader grub-efi-removable-bootloader)
                                           (targets (list "/boot/efi"))
                                           (keyboard-layout keyboard-layout)))
                             (swap-devices (list (swap-space
                                                   (target (uuid
                                                             "e8e45812-542b-4ff3-baac-1fb7b5c34aa3")))))
                             (file-systems (cons* (file-system
                                                    (mount-point "/boot/efi")
                                                    (device (uuid "49F5-6AE8"
                                                                  'fat32))
                                                    (type "vfat"))
                                                  (file-system
                                                    (mount-point "/")
                                                    (device (uuid
                                                              "02796792-2e71-4dfa-a014-6daaf49156fc"
                                                              'ext4))
                                                    (type "ext4")) %base-file-systems))))

k80
