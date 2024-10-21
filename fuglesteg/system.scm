(define-module (fuglesteg system)
               #:use-module (gnu)
               #:use-module (gnu home)
               #:use-module (gnu services)
               #:use-module (gnu services guix)
               #:use-module (gnu system setuid)
               #:use-module (guix packages)
               #:use-module (guix channels)
               #:use-module (nongnu packages linux)
               #:use-module (fuglesteg home desktop))

(use-service-modules cups desktop networking ssh linux
                     docker xorg)

(use-package-modules fonts vim video certs docker networking
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
                                                         "video" "docker" "sudo")))
                               %base-user-accounts))
                 (packages (cons* git vim
                                  v4l2loopback-linux-module ; Used for virtual camera in OBS
                                  xf86-video-amdgpu mesa
                                  %base-packages))
                 (kernel-loadable-modules (list v4l2loopback-linux-module))
                 (initrd-modules (cons "kvm_amd" %base-initrd-modules)) ; Virual machine kernel module
                 (services
                   (append (list
                             (service kernel-module-loader-service-type
                                      '("v4l2loopback" "amdgpu" "kvm"))
                             (service openssh-service-type)
                             (service cups-service-type)
                             (service bluetooth-service-type)
                             (service docker-service-type)
                             (service containerd-service-type)
                             (service guix-home-service-type
                                      `(("andy" ,desktop-home))))
                           (modify-services %desktop-services
                                            (gdm-service-type
                                              config =>
                                              (gdm-configuration
                                                (inherit config)
                                                (xorg-configuration 
                                                  (xorg-configuration
                                                    (resolutions '((3840 2160) (1920 1080)))
                                                    (extra-config '(
                                                                    "Section \"Device\"
                                                                    Identifier \"AMD\"
                                                                    Driver \"amdgpu\"
                                                                    Option \"TearFree\" \"true\"
                                                                    Option \"DRI\" \"3\"
                                                                    Option \"VariableRefresh\" \"true\"
                                                                    EndSection"))))))
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
                                                                                  %default-authorized-guix-keys)))))))
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
