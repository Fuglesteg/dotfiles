(define-module (fuglesteg kip)
  #:use-module (fuglesteg home desktop)
  #:use-module (fuglesteg services vpn)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services pm)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu services desktop)
  #:use-module (gnu services ssh)
  #:use-module (gnu services guix)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services linux)
  #:use-module (gnu services xorg)
  #:use-module (nongnu packages video)
  #:use-module (nongnu packages linux))

(operating-system
 (kernel linux)
 (firmware (cons* linux-firmware
                  intel-microcode
                  ibt-hw-firmware
                  iwlwifi-firmware
                  i915-firmware
                  %base-firmware))
 (locale "en_US.utf8")
 (timezone "Europe/Oslo")
 (keyboard-layout (keyboard-layout "us"
                                   #:options '("esc:nocaps")))
 (host-name "kip")
 (groups (cons* (user-group (name "sudo"))
                %base-groups))
 (users (cons* (user-account
                (name "andy")
                (comment "Andreas Fuglesteg Dale")
                (group "users")
                (home-directory "/home/andy")
                (supplementary-groups (list "wheel" "netdev" "audio"
                                            "kvm" "video" "sudo")))
               (user-account
                (name "dineo")
                (comment "Dineo Joy Seabi")
                (group "users")
                (home-directory "/home/dineo")
                (supplementary-groups (list "netdev" "audio" "video")))
               (user-account
                (name "mina")
                (comment "Mina Fuglesteg Dale")
                (group "users")
                (home-directory "/home/mina")
                (supplementary-groups (list "netdev" "audio" "video")))
               %base-user-accounts))
 (name-service-switch %mdns-host-lookup-nss)
 (packages (cons* mesa
                  light
                  gnome-software
                  intel-media-driver/nonfree
                  ;xf86-input-synaptics
                  xf86-input-wacom
                  %base-packages))
 (services
  (cons* (service guix-home-service-type
                  `(("andy" ,desktop-home)))
         (service openssh-service-type
                  (openssh-configuration
                   (password-authentication? #f)
                   (authorized-keys
                    `(("andy" ,(local-file "k80.pub"))))))
         (service tlp-service-type)
         (service bluetooth-service-type)
         (fuglesteg-wireguard-service #:private-key "/home/andy/wg/fuglesteg-private.key"
                                      #:address "10.0.0.1/24"
                                      #:peers (list peer-k8
                                                    peer-k80))
         (fuglesteg-hosts-service)
         (service gnome-desktop-service-type)
         (udev-rules-service 'light-rules light)
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
         (modify-services %desktop-services
                          (gdm-service-type
                           config =>
                           (gdm-configuration
                            (inherit config)
                            (xorg-configuration
                             (xorg-configuration
                              (resolutions '((1920 1080)))
                              (extra-config '("Section \"Device\""
                                              "Identifier \"Intel Graphics\""
                                              "Driver \"modesetting\""
                                              ;"Option \"TearFree\" \"true\""
                                              "EndSection"

                                              "Section \"InputClass\""
                                              "Identifier \"touchpad catchall\""
                                              "Driver \"synaptics\""
                                              "MatchIsTouchpad \"on\""
                                              "MatchDevicePath \"/dev/input/event*\""
                                              "Option \"TapButton1\" \"1\""
                                              "Option \"TapButton2\" \"2\""
                                              "Option \"TapButton3\" \"3\""
                                              "Option \"CircularScrolling\" \"on\""
                                              "Option \"CircScrollStrigger\" \"0\""
                                              "Option \"CircularPad\" \"on\""
                                              "EndSection")))))))))
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
