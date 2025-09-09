(define-module (fuglesteg kit)
  #:use-module (fuglesteg home desktop)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu services desktop)
  #:use-module (gnu services ssh)
  #:use-module (gnu services guix)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages xml)
  #:use-module (gnu services linux)
  #:use-module (gnu services xorg)
  #:use-module (nongnu packages nvidia)
  #:use-module (nongnu services nvidia)
  #:use-module (nongnu packages linux))

(define new-gnome-meta-core-services
  (package/inherit gnome-meta-core-services
                   (propagated-inputs
                    (modify-inputs
                     (package-propagated-inputs gnome-meta-core-services)
                     (replace
                      "evolution-data-server"
                      (let ((base evolution-data-server))
                        (package/inherit base
                                         (build-system trivial-build-system)
                                         (source #f)
                                         (arguments
                                          (list
                                           #:modules '((guix build utils)
                                                       (guix build union))
                                           #:builder
                                           #~(begin
                                               (use-modules (guix build union))
                                               (use-modules (guix build utils))
                                               (union-build
                                                #$output
                                                (list
                                                 #$(this-package-input "evolution-data-server"))
                                                #:create-all-directories? #t)
                                               (delete-file-recursively (string-append #$output "/lib"))
                                               (delete-file-recursively (string-append #$output "/include")))))
                                         (inputs
                                          (list base))
                                         ;; Drop propagation of Requires.private from .pc file for application
                                         (propagated-inputs '()))))))))

(operating-system
 (kernel linux)
 (firmware (cons* linux-firmware
                  broadcom-bt-firmware
                  %base-firmware))
 (kernel-arguments (list "modprobe.blacklist=b43,b43legacy,ssb,bcm43xx,brcm80211,brcmfmac,brcmsmac,bcma"))
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
            %base-packages))
 (services (cons* (service guix-home-service-type
                           `(("andy" ,desktop-home)))
                  (service openssh-service-type
                           (openssh-configuration
                            (password-authentication? #f)
                            (authorized-keys
                             `(("andy" ,(local-file "k80.pub"))))))
                  (service gnome-desktop-service-type
                           (gnome-desktop-configuration
                            (core-services ((@@ (gnu services desktop) extract-propagated-inputs)
                                            new-gnome-meta-core-services))))
                  (modify-services %desktop-services
                                   (guix-service-type
                                    config =>
                                    (guix-configuration
                                     (inherit config)
                                     (substitute-urls
                                      (append (list "https://substitutes.nonguix.org")
                                              #;(list "https://nonguix-proxy.ditigal.xyz")
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
