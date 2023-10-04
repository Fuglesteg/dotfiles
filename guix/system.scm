;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.


;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu)
  (gnu services docker)
  (gnu services desktop)
  (gnu services xorg)
  (gnu packages lisp)
  (gnu packages tmux)
  (gnu packages rust-apps)
  (gnu packages terminals)
  (gnu packages image-viewers)
  (gnu packages xdisorg)
  (gnu packages xorg)
  (gnu packages pulseaudio)
  (gnu packages music)
  (gnu packages image)
  (gnu packages compton)
  (gnu packages web-browsers)
  (nongnu packages mozilla)
  (nongnu packages linux))

(use-service-modules cups desktop networking ssh)
(use-package-modules fonts wm vim video certs version-control)

(operating-system
  (kernel linux)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "Europe/Oslo")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "K80")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "andy")
                  (comment "Andreas Fuglesteg Dale")
                  (group "users")
                  (home-directory "/home/andy")
                  (supplementary-groups '("wheel" "netdev" "audio" "video" "docker")))
                %base-user-accounts))

  ;; Packages installed system-wide.  Users can also install packages
  ;; under their own account: use 'guix search KEYWORD' to search
  ;; for packages and 'guix install PACKAGE' to install a package.
  (packages (append (list (specification->package "nss-certs"))
		    (list sbcl stumpwm `(,stumpwm "lib"))
                    (list neovim tmux zoxide alacritty git) ; Terminal tools
                    (list feh xrandr rofi pamixer playerctl xscreensaver flameshot picom) ; Desktop utils
                    (list nyxt firefox) ; Browser
		    (list sbcl-stumpwm-ttf-fonts font-dejavu)
                    %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (append (list

                 ;; To configure OpenSSH, pass an 'openssh-configuration'
                 ;; record as a second argument to 'service' below.
                 (service openssh-service-type)
                 ;(service network-manager-service-type)
                 ;(service wpa-supplicant-service-type)
                 ;(service ntp-service-type)
                 (service cups-service-type)
                 (service bluetooth-service-type)
                 (service docker-service-type))
		 ;(service gdm-service-type))

           ;; This is the default list of services we
           ;; are appending to.
           %desktop-services))
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))
  (swap-devices (list (swap-space
                        (target (uuid
                                 "e8e45812-542b-4ff3-baac-1fb7b5c34aa3")))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
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
                         (type "ext4")) %base-file-systems)))
