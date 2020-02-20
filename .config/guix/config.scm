;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu))
(use-modules (gnu packages emacs))
(use-modules (gnu packages emacs-xyz))
(use-modules (gnu packages admin))
(use-modules (gnu packages shells))
(use-service-modules desktop networking ssh xorg)
(use-modules (nongnu packages linux))

(operating-system
 (kernel linux)
 (firmware (cons* iwlwifi-firmware
                  %base-firmware))
 (locale "en_US.utf8")
 (timezone "America/New_York")
 (keyboard-layout
  (keyboard-layout "us" "altgr-intl"
                   #:options '("caps:swapescape")))
 (bootloader
  (bootloader-configuration
   (bootloader grub-bootloader)
   (target "/dev/sda")
   (keyboard-layout keyboard-layout)))
 (swap-devices (list "/dev/sda1"))
 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device
           (uuid "1bb6b9ef-0dcf-4daa-994c-35b6d5224085"
                 'ext4))
          (type "ext4"))
         %base-file-systems))
 (host-name "RSD")
 (users (cons* (user-account
                (name "Renzix")
                (comment "Renzix")
                (group "users")
                (shell #~(string-append #$zsh "/bin/zsh"))
                (home-directory "/home/Renzix")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))
 (packages
  (append
   (list (specification->package "nss-certs")
         emacs
         emacs-exwm
         zsh)
   %base-packages))
 (services
  (append
   (list (service openssh-service-type)
         (set-xorg-configuration
          (xorg-configuration
           (keyboard-layout keyboard-layout))))
   %desktop-services)))
