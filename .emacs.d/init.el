;; Keep track of loading time
(defconst emacs-start-time (current-time))
;; initalize all ELPA packages
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
;;(setq package-enable-at-startup nil)
;;(let ((elapsed (float-time (time-subtract (current-time)
;;					  emacs-start-time))))
;;  (message "Loaded packages in %.3fs" elapsed))

(require 'use-package)
;; Load use-package, used for loading packages
(setq use-package-always-ensure t)
(setq vc-follow-symlinks t)

(use-package org-evil)
(org-babel-load-file
  (expand-file-name "settings.org"
		    user-emacs-directory))

;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time)
					  emacs-start-time))))
  (message "Loading settings...done (%.3fs)" elapsed))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#424242" "#EF9A9A" "#C5E1A5" "#FFEE58" "#64B5F6" "#E1BEE7" "#80DEEA" "#E0E0E0"])
 '(beacon-color "#7fff00007fff")
 '(custom-safe-themes
   (quote
    ("5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" default)))
 '(delete-selection-mode nil)
 '(evil-emacs-state-cursor (quote ("#E57373" hbar)) t)
 '(evil-insert-state-cursor (quote ("#E57373" bar)) t)
 '(evil-normal-state-cursor (quote ("#FFEE58" box)) t)
 '(evil-visual-state-cursor (quote ("#C5E1A5" box)) t)
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   (quote
    ("#FFEE58" "#C5E1A5" "#80DEEA" "#64B5F6" "#E1BEE7" "#FFCC80")))
 '(highlight-symbol-foreground-color "#E0E0E0")
 '(highlight-tail-colors (quote (("#7fff00007fff" . 0) ("#424242" . 100))))
 '(package-selected-packages
   (quote
    (ox-pandoc csharp-mode kdeconnect helm-rg json-mode elcord exwm-randr powershell autopair company-irony company helm-projectile which-key linum-relative use-package org-evil helm god-mode general apropospriate-theme)))
 '(pos-tip-background-color "#000000000000")
 '(pos-tip-foreground-color "#9E9E9E")
 '(tabbar-background-color "#000000000000"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
