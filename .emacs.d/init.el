;; Keep track of loading time
(defconst emacs-start-time (current-time))
;; initalize all ELPA packages
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(setq package-enable-at-startup nil)
(let ((elapsed (float-time (time-subtract (current-time)
					  emacs-start-time))))
  (message "Loaded packages in %.3fs" elapsed))

;; Load use-package, used for loading packages
(require 'use-package)

(require 'org-evil)
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
 '(package-selected-packages
   (quote
    (ctags-update makefile-executor projectile-ripgrep helm-projectile magit evil-magit slime-company slime god-mode python-mode helm-tramp company-anaconda company-racer company-c-headers company-web web-mode restart-emacs helm helm-core company racer cargo autopair which-key general org-evil monitor apropospriate-theme use-package evil auto-package-update))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
