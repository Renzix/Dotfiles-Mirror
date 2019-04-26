;; Keep track of loading time
(defconst emacs-start-time (current-time))
;; initalize all ELPA packages
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("GNU" . "https://elpa.gnu.org/packages/"))
(when (not package-archive-contents)
  (package-refresh-contents))
(when (version< emacs-version "27.0")
  (unless package--initialized (package-initialize t)))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
;; Load use-package, used for loading packages
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq vc-follow-symlinks t)
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package org-evil
  :after evil
  :init
  (setq evil-want-keybinding nil))
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
   '(helm-circe circe frame-purpose rainbow-identifiers esxml tracking ov a anaphora matrix-client elcord emms webpaste htmlize ox-pandoc helm-flyspell treemacs-magit treemacs-evil treemacs-projectile treemacs minimap autopair flycheck-haskell lsp-haskell haskell-mode ensime company-irony-c-headers company-irony irony-eldoc flycheck-irony irony company-jedi racer clippy lsp-mode cargo rustic rust-mode git-gutter git-timemachine evil-magit helm-projectile company evil-collection evil-god-state helm which-key general quelpa-use-package quelpa dracula-theme org-evil auto-package-update use-package))
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "smtp.gmail.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
