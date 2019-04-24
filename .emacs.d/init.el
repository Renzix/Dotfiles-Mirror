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
