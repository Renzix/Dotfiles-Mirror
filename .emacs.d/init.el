(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives
      '(("elpa"     . "https://elpa.gnu.org/packages/")
	("melpa"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("elpa"     . 5)
	("melpa"        . 10)))
(if (< emacs-major-version 26)
    (package-initialize)
  (package-refresh-contents))

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
;; Always ensure the package is there
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Follow symlinks instead of asking to follow symlinks
(setq vc-follow-symlinks t)

;; Some visual defaults you can change that i think look ugly af
(setq inhibit-startup-screen t
      initial-buffer-choice 'eshell)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-battery-mode)

;; Very useful funciton
(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
			 (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
(defun open-emacs-config ()
  "Opens my emacs config uwu"
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "New name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
	  (message "A buffer named '%s' already exists!" new-name)
	(progn
	  (rename-file filename new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil))))))
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
	  (vc-delete-file filename)
	(progn
	  (delete-file filename)
	  (message "Deleted file %s" filename)
	  (kill-buffer))))))
(defun helm-projectile-find-file-or-project () 
  "Does switch project if not in a project and find-file if in one"
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-find-file)
    (helm-projectile-switch-project)))
(defun helm-projectile-rg-or-project () 
  "Does switch project if not in a project and find-file if in one"
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-rg)
    (helm-projectile-switch-project)))

;; Relative Line numbers r lit
(when (> emacs-major-version 26)
  (global-display-line-numbers-mode)
  (setq-default display-line-numbers-type 'relative
		display-line-numbers-current-absolute t
		display-line-numbers-width 4
		display-line-numbers-widen t))

;; Better backup defaults
(setq backup-directory-alist `(("." . "~/.saves"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 10
      version-control t
      warning-minimum-level :error)
;; Themes. Just gonna default to apropospriate-dark
(use-package apropospriate-theme
  :config (load-theme 'apropospriate-dark t))

;; Auto updates packages (makes startup time alot longer as it checks for updates)
;;(use-package auto-package-update
;;  :config
;;  (setq auto-package-Mpdate-delete-old-versions t
;;    auto-package-update-hide-results t)
;;  (auto-package-update-maybe))

;; Add which-key (makes keybinds show all possible things) VERY nice to learn emacs with
(use-package which-key
  :config (which-key-mode))

;; Adds fuzzy things for many emacs things
(use-package helm
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-buffers-list))
  :config
  (helm-autoresize-mode t)
  (setq helm-autoresize-max-height 30)
  (setq helm-display-header-line nil)
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
  (helm-mode t))

;; If you want evil uncomment this stuff
(use-package evil
  :init
  (setq evil-want-keybinding nil ; for evil-collection
	evil-want-intergration t) ; also for evil-collection
  :config 
  (evil-mode 1))
;; You probably want this as it allows intergration with a shit ton of things with evil
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Must have if you get magit this is one of the only things not covered by evil-collection
(use-package evil-magit
  :after magit)
(use-package evil-org
  :after org)
(use-package treemacs-evil
  :after treemacs evil)
;; Nice for keybinds in evil mode for example i will bind ; to helm-M-x in normal/visual mode
;; Note that use-package also has a :general keyword which lets you do the same thing
(use-package general)
(general-define-key
 :states '(normal visual)
 ";" 'helm-M-x)

;; If you dont like evil i would try out god-mode. Basically a prefix key (defaults to escape) then you are in god mode
;; during which all keybinds assume you do a Control. the g key assumes you do a alt (M). a space assumes you mean to not use control
;; and a capital G means C-M or control and alt. You can also add a keybind to repeat the last bind
;;(use-package god-mode
;;	     :init (progn
;;		     (setq god-exempt-major-modes nil
;;			   god-exempt-predicates nil)
;;		     (defun my-update-cursor ()
;;		       (setq cursor-type (if (or god-local-mode buffer-read-only)
;;					   'box
;;					   'bar)))
;;		     (add-hook 'god-mode-enabled-hook 'my-update-cursor)
;;		     (add-hook 'god-mode-disabled-hook 'my-update-cursor))
;;	     :bind (("<escape>" . god-local-mode)
;;		    :map god-local-mode-map
;;		    ("." . repeat)))



;; Git intergrations add if you want
(use-package magit
  :bind ("C-c g g" . 'magit-status))
(use-package git-timemachine ; Way to go back in a single file with git history and a keybind
  :bind ("C-c g t" . 'git-timemachine-toggle))
(use-package git-gutter  ; + for addition and - for subtraction at side bar
  :config
  (global-git-gutter-mode))
(use-package forge ; UNSTABLE but good thing for github/gitlab specific stuff like issues and prs
  :after magit)

;; My personal autocomplete plugin. Lots of config options
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-require-match 'never
	company-minimum-prefix-length 3
	company-tooltip-align-annotations t
	company-idle-delay 1
	company-dabbrev-downcase 0
	company-tooltip-limit 20
	global-company-mode t)
  :bind (:map company-active-map
	      ("S-TAB" . company-select-previous)
	      ("<backtab>" . company-select-previous)
	      ("<return>" . nil)
	      ("RET" . nil)
	      ("C-SPC" . company-complete-selection)
	      ("TAB" . company-complete-common-or-cycle)))

;; Projectile!!! Run/do things with projects in projectile (assumes we have helm but you can install the normal one without it)
(use-package helm-projectile
  :init
  (setq projectile-enable-caching t
	projectile-file-exists-local-cache-expire (* 5 60)
	projectile-file-exists-remote-cache-expire (* 10 60)
	projectile-switch-project-action 'helm-projectile-find-file
	projectile-sort-order 'recently-active)
  :config
  (projectile-mode t))

;; Org mode with my default config you can change it if you want
(use-package org
  :init
  (setq default-major-mode 'org-mode
	org-src-tab-acts-natively t
	org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((org . t)
     (latex . t)
     (emacs-lisp . t)
     (sql . t)
     (shell . t)
     (python . t))))
;; Other nice org addon packages
(use-package ox-pandoc
  :after org) ;; for pandoc exports in org mode
(use-package htmlize
  :after org) ;; for html exports in org mode

;; Treemacs which is just a nerdtree like plugin (although there is a nerd tree port). I think this one is better
;; because it has more plugins
;;(use-package treemacs)
;;(use-package treemacs-projectile
;;	     :after treemacs projectile)
;;(use-package treemacs-magit
;;	     :after treemacs magit)


;; Actually useful terminal emulator inside emacs based on the same library as :term that doesnt work
;; properly from melpa yet!!! Dont uncomment this you have to build from their github as of rn
;;(use-package vterm)

;; Easy way to install directly from github using quelpa. Note that this is only used in the config
;; So if you never use it you can just get rid of it or comment it out. Keep in mind this auto updates with the setq
;;(use-package quelpa
;; 	     :init (setq quelpa-upgrade-p t
;;			 quelpa-stable-p t))
;;(use-package quelpa-use-package) 
;; Use like use-package but add the :quelpa flag

;; Misc programming stuff
(use-package autopair
  :config (autopair-global-mode t))

;; Languages!!!
;; Scala
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;; Rust
(use-package rustic)

;; Python
(use-package lsp-python-ms)

;; Haskell
(use-package haskell-mode)
(use-package lsp-haskell)

;; Flycheck
(use-package flycheck
  :init (global-flycheck-mode))
(use-package flycheck-pos-tip
  :after flycheck
  :config (flycheck-pos-tip-mode))
(use-package flycheck-perl6
  :after flycheck)
(use-package flycheck-haskell
  :after flycheck)
;; LSP
(use-package lsp-mode
  :hook
  ((scala-mode . lsp)
   (python-mode . lsp)
   (c-mode . lsp))
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode-hook . lsp-ui-mode))

;; Add company-lsp backend for metals
(use-package company-lsp)

;; KEYBINDS
(general-define-key
 :states '(normal visual)
 :keymaps 'override
 (kbd "SPC") 'helm-buffer-list
 (kbd "S") 'helm-projectile-find-file-or-project
 (kbd "s") 'helm-find-files
 (kbd ";") 'helm-M-x
 (kbd "g c c") 'comment-line
 (kbd "g c r") 'comment-or-uncomment-region
 (kbd "g =") 'indent-buffer
 (kbd "g p") 'projectile-command-map
 (kbd "\\") 'helm-projectile-rg-or-project
 (kbd "|") 'helm-locate
 (kbd "g _") 'magit-status
 (kbd "_") 'magit-dispatch
 (kbd "Q") 'save-buffers-kill-terminal
 (kbd "U") 'undo-tree-visualize)
(evil-ex-define-cmd "cfg" 'open-emacs-config)
;; Anything past here is autogenerated every time you run emacs. it is safe to delete this only if you also delete everything else
;; the actual config instead

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" default))
 '(package-selected-packages
   '(perl6-mode helm which-key quelpa-use-package quelpa use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
