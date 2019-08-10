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

;; Bootstrap use-package
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
      initial-buffer-choice nil
      confirm-kill-processes nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-battery-mode)

;; Very useful functions
(defun magit-status-only ()
  "Opens magit-status in a single buffer."
  (magit-status)
  (delete-other-windows))

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

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
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
	(error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
	(if (get-buffer new-name)
	    (error "A buffer named '%s' already exists!" new-name)
	  (rename-file filename new-name 1)
	  (rename-buffer new-name)
	  (set-visited-file-name new-name)
	  (set-buffer-modified-p nil)
	  (message "File '%s' successfully renamed to '%s'"
		   name (file-name-nondirectory new-name)))))))
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
;; Window stuff for eshell and vterm
(defvar my:window-conf nil)
(defun eshell-toggle (buf-name)
  "Switches to eshell and saves persp"
  (interactive (list (buffer-name)))
  (if (string-equal buf-name "*eshell*")
      (set-window-configuration my:window-conf)
    (progn
      (setq my:window-conf (current-window-configuration))
      (delete-other-windows)
      (eshell))))
(defun switch-to-vterm ()
  "Switches to vterm"
  (if (get-buffer "vterm")
      (switch-to-buffer "vterm")
    (vterm)))
(defun vterm-toggle (buf-name)
  "Switches to vterm and saves persp"
  (interactive (list (buffer-name)))
  (if (string-equal buf-name "vterm")
      (set-window-configuration my:window-conf)
    (progn
      (setq my:window-conf (current-window-configuration))
      (delete-other-windows)
      (switch-to-vterm))))
(defun previous-newline-without-break-of-line ()
  "1. move to previous line
  2. move to end of the line.
  3. insert newline with index"
  (interactive)
  (let ((oldpos (point)))
    (previous-line)
    (end-of-line)
    (newline-and-indent)))
(defun newline-without-break-of-line ()
  "1. move to end of the line.
  2. insert newline with index"
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

;; Relative Line numbers r lit
(when (> emacs-major-version 26)
  (global-display-line-numbers-mode)
  (setq-default display-line-numbers-type 'relative
		display-line-numbers-current-absolute t
		display-line-numbers-width 3
		display-line-numbers-widen t))

;; Better backup defaults
(setq backup-directory-alist `(("." . "~/.saves"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 10
      version-control t
      split-width-threshold 1
      warning-minimum-level :error)
;; Themes. Changes depending on the day of the week
(when (display-graphic-p)
  (defvar renzix-weekday (format-time-string "%w"))
  (use-package doom-themes)
  (use-package apropospriate-theme)
  (use-package monokai-theme)
  (cond ((string= "0" renzix-weekday) ;; Sunday
	 (load-theme 'doom-dracula t))
	((string= "1" renzix-weekday) ;; Monday
	 (load-theme 'doom-opera t))
	((string= "2" renzix-weekday) ;; Tuesday
	 (load-theme 'apropospriate-dark t))
	((string= "3" renzix-weekday) ;; Wednsday
	 (load-theme 'doom-molokai t))
	((string= "4" renzix-weekday) ;; Thursday
	 (load-theme 'doom-nord t))
	((string= "5" renzix-weekday) ;; Friday
	 (load-theme 'monokai t))
	((string= "6" renzix-weekday) ;; Saterday
	 (load-theme 'doom-one t))))

;; Add which-key (makes keybinds show all possible things) VERY nice to learn emacs with
(use-package which-key
  :config (which-key-mode))

;; Adds fuzzy things for many emacs things
(use-package helm
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-mini))
  :config
  (helm-autoresize-mode t)
  (setq helm-autoresize-max-height 30)
  (setq helm-display-header-line nil)
  (helm-mode t))
(use-package helm-rg
  :after helm)
(use-package imenu-anywhere)

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
(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))
(use-package evil-matchit
  :after evil
  :config (global-evil-matchit-mode 1))

;; Must have if you get magit this is one of the only things not covered by evil-collection
(use-package evil-magit
  :after magit)
;; :config ; Magit defaults to opening status in the current window better default
;;   (setq magit-display-buffer-function
;; 	(lambda (buffer)
;; 	  (display-buffer
;; 	   buffer (if (and (derived-mode-p 'magit-mode)
;; 			   (memq (with-current-buffer buffer major-mode)
;; 				 '(magit-process-mode
;; 				   magit-revision-mode
;; 				   magit-diff-mode
;; 				   magit-stash-mode
;; 				   magit-status-mode)))
;; 		      nil
;; 		    '(display-buffer-same-window))))))
(use-package evil-org
  :after '(org evil))
(use-package evil-space
  ;; :init (setq evil-space-auto-setup nil)
  :config (evil-space-mode))

(use-package key-chord
  :after evil
  :config
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-mode 1))

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
  (setq-default initial-major-mode 'org-mode
		initial-scratch-message ""
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
;; LaTeX
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t
	TeX-parse-self t
	TeX-view-program-selection '(((output-dvi has-no-display-manager)
				      "dvi2tty")
				     ((output-dvi style-pstricks)
				      "dvips and gv")
				     (output-dvi "xdvi")
				     (output-pdf "mupdf")
				     (output-html "xdg-open")))
  (add-to-list 'TeX-view-program-list '("mupdf" "mupdf %o")))
(use-package company-auctex
  :after tex
  :config (company-auctex-init))
;; Other useful text stuff
(use-package langtool
  :init (setq langtool-language-tool-jar "~/Projects/NotMine/LanguageTool-4.6/languagetool-commandline.jar"
	      langtool-default-language "en-US"))

;; Actually useful terminal emulator inside emacs based on the same library as :term that doesnt work
;; properly from melpa yet!!! For now i install it manually
;;(use-package vterm)
(when (file-directory-p "~/Projects/NotMine/emacs-libvterm")
  (add-to-list 'load-path "~/Projects/NotMine/emacs-libvterm")
  (require 'vterm)
  (general-define-key
   :states '(normal)
   :keymaps 'vterm-mode-map
   "o" #'evil-insert-resume
   "a" #'evil-insert-resume
   "i" #'evil-insert-resume
   "<return>" #'evil-insert-resume))
(use-package powershell)

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
(use-package beacon
  :config (beacon-mode 1))
(use-package mentor)
(use-package treemacs)
(use-package treemacs-evil
  :after '(treemacs evil))
(use-package treemacs-projectile
  :after '(treemacs projectile))
(use-package treemacs-magit
  :after '(treemacs magit))
;; Icons for treemacs
(use-package all-the-icons)

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

(use-package company-lsp
  :after '(company lsp-mode))

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

;; C
(use-package irony
  :hook (c++-mode-hook . irony-mode)
  :hook (objc-mode-hook . irony-mode)
  :hook (c-mode-hook . irony-mode))
(use-package company-irony
  :after '(company irony))
(use-package flycheck-irony
  :after '(flycheck irony))
(use-package irony-eldoc
  :after '(irony))

;; C#
(use-package csharp-mode)
(use-package omnisharp
  :hook (csharp-mode-hook . omnisharp-mode)
  :config
  (add-to-list 'company-backends 'company-omnisharp))

;; Rust
(use-package rustic)

;; Python
(use-package lsp-python-ms)

;; Haskell
(use-package haskell-mode)
(use-package lsp-haskell
  :after lsp-mode)
(use-package flycheck-haskell
  :after flycheck)

;; Perl6
(use-package perl6-mode)
(use-package flycheck-perl6
  :after flycheck)

;; Dart
(use-package dart-mode)

;; Flycheck misc
(use-package flycheck
  :init (global-flycheck-mode))
(use-package flycheck-pos-tip
  :after flycheck
  :config (flycheck-pos-tip-mode))

;; Other fun stuff
(use-package elcord
  :config (elcord-mode))
;; MY PROJECT
(when (file-directory-p "~/Projects/Mine/rencord")
  (add-to-list 'load-path "~/Projects/Mine/rencord")
  (require 'rencord))

;; KEYBINDS general is nice 4 evil but useful without it also
(use-package general)
(general-define-key
 :states '(normal visual)
 "|" 'helm-mini
 "_" 'evil-jump-backward
 "S" 'helm-projectile-find-file-or-project
 "s" 'helm-find-files
 ";" 'helm-M-x
 "g c c" 'comment-line
 "g c r" 'comment-or-uncomment-region
 "g =" 'indent-buffer
 "g p" 'projectile-command-map
 "\\" 'helm-projectile-rg-or-project
 "," 'magit-status
 "Q" 'save-buffers-kill-terminal
 "U" 'undo-tree-visualize
 "[ SPC" 'previous-newline-without-break-of-line
 "] SPC" 'newline-without-break-of-line)
(general-define-key
 :states '(normal visual insert)
 "C-s" 'eshell-toggle
 "C-t" 'vterm-toggle)
(evil-ex-define-cmd "cfg" 'open-emacs-config)
(evil-ex-define-cmd "l" 'TeX-command-master)
(evil-ex-define-cmd "q[uit]" 'delete-window)
;; Anything past here is autogenerated every time you run emacs. it is safe to delete this only if you also delete everything else
;; the actual config instead

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("1d2f406a342499f0098f9388b87d05ec9b28ccb12ca548f4f5fa80ae368235b6" "a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" "8c847a5675ece40017de93045a28ebd9ede7b843469c5dec78988717f943952a" "82358261c32ebedfee2ca0f87299f74008a2e5ba5c502bde7aaa15db20ee3731" "e3c87e869f94af65d358aa279945a3daf46f8185f1a5756ca1c90759024593dd" "34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" default))
 '(package-selected-packages
   '(websocket helm-rg helm which-key quelpa-use-package quelpa use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
