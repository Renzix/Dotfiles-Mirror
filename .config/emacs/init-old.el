;; Welcome to renzix's second emacs config
;; This one is going to be as vanilla emacs as possible

;;; Garbage Collection for speed
(setq gc-cons-threshold 10000000)
;; Restore after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 1000000)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))
(setq read-process-output-max (* 1024 1024))

;;; Package Stuff
(setq package-enable-at-startup nil
      package-archives
      '(("elpa"     . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("elpa"     . 5) ("melpa"        . 10)))
(package-refresh-contents)

(add-to-list 'load-path "~/Dotfiles/.config/emacs/elisp")
(load-file "~/.config/passwords.el")

;; Macros

(defmacro get! (package)
  `(unless (package-installed-p ,package)
     (package-install ,package)))

(defmacro before! (package stuff)
  stuff)

(defalias 'after! 'with-eval-after-load)

(defalias 'force! 'require)

;; @TODO(Renzix): add key! so i can lazy load?

(get! '0x0)
(get! 'amx)
(get! 'anzu)
(get! 'auctex)
(get! 'avy)
(get! 'browse-kill-ring)
(get! 'circe)
(get! 'command-log-mode)
(get! 'comment-dwim-2)
(get! 'company)
(get! 'company-lsp)
(get! 'counsel)
(get! 'counsel-projectile)
(get! 'crux)
(get! 'dap-mode)
(get! 'doom-modeline)
(get! 'doom-themes)
(get! 'easy-kill)
(get! 'emms)
(get! 'erc-hl-nicks)
(get! 'erc-image)
(get! 'expand-region)
(get! 'flx)
(get! 'flycheck)
(get! 'flycheck-perl6)
(get! 'git-gutter)
(get! 'git-timemachine)
(get! 'hl-todo)
(get! 'ivy)
(get! 'lsp-java)
(get! 'lsp-mode)
(get! 'lsp-python-ms)
(get! 'lsp-ui)
(get! 'lua-mode)
(get! 'magit)
(get! 'magit-todos)
(get! 'nix-mode)
(get! 'perl6-mode)
(get! 'powershell)
(get! 'projectile)
(get! 'projectile-ripgrep)
(get! 'rainbow-delimiters)
(get! 'rustic)
(get! 'try)
(get! 'visible-mark)
(get! 'vterm)
(get! 'which-key)
(get! 'yasnippet)
(get! 'yasnippet-snippets)

;; These get force loaded
;; The fewer the better

(force! 'amx)
(force! 'anzu)
(force! 'avy)
(force! 'better-registers)
(force! 'browse-kill-ring)
(force! 'circe)
(force! 'comment-dwim-2)
(force! 'company)
(force! 'company-lsp)
(force! 'counsel)
(force! 'counsel-projectile)
(force! 'crux)
(force! 'dap-mode)
(force! 'doom-modeline)
(force! 'doom-themes)
(force! 'easy-kill)
(force! 'erc-hl-nicks)
(force! 'erc-image)
(force! 'expand-region)
(force! 'flx)
(force! 'flycheck)
(force! 'git-gutter)
(force! 'git-timemachine)
(force! 'hl-todo)
(force! 'ivy)
(force! 'lsp-mode)
(force! 'lsp-ui)
(force! 'magit)
(force! 'magit-todos)
(force! 'projectile)
(force! 'projectile-ripgrep)
(force! 'rainbow-delimiters)
(force! 'visible-mark)
(force! 'vterm)
(force! 'which-key)
(force! 'yasnippet)
(force! 'yasnippet-snippets)

;;; Config

;; Sane defaults
(setq inhibit-startup-screen t
      initial-buffer-choice nil
      confirm-kill-processes nil
      browse-url-browser-function 'eww-browse-url)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Magic that makes only 2 windows spawn max
(setq split-width-threshold (- (window-width) 10))
(setq split-height-threshold nil)
(defun count-visible-buffers (&optional frame)
  "Count how many buffers are currently being shown.  Defaults to selected FRAME."
  (length (mapcar #'window-buffer (window-list frame))))
(defun do-not-split-more-than-two-windows (window &optional horizontal)
  "WINDOW HORIZONTAL."
  (if (and horizontal (> (count-visible-buffers) 1))
      nil
    t))
(advice-add 'window-splittable-p
            :before-while #'do-not-split-more-than-two-windows)

(global-hl-line-mode)
(setq vc-follow-symlinks t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; Parens
(show-paren-mode 1)
(electric-pair-mode t)
(setq electric-pair-preserve-balance nil)

(setq backup-directory-alist `(("." . "~/.saves"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 10
      version-control t
      auto-save-list-file-prefix nil)

(prefer-coding-system 'utf-8)

;; Dired

(setq dired-isearch-filenames 'dwim
      delete-by-moving-to-trash t)

;; whitespace
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

(after! 'visible-mark
  ;;(transient-mark-mode -1)
  (global-visible-mark-mode 1)
  (setq visible-mark-max 1
        visible-mark-faces `(visible-mark-face2)))

(after! 'ivy
  (ivy-mode 1)
  (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-dispatching-done)
  (define-key ivy-minibuffer-map (kbd "S-TAB") 'ivy-dispatching-call)
  (require 'flx)
  (setq ivy-re-builders-alist '((t . ivy--regex-plus))
        ivy-use-virtual-buffers t
        ivy-initial-inputs-alist nil
        ivy-use-selectable-prompt t
        projectile-completion-system 'ivy))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (pos) 'read-face-name)
                  (get-char-property (pos) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(after! 'org
  (setq-default initial-major-mode 'org-mode
                initial-scratch-message ""
                org-src-tab-acts-natively t
                org-confirm-babel-evaluate nil
                org-return-follows-link t)
  (setq org-log-done 'time
        org-src-window-setup 'current-window
        org-archive-location "archived.org:: From %s"
        org-todo-keywords '((sequence "TODO(t)" "HOLD(h@)" "NEXT(n!)"
                                      "|" "DONE(d!)" "CANCEL(c@)"))
        org-todo-keyword-faces
        '(("TODO" . (:foreground "#f0dfdf" :weight bold))
          ("HOLD" . (:foreground "#d0bf8f" :weight bold))
          ("NEXT" . (:foreground "#873e23" :weight bold))
          ("CANCEL" . (:foreground "#7cb8bb" :weight bold))
          ("DONE" . (:foreground "#583659" :weight bold)))
        org-capture-templates
        '(("e" "Emacs" entry
           (file+headline "~/Documents/agenda/home.org" "Emacs")
           "* TODO %?\n %i\n %U")
          ("f" "Fixme" entry
           (file+headline "~/Documents/agenda/work.org" "Fixme")
           "* TODO %?\nEntered on %U\n %i\n %a")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((org . t)
     (C . t)
     (latex . t)
     (emacs-lisp . t)
     (sql . t)
     (shell . t)
     (python . t))))

(after! 'doom-modeline
  (doom-modeline-mode 1))

(after! 'flycheck
  (global-flycheck-mode))

(before! 'company
         (progn
           (add-hook 'prog-mode-hook 'company-mode)))

(after! 'company
  (progn
    (setq company-minimum-prefix-length 1
          company-idle-delay 0.1) ;; default is 0.2
    (require 'company-lsp)
    (add-to-list 'company-backends 'company-lsp)
    (define-key company-active-map (kbd "<return>") nil)
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map (kbd "C-SPC") 'company-complete-selection)))

(after! 'circe
  (require 'circe-chanop)
  (setq circe-network-options
        '(("Freenode"
           :tls t
           :nick "Renzix"
           :sasl-username "Renzix"
           :sasl-password ,my/irc-password
           :channels ("#nixhub"))
          ("Bitlbee"
           :nick "Renzix"
           :user "Renzix"
           :nickserv-password ,my/irc-password)))
  (setq circe-reduce-lurker-spam t
        circe-format-say "{nick:-10s}| {body}"
        circe-format-self-say "{nick:-10s}| {body}"
        lui-time-stamp-position 'right-margin
        lui-time-stamp-format "%H:%M"
        lui-fill-type nil)
  (require 'lui-autopaste)
  (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)
  (add-hook 'lui-mode-hook 'my-circe-set-margin)
  (defun my-circe-set-margin ()
    (setq right-margin-width 5))
  (add-hook 'lui-mode-hook 'my-lui-setup)
  (defun my-lui-setup ()
    (setq
     fringes-outside-margins t
     right-margin-width 5
     word-wrap t
     wrap-prefix "    "))
  (add-hook 'circe-chat-mode-hook 'my-circe-prompt)
  (defun my-circe-prompt ()
    (lui-set-prompt
     (concat (propertize (concat (buffer-name) ">")
                         'face 'circe-prompt-face)
             " ")))
  (enable-circe-color-nicks)
  (enable-circe-display-images))

(after! 'erc
  (setq renzix-erc-user-colors '())
  (defun my-hl-thing ()
    (put-text-property
     (beginning-of-thing 'word) (end-of-thing 'word)
     'font-lock-face `(:foreground ,(erc-hl-nicks-color-for-nick (word-at-point)))))
  ;; @TODO(Renzix): Make this actually work properly. Need to 1. find nick
  ;; 2. find begining and end of said nick location wise and maybe 3. cache it
  (defun erc-nixhubd-fix ()
    (when (string-prefix-p "<nixhubd>" (buffer-string))
      (goto-char (point-min))
      (delete-char 10)
      (insert "[")
      (skip-chars-forward "^:")
      (delete-char 1)
      (insert "]")
      (skip-chars-backward "^[")
      (forward-char)
      (my-hl-thing)))

  (setq erc-autojoin-channels-alist
        '(("freenode.net" "#nixhub" "#kisslinux" "#gentoo-chat")
          ("127.0.0.1"))
        erc-kill-buffer-on-part t
        erc-kill-server-buffer-on-quit t
        erc-nick "Renzix"
        erc-hide-list '("JOIN" "PART" "QUIT")
        erc-lurker-hide-list '("JOIN" "PART" "QUIT")
        erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                                  "324" "329" "332" "333" "353" "477"))
  (add-hook 'erc-insert-modify-hook 'erc-nixhubd-fix))

(after! 'erc-image
  (add-to-list 'erc-modules 'image)
  (erc-update-modules)
  (setq erc-image-display-func 'erc-image-insert-inline
        erc-image-inline-rescale 200))

(before! 'lsp
         (progn 
           (add-hook 'prog-mode-hook 'lsp)
           (add-hook 'lsp-mode-hook 'company-mode)
           (add-hook 'lsp-mode-hook 'lsp-ui-mode)
           (add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)))

(after! 'lsp
  (progn
    (setq lsp-ui-sideline-enable t
          lsp-ui-doc-enable t
          lsp-ui-flycheck-enable t
          lsp-ui-imenu-enable t
          lsp-ui-sideline-ignore-duplicate t)))

;; Random Packages
(after! 'amx
  (amx-mode t))

(after! 'doom-themes
  (load-theme 'doom-one t))

(after! 'git-gutter
  (global-git-gutter-mode t))

(after! 'hl-todo
  (global-hl-todo-mode)
  (setq hl-todo-keyword-faces
        '(("TODO" . (:foreground "#f0dfdf" :weight bold))
          ("HOLD" . (:foreground "#d0bf8f" :weight bold))
          ("NEXT" . (:foreground "#873e23" :weight bold))
          ("CANCEL" . (:foreground "#7cb8bb" :weight bold))
          ("DONE" . (:foreground "#583659" :weight bold))
          ("NOTE" . (:foreground "#41d14a" :weight bold)))))

(after! 'projectile
  (setq projectile-enable-caching t
        projectile-file-exists-local-cache-expire (* 5 60)
        projectile-file-exists-remote-cache-expire (* 10 60)
        projectile-sort-order 'recently-active)
  (projectile-mode t))

(after! 'rainbow-delimiters
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(after! 'which-key
  (which-key-mode t))

(after! 'yasnippet
  (yas-global-mode t))

;; @TODO(Renzix): Clean these up
(setq-default cursor-type 'box)
(set-cursor-color "#43DE43")
(delete-selection-mode 1)
(pcre-mode t)

;; Useful Functions

(defun dired-dotfiles-toggle ()
  "Show/hide dot-files."
  (interactive)
  (when (equal major-mode 'dired-mode)
    ;; if currently showing
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) 
	(progn 
	  (set (make-local-variable 'dired-dotfiles-show-p) nil)
	  (message "h")
	  (dired-mark-files-regexp "^\\\.")
	  (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
	     (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(defun my/switch-to-vterm ()
  "Switch to vterm."
  (if (get-buffer "vterm")
      (switch-to-buffer "vterm")
    (vterm)))

(defun my/vterm-toggle (buf-name)
  "Switch to vterm and save persp.  BUF-NAME is the current buffer name."
  (interactive (list (buffer-name)))
  (if (string-equal buf-name "vterm")
      (set-window-configuration my:window-conf)
    (progn
      (setq my:window-conf (current-window-configuration))
      (delete-other-windows)
      (my/switch-to-vterm))))

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun my/crux-duplicate-current-line-or-region-up ()
  (interactive)
  (crux-duplicate-current-line-or-region 1)
  (previous-line)
  (scroll-up-line))

(defun my/avy-goto-url ()
  (interactive)
  (avy--generic-jump "http" nil nil))

;;; Keybinds

;; Global
(after! 'amx
  (global-set-key (kbd "M-x") 'amx))

(after! 'anzu
  (global-set-key (kbd "M-%") 'anzu-query-replace-regexp))

(after! 'avy
  (global-set-key (kbd "C-c l") 'my/avy-goto-url))


(global-set-key (kbd "M-m") 'kmacro-keymap)
(global-set-key (kbd "M-;") 'comment-dwim-2)
(global-set-key (kbd "C-,") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-.") 'kmacro-end-or-call-macro)
;; (global-set-key (kbd "C-s") 'swiper-isearch-forward) ;; Swiper seems too distracting
;; (global-set-key (kbd "C-r") 'swiper-isearch-backward)
(global-set-key (kbd "M-<left>") (lambda () (interactive) (transpose-words -1)))
(global-set-key (kbd "M-<right>") (lambda () (interactive) (transpose-words 1)))
(global-set-key (kbd "M-<down>") 'move-text-down)
(global-set-key (kbd "M-<up>") 'move-text-up)

;; M-w is now 10x better
(after! 'easy-kill
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))
;; M-z is forwards so C-z is backwards
(global-set-key (kbd "C-z")
                (lambda (arg char)
                  (interactive "p\ncZap to char: ")
                  (zap-to-char -1 char)))

(after! 'expand-region
  (global-set-key (kbd "C-\\") 'er/expand-region))

;; Ibuffer is just better list-buffers and C-x f
;; to be consistant with ibuffer/switch-buffers
(global-set-key (kbd "C-x f") 'dired-jump)
(global-set-key (kbd "C-x b") 'ibuffer)

(after! 'counsel
  ;; These two are replacable with counsel-git and counsel-rg
  (global-set-key (kbd "C-:") 'counsel-projectile-rg)
  (global-set-key (kbd "C-;") 'counsel-projectile-find-file)
  ;; TODO temp fix for org mode
  (global-set-key (kbd "C-'") 'counsel-find-file)
  (define-key org-mode-map (kbd "C-'") 'counsel-find-file)
  (global-set-key (kbd "C-\"") 'ivy-switch-buffer)
  (define-key org-mode-map (kbd "C-\"") 'ivy-switch-buffer))

(after! 'circe
  (define-key circe-mode-map (kbd "C-c u") 'browse-url-emacs))

;; Nice functions that should be default but dont exist for ??? reason
(after! 'browse-kill-ring
  (global-set-key (kbd "C-c y") 'browse-kill-ring))

(after! 'vterm
  (global-set-key (kbd "C-c v") 'my/vterm-toggle))

;; Crux has alot of nicer defaults
(after! 'crux
  (global-set-key (kbd "M-S-<down>") 'crux-duplicate-current-line-or-region)
  (global-set-key (kbd "M-S-<up>") 'my/crux-duplicate-current-line-or-region-up)
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
  (global-set-key (kbd "C-k") 'crux-smart-kill-line)
  (global-set-key (kbd "C-o") 'crux-smart-open-line)
  (global-set-key (kbd "M-o") 'crux-smart-open-line-above)
  (global-set-key (kbd "C-^") 'crux-top-join-line)
  (global-set-key (kbd "C-S-<backspace>") 'crux-kill-whole-line)
  (global-set-key (kbd "C-c D") 'crux-delete-file-and-buffer)
  (global-set-key (kbd "C-c I") 'crux-find-user-init-file)
  (global-set-key (kbd "C-c R") 'crux-rename-file-and-buffer)
  (global-set-key (kbd "C-c k") 'crux-kill-other-buffers)
  (global-set-key (kbd "C-c o") 'crux-open-with)
  (global-set-key (kbd "C-c t") 'crux-transpose-windows))
(global-set-key (kbd "C-x 4 t") 'crux-transpose-windows)

;; Projectile
(after! 'projectile
  (global-set-key (kbd "C-c c") 'projectile-compile-project)
  (global-set-key (kbd "C-c p") 'projectile-command-map))

;; Magit
(after! 'magit
  (global-set-key (kbd "C-c g g") 'magit-status)
  (global-set-key (kbd "C-c g t") 'git-timemachine-toggle)
  (global-set-key (kbd "C-c g s") 'magit-stage-file)
  (global-set-key (kbd "C-c g u") 'magit-unstage-file)
  (global-set-key (kbd "C-c g c") 'magit-commit)
  (global-set-key (kbd "C-c g p") 'magit-push-current-to-upstream)
  (global-set-key (kbd "C-c g P") 'magit-push-current-to-pushremote))

(after! 'lsp
  (define-key lsp-mode-map (kbd "s-i") 'lsp-goto-implementation)
  (define-key lsp-mode-map (kbd "s-t") 'lsp-goto-type-definition)
  (define-key lsp-mode-map (kbd "s-r") 'lsp-rename))

(after! 'org
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c z") 'org-capture)
  (global-set-key (kbd "C-c s") 'org-store-link))

(after! 'dired
  (define-key dired-mode-map (kbd ".") 'dired-dotfiles-toggle))

(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/Documents/agenda/home.org" "~/Documents/agenda/archived.org" "~/Documents/agenda/work.org" "~/Documents/agenda/emacs.org"))
 '(package-selected-packages
   '(nix-mode circe doom-modeline electric-parens vterm erc-image erc-hl-nicks try anzu auctex command-log-mode comment-dwim-2 emms yasnippet-snippets which-key rustic rainbow-delimiters projectile-ripgrep powershell perl6-mode md4rd magit-todos lua-mode lsp-ui lsp-python-ms lsp-java ido-completing-read+ git-timemachine git-gutter flycheck-perl6 expand-region easy-kill doom-themes deadgrep dap-mode crux company-lsp browse-kill-ring amx 0x0))
 '(safe-local-variable-values
   '((projectile-project-run-cmd . "./opengl")
     (projectile-project-compilation-cmd . "clang++ -o opengl -lglut -lGLU -lGL -lGLEW main.cpp"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
