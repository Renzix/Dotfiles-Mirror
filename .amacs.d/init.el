;; My attempt at making acme in emacs. Its not very good tbh

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
(package-initialize)

;; Macros

(defmacro use! (package)
  `(unless (package-installed-p ,package)
     (package-install ,package)))

(defmacro before! (package stuff)
  stuff)

(defalias 'after! 'with-eval-after-load)

;;(use! 'amx)
;;(use! 'avy)
;;(use! 'browse-kill-ring)
(use! 'company)
(use! 'company-lsp)
(use! 'crux)
(use! 'dap-mode)
;;(use! 'deadgrep)
(use! 'doom-themes)
;;(use! 'easy-kill)
;;(use! 'expand-region) @TODO(Renzix): Double click to expand region?
(use! 'flycheck)
(use! 'git-gutter)
;;(use! 'git-timemachine) @TODO(Renzix): Maybe???
(use! 'hl-todo)
;;(use! 'ido-completing-read+)
(use! 'lsp-java)
(use! 'lsp-mode)
(use! 'lsp-python-ms)
(use! 'lsp-ui)
(use! 'lua-mode)
;;(use! 'magit)
;;(use! 'magit-todos)
(use! 'pcre2el)
;;(use! 'projectile)
;;(use! 'projectile-ripgrep)
(use! 'rainbow-delimiters)
(use! 'rustic)
(use! 'vterm)
(use! 'wand)
;;(use! 'which-key)
(use! 'yasnippet)
(use! 'yasnippet-snippets)
(use! 'perl6-mode)
(use! 'flycheck-perl6)

(add-to-list 'load-path "~/.emacs.d/elisp")
(require 'acme-mouse)
;;; Config

;; Sane defaults
(setq inhibit-startup-screen t
      initial-buffer-choice nil
      confirm-kill-processes nil)

(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode 1)

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

(show-paren-mode 1)
(setq vc-follow-symlinks t)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

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

;; Org mode

(setq-default initial-major-mode 'org-mode
              initial-scratch-message ""
              org-src-tab-acts-natively t
              org-confirm-babel-evaluate nil
              org-return-follows-link t)
(setq org-log-done 'time
      org-src-window-setup 'current-window
      org-todo-keywords '((sequence "TODO(t)" "SOMEDAY(s)" "NEXT(n)" "|")
                          (sequence "WORKING(w!)" "BLOCKED(B@)" "|")
                          (sequence "REPORT(r)" "BUG(b)" "KNOWN(k)" "|" "FIXED(f!)")
                          (sequence "|" "DONE(d)" "CANCEL(c@)")
                          (sequence "|" "STUDY(y!)")))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((org . t)
   (C . t)
   (latex . t)
   (emacs-lisp . t)
   (sql . t)
   (shell . t)
   (python . t)))

(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Package

(global-flycheck-mode)

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
(after! 'wand
  (require 'wand)
  (setq wand:*rules*
        (list
              (wand:create-rule :match "https?://"
                                :capture :whole
                                :action #'open-url-in-firefox)
              (wand:create-rule :match "file:"
                                :capture :after
                                :action #'find-file)
              (wand:create-rule :match "#> "
                                :capture :after
                                :action #'(lambda (string)
                                            (eval (read string)))))))

;; Random Packages
(load-theme 'doom-nord-light t)
(global-git-gutter-mode t)
(yas-global-mode t)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(setq-default cursor-type 'box)
(set-cursor-color "#D4AFD7")
(global-hl-todo-mode)
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

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun my/kill-word-or-region ()
  "Kill the `word' or `region' at point."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (let ((bounds (bounds-of-thing-at-point 'word)))
        (if bounds
        (kill-region (car bounds) (cdr bounds))
      (user-error "Trying to execute nil")))))

(defun my/eval-shell-and-replace ()
  "Evals shell and replaces it."
  (interactive)
  (my/kill-word-or-region)
  (insert (shell-command-to-string (car kill-ring))))

(defun my/eval-shell-and-replace-buffer ()
  "Evals shell on entire buffer."
  (interactive)
  (my/kill-word-or-region)
  (message (car kill-ring)))

;;; Keybinds

(global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
(global-set-key (kbd "C-k") 'crux-smart-kill-line)
(global-set-key (kbd "C-o") 'crux-smart-open-line)
(global-set-key (kbd "M-o") 'crux-smart-open-line-above)
(global-set-key (kbd "M-<left>") (lambda () (interactive) (transpose-words -1)))
(global-set-key (kbd "M-<right>") (lambda () (interactive) (transpose-words 1)))
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "C-S-<down>") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-S-<up>") (lambda () (interactive)
                                   (crux-duplicate-current-line-or-region 1)
                                   (previous-line)
                                   (scroll-up-line)))
(global-set-key (kbd "C-S-<backspace>") 'crux-kill-whole-line)

;; Mouse Binds!!!
;; (global-set-key (kbd "<up-mouse-2>") 'my/eval-shell-and-replace)
;; (global-set-key (kbd "<down-mouse-2> <down-mouse-1>") 'my/eval-shell-and-replace-buffer)
;; (global-set-key (kbd "<mouse-3>") 'wand:execute)
