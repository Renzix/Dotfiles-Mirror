; @package updating/managment
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


; @pre-require mainly functions that should be in emacs already which i may or maynot
; have copied XDDDDDDDDDDDDDDDDDDDDDDDD
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
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

(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))
(global-set-key (kbd "C-c r") 'reset-erc-track-mode)



; @require
(require 'evil)
(require 'evil-leader)
(require 'helm)
(require 'company)
(require 'racer)
(require 'autopair)
(require 'rust-mode)
(require 'org)
(require 'erc)

; @visual
(load-theme 'apropospriate-dark t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

; @evil-mode
(evil-mode t)

; @evil-leader
; @key-misc
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>" )
(evil-leader/set-key "<SPC>" 'helm-M-x)
(evil-leader/set-key "'" 'eshell)
(evil-leader/set-key "\"" 'term "/bin/bash")
; @key-buffers
(evil-leader/set-key (kbd "b b") 'helm-buffers-list)
(evil-leader/set-key (kbd "b d") 'kill-this-buffer)
(evil-leader/set-key (kbd "b N") 'switch-to-prev-buffer)
(evil-leader/set-key (kbd "b n") 'switch-to-next-buffer)
; @key-files
(evil-leader/set-key (kbd "f f") 'helm-find-files)
(evil-leader/set-key (kbd "f S") 'sudo-edit)
(evil-leader/set-key (kbd "f d") 'delete-file-and-buffer)
(evil-leader/set-key (kbd "f r") 'rename-file-and-buffer)
(evil-leader/set-key (kbd "f s") 'save-buffer)
(evil-leader/set-key (kbd "f e e") (lambda() (interactive) (find-file "/home/genzix/.emacs")))
(evil-leader/set-key (kbd "f e b") (lambda() (interactive) (find-file "/home/genzix/.config/bspwm/bspwmrc")))
(evil-leader/set-key (kbd "f e s") (lambda() (interactive) (find-file "/home/genzix/.config/sxhkd/sxhkdrc_bspwm")))
(evil-leader/set-key (kbd "f e m") (lambda() (interactive) (find-file "/sudo::/etc/portage/make.conf")))
; @key-quit
(evil-leader/set-key (kbd "q q") 'save-buffers-kill-emacs)
(evil-leader/set-key (kbd "q a") 'kill-emacs)
(evil-leader/set-key (kbd "q r") 'restart-emacs)
; modes for leader @TODO(renzix): make a helm buffer for each major mode i use
;(evil-leader/set-key-for-mode 'emacs-lisp-mode (kbd "m")

; @text
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

; @helm
(helm-autoresize-mode t)
(setq helm-autoresize-max-height 30)
(setq helm-display-header-line nil)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(helm-mode t)

; @org
(setq org-log-done t)

; @erc

(setq erc-hide-list '("JOIN" "PART" "QUIT" "ROOT"))
(setq erc-kill-buffer-on-part t)
(setq erc-kill-queries-on-quit t)
(setq erc-kill-server-buffer-on-quit t)

(defmacro erc-connect (command server port nick ssl pass)
    "Create interactive command `command', for connecting to an IRC server. The
   command uses interactive mode if passed an argument."
    (fset command
          `(lambda (arg)
             (interactive "p")
	           (if (not (= 1 arg))
	               (call-interactively 'erc)
	             (let ((erc-connect-function ',(if ssl 'erc-open-ssl-stream 'open-network-stream)))
 	               (erc :server ,server :port ,port :nick ,nick :password ,pass))))))
(erc-connect erc-twitch "irc.chat.twitch.tv" 6667 "TheRenzix" nil "oauth:n4um1shlxi6f84zswzutfhx7c1azd5")
(erc-connect erc-discord "127.0.0.1" 6667 "Renzix" nil "Akeyla10!")

;; @programming
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "TAB") #'company-ident-or-complete-common)

; @c/cpp
(add-to-list 'company-backends 'company-c-headers)

; @Rust
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(setq rust-format-on-save t)

; @Python
(eval-after-load "company"
 '(add-to-list 'company-backends 'company-anaconda))
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

; @config
(setq inhibit-startup-screen t)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
(autopair-global-mode t)
(setq initial-buffer-choice 'eshell)
(setq explicit-shell-file-name "/bin/bash")

; @system
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "0c32e4f0789f567a560be625f239ee9ec651e524e46a4708eb4aba3b9cdc89c5" default)))
 '(package-selected-packages
   (quote
    (company-anaconda restart-emacs helm-tramp python-mode company-c-headers helm-company company-racer auto-complete autopair apropospriate-theme company-lsp racer rust-mode zenburn-theme helm use-package evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
