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

(when (not package-archive-contents)
    (package-refresh-contents))

(eval-when-compile
  (require 'use-package))


; @pre-require mainly functions that should be in emacs already which i
; may or may not have copied XDDDDDDDDDDDDDDDDDDDDDDDD
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
(global-set-key (kbd "C-c C-r") 'reset-erc-track-mode)

(defun buf-move-up ()
  "Swap the current buffer and the buffer above the split.
If there is no split, ie now window above the current one, an
error is signaled."
;;  "Switches between the current buffer, and the buffer above the
;;  split, if possible."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'up))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No window above this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(defun buf-move-down ()
"Swap the current buffer and the buffer under the split.
If there is no split, ie now window under the current one, an
error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'down))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (or (null other-win) 
            (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
        (error "No window under this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(defun buf-move-left ()
"Swap the current buffer and the buffer on the left of the split.
If there is no split, ie now window on the left of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'left))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No left split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(defun buf-move-right ()
"Swap the current buffer and the buffer on the right of the split.
If there is no split, ie now window on the right of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No right split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))


; @require
(require 'evil)
(require 'evil-leader)
(require 'linum-relative)
(require 'helm)
(require 'company)
(require 'projectile)
;(require 'eclim)
(require 'autopair)
(require 'racer)
(require 'rust-mode)
(require 'org)
(require 'erc)
(require 'magit)
(require 'evil-magit)

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
(helm-linum-relative-mode)

; @org
(setq org-log-done t)
(eval-after-load 'org '(require 'org-pdfview))

(add-to-list 'org-file-apps 
             '("\\.pdf\\'" . (lambda (file link)
                                     (org-pdfview-open link))))

; @erc pretty much makes it so i can join erc with certian reqs
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
(erc-connect erc-twitch "irc.chat.twitch.tv" 6667 "TheRenzix" nil "")
(erc-connect erc-discord "127.0.0.1" 6667 "Renzix" nil "Akeyla10!")

;; @programming
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "TAB") #'company-ident-or-complete-common)
(add-hook 'text-mode-hook 'linum-relative-mode)
(add-hook 'prog-mode-hook 'linum-relative-mode)

; @c/cpp
(add-to-list 'company-backends 'company-c-headers)

; @Rust
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(setq rust-format-on-save t)

; @Python
(add-to-list 'company-backends 'company-anaconda)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

; @Java @TODO Add java support???
;(setq eclimd-autostart t)

;(defun my-java-mode-hook ()
;    (eclim-mode t))
;(add-hook 'java-mode-hook 'my-java-mode-hook)
;(company-emacs-eclim-setup)

; @visual
(load-theme 'apropospriate-dark t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

; @evil-mode
(evil-mode t)

; @evil-leader/@which-key
; @key-misc
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>" )
(evil-leader/set-key
  "<SPC>" 'helm-M-x
  "'" 'eshell
  "\"" 'term
  )
; @which-key
(which-key-add-key-based-replacements
  "<SPC> b" "Buffer"
  "<SPC> w" "Window"
  "<SPC> f" "Files"
  "<SPC> m" "Major-Mode"
  "<SPC> q" "Quit")
; @key-buffers
(evil-leader/set-key
  (kbd "b b") 'helm-buffers-list
  (kbd "b d") 'kill-this-buffer
  (kbd "b N") 'switch-to-prev-buffer
  (kbd "b n") 'switch-to-next-buffer)
; @key-window
(evil-leader/set-key
  (kbd "w h") 'windmove-left
  (kbd "w j") 'windmove-down
  (kbd "w k") 'windmove-up
  (kbd "w l") 'windmove-right
  (kbd "w n") 'buf-move-left
  (kbd "w m") 'buf-move-down
  (kbd "w ,") 'buf-move-up
  (kbd "w .") 'buf-move-right)
; @key-files
(evil-leader/set-key
  (kbd "f f") 'helm-find-files
  (kbd "f S") 'sudo-edit
  (kbd "f d") 'delete-file-and-buffer
  (kbd "f r") 'rename-file-and-buffer
  (kbd "f s") 'save-buffer
  (kbd "f e e") (lambda() (interactive) (find-file "/home/genzix/.emacs"))
  (kbd "f e b") (lambda() (interactive) (find-file "/home/genzix/.config/bspwm/bspwmrc"))
  (kbd "f e s") (lambda() (interactive) (find-file "/home/genzix/.config/sxhkd/sxhkdrc_bspwm"))
  (kbd "f e m") (lambda() (interactive) (find-file "/sudo::/etc/portage/make.conf")))
; @key-quit
(evil-leader/set-key
  (kbd "q q") 'save-buffers-kill-emacs
  (kbd "q a") 'kill-emacs
  (kbd "q r") 'restart-emacs)
; @TODO(renzix): make a helm buffer for each major mode i use
; @key-rust-major
(evil-leader/set-key-for-mode 'rust-mode
  (kbd "m f") 'cargo-process-fmt
  (kbd "m r") 'cargo-process-run
  (kbd "m d") 'cargo-process-doc
  (kbd "m o") 'cargo-process-doc-open
  (kbd "m t") 'cargo-process-test
  (kbd "m c") 'cargo-process-check
  (kbd "m R") 'cargo-process-clean
  (kbd "m n") 'cargo-process-new
  (kbd "m u") 'cargo-process-update
  (kbd "m b") 'cargo-process-build)

; @config misc
(setq inhibit-startup-screen t)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
(autopair-global-mode t)
(setq initial-buffer-choice 'eshell)
(setq explicit-shell-file-name "/bin/bash")
(which-key-mode)

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
    (evil-magit which-key helm-descbinds org-pdfview magit helm-make cargo helm-ag-r helm-ag helm-projectile web-mode linum-relative company-emacs-eclim company-anaconda restart-emacs helm-tramp python-mode company-c-headers helm-company company-racer auto-complete autopair apropospriate-theme company-lsp racer rust-mode zenburn-theme helm use-package evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
