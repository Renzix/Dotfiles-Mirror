(use-package apropospriate-theme
  :ensure t
  :config (load-theme 'apropospriate-dark t))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package general
  :defer -1
  :config (general-evil-setup t))

(defun eshell/clear ()
   "Clear the eshell buffer."
     (let ((inhibit-read-only t))
       (erase-buffer)
       (eshell-send-input)))
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

(use-package helm
  :ensure t
  :config 
  (helm-autoresize-mode t)
  (setq helm-autoresize-max-height 30)
  (setq helm-display-header-line nil)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (helm-mode t))

(use-package evil
  :config (evil-mode t))
(use-package god-mode)

(evil-define-state god
  "God state."
  :tag " <G> "
  :message "-- GOD MODE --"
  :entry-hook (evil-god-start-hook)
  :exit-hook (evil-god-stop-hook)
  :input-method t
  :intercept-esc nil)
(defun evil-god-start-hook ()
  "Run before entering `evil-god-state'."
  (evil-emacs-state 1)
  (god-local-mode 1))

(defun evil-god-stop-hook ()
  "Run before exiting `evil-god-state'."
  (evil-emacs-state -1)
  (god-local-mode -1))

(defun evil-god-fix-last-command ()
  "Change `last-command' to be the command before `evil-execute-in-god-state'."
  (setq last-command evil-god-last-command))

 (defun evil-stop-execute-in-god-state ()
  "Switch back to previous evil state."
  (unless (or (eq this-command #'evil-execute-in-god-state)
              (eq this-command #'universal-argument)
              (eq this-command #'universal-argument-minus)
              (eq this-command #'universal-argument-more)
              (eq this-command #'universal-argument-other-key)
              (eq this-command #'digit-argument)
              (eq this-command #'negative-argument)
              (minibufferp))
    (remove-hook 'pre-command-hook 'evil-god-fix-last-command)
    (remove-hook 'post-command-hook 'evil-stop-execute-in-god-state)
    (when (buffer-live-p evil-execute-in-god-state-buffer)
      (with-current-buffer evil-execute-in-god-state-buffer
        (if (and (eq evil-previous-state 'visual)
                 (not (use-region-p)))
            (progn
              (evil-change-to-previous-state)
              (evil-exit-visual-state))
          (evil-change-to-previous-state))))
	  (setq evil-execute-in-god-state-buffer nil))) 

(defvar evil-execute-in-god-state-buffer nil)
(defvar evil-god-last-command nil)
	  
(defun evil-start-god-state () "Execute commands in god state"
  (interactive)
  (setq evil-execute-in-god-state-buffer (current-buffer))
  (setq evil-god-last-command last-command)
  (cond
  ((evil-visual-state-p)
   (let ((mrk (mark))
    (pnt (point)))
      (evil-god-state)
      (set-mark mrk)
      (goto-char pnt)))
      (t
   (evil-god-state)))
  )
(defun evil-god-state-bail ()
  "Stop current God command and exit God state."
  (interactive)
  (evil-stop-execute-in-god-state)
  (evil-god-stop-hook)
  (evil-normal-state))

(linum-mode)
(linum-relative-global-mode)
(setq linum-relative-current-symbol "")
(set-face-attribute 'linum nil :height 100)
(set-face-attribute 'linum-relative-current-face nil :height 100)
(defun linum-update-window-scale-fix (win)
"fix linum for scaled text"
  (set-window-margins win
    (ceiling (* (if (boundp 'text-scale-mode-step)
      (expt text-scale-mode-step
        text-scale-mode-amount) 1)
    (if (car (window-margins))
      (car (window-margins)) 1)
    ))))
(advice-add #'linum-update-window :after #'linum-update-window-scale-fix)

(use-package which-key)
(which-key-mode)
(which-key-add-key-based-replacements
  "<SPC> b" "Buffer"
  "<SPC> w" "Window"
  "<SPC> f" "Files"
  "<SPC> q" "Quit"
  "<SPC> f e" "Config Files"
  "<SPC> f e e" ".emacs"
  "<SPC> f e s" "sxhkdrc"
  "<SPC> f e b" "bspwmrc"
  "<SPC> f e m" "make.conf")
(evil-define-key 'normal global-map (kbd "<escape>") 'evil-start-god-state)
(evil-define-key 'normal global-map (kbd ";") 'helm-M-x)
(evil-define-key 'emacs global-map (kbd "<escape>") 'evil-god-state-bail) 

;; Some other keybinds 
;; Kill-buffer C-x k
(general-define-key
  :states '(emacs)
  (kbd "M-x") 'helm-M-x
  (kbd "C-f C-f") 'helm-find-files
  (kbd "C-f C-S") 'sudo-edit
  (kbd "C-f C-d") 'delete-file-and-buffer
  (kbd "C-f C-r") 'rename-file-and-buffer
  (kbd "C-f C-s") 'save-buffer
  (kbd "C-f C-e C-e") (lambda() (interactive) (find-file "/home/genzix/.emacs.d/settings.org"))
  (kbd "C-f C-e C-b") (lambda() (interactive) (find-file "/home/genzix/.config/bspwm/bspwmrc"))
  (kbd "C-f C-e C-s") (lambda() (interactive) (find-file "/home/genzix/.config/sxhkd/sxhkdrc_bspwm"))
  (kbd "C-f C-e C-m") (lambda() (interactive) (find-file "/sudo::/etc/portage/make.conf"))
  (kbd "C-b C-b") 'helm-list-buffers
  (kbd "C-b C-N") 'switch-to-prev-buffer
  (kbd "C-b C-n") 'switch-to-next-buffer
  (kbd "C-b C-k") 'kill-this-buffer
  (kbd "C-l C-h") 'windmove-left ;; @TODO(renzix): Fix this to make it C-w or something
  (kbd "C-l C-j") 'windmove-down
  (kbd "C-l C-k") 'windmove-up
  (kbd "C-l C-l") 'windmove-right
  (kbd "C-l C-H") 'buf-move-left
  (kbd "C-l C-J") 'buf-move-down
  (kbd "C-l C-K") 'buf-move-up
  (kbd "C-l C-L") 'buf-move-right
  (kbd "C-q C-q") 'save-buffers-kill-emacs 
  (kbd "C-q C-a") 'kill-emacs
  (kbd "C-q C-r") 'restart-emacs)
(general-define-key
  :states '(normal)
  :prefix "SPC"
  (kbd "'") 'eshell
  (kbd "\"") 'term)

(use-package company
  :ensure t
  :defer t
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-irony :ensure t :defer t)
  (setq company-idle-delay        2
    company-minimum-prefix-length   2
    company-show-numbers            t
    company-tooltip-limit           20
    company-dabbrev-downcase        nil
    company-backends                '((company-irony company-gtags company-anaconda company-racer)))
  :bind ("<tab>" . company-indent-or-complete-common))
(setq tab-always-indent 'complete)

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(setq rust-format-on-save t)

(general-define-key
  :states '(normal)
  :keymaps 'rust-mode-map
  :prefix "," 
  (kbd "f") 'cargo-process-fmt
  (kbd "r") 'cargo-process-run
  (kbd "d") 'cargo-process-doc
  (kbd "o") 'cargo-process-doc-open
  (kbd "t") 'cargo-process-test
  (kbd "c") 'cargo-process-check
  (kbd "R") 'cargo-process-clean
  (kbd "n") 'cargo-process-new
  (kbd "u") 'cargo-process-update
  (kbd "b") 'cargo-process-build)

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

(use-package irony
  :ensure t
  :defer t
  :init
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)
  :config
    (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package autopair
  :config (autopair-global-mode t))

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

(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq inhibit-startup-screen t)
(setq initial-buffer-choice 'eshell)
