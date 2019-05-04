;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;;; Code:
;; Functions
(defun indent-buffer ()
  (interactive)
  (save-excursion
	(indent-region (point-min) (point-max) nil)))

(defun rename-current-buffer-and-file ()
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

;; Variables/Hooks
(add-hook 'after-save-hook '(lambda () (async-start (bookmark-set (buffer-name) nil))))
(add-hook 'after-save-hook '(lambda () (async-start (bookmark-set "LastSave" nil))))
(setq display-line-numbers-type 'relative
      display-line-numbers-current-absolute t
      display-line-numbers-width 4
      display-line-numbers-widen t
      backup-directory-alist `(("." . "~/.saves")
                               backup-by-copying t
                               delete-old-versions t
                               kept-new-versions 10
                               kept-old-versions 10
                               version-control t)
      vterm-shell "ion")



;; Keybindings
(map!
 :n ";" #'counsel-M-x
 :n "," (general-simulate-key "SPC m")
 :n "`" #'magit-status ;; @TODO(renzix): Make this open in a new tab???
 :n "\\" #'projectile-find-file
 :n "g c" #'comment-line
 :n "g =" #'indent-buffer
 :nimv "M-h" #'evil-window-left
 :nimv "M-j" #'evil-window-down
 :nimv "M-k" #'evil-window-up
 :nimv "M-l" #'evil-window-right
 :nimv "M-s" #'evil-window-split
 :nimv "M-v" #'evil-window-vsplit
 :nimv "M-d" #'evil-delete-buffer
 :nimv "M-c" #'evil-window-delete
 :nimv "M-t" #'+workspace:new
 :nimv "M-T" #'+workspace:delete
 :nimv "M-]" #'+workspace:switch-next
 :nimv "M-[" #'+workspace:switch-previous
 :n "SPC p x v" #'+vterm/open-popup-in-project) ;; idk why this isnt here already

;; Ex commands
(evil-ex-define-cmd "config" 'doom/open-private-config)
(evil-ex-define-cmd "buffer" '+ivy/switch-buffer)
(evil-ex-define-cmd "rename" 'rename-current-buffer-and-file)
(evil-ex-define-cmd "reload" 'doom/reload)
(evil-ex-define-cmd "refresh" 'doom//refresh)
(evil-ex-define-cmd "eshell" '+eshell/open-popup)
(evil-ex-define-cmd "terminal" '+vterm/open-popup)

;; Package configuration
(after! 'elcord
  (elcord-mode))
