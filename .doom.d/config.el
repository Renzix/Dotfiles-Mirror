;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; Better defaults
(setq display-line-numbers-type 'relative
           display-line-numbers-current-absolute t
           display-line-numbers-width 4
           display-line-numbers-widen t)
(add-hook 'after-save-hook '(lambda () (async-start (bookmark-set (buffer-name) nil))))
(add-hook 'after-save-hook '(lambda () (async-start (bookmark-set "LastSave" nil))))

;; Keybindings
(map!
 :n ";" #'counsel-M-x
 :n "," (general-simulate-key "SPC m")
 :n "`" #'magit-status
 :n "g c" #'comment-line
 :n "\\" #'projectile-find-file)

;; Ex commands
(evil-ex-define-cmd "es" 'eshell)
(evil-ex-define-cmd "te" 'vterm)

;; Package configuration
(after! 'elcord
  (elcord-mode))
