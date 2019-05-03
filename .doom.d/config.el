;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; Better defaults
(setq display-line-numbers-type 'relative
           display-line-numbers-current-absolute t
           display-line-numbers-width 4
           display-line-numbers-widen t)

;; Keybindings
(map!
 :n ";" #'counsel-M-x
 :n "," (general-simulate-key "SPC m")
 :n "`" #'magit-status
 :n "g c" #'comment-line
 :n "\\" #'projectile-find-file)

;; Package configuration
(after! elcord
  (elcord-mode))
