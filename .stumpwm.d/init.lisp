(in-package :stumpwm)

;; Autostart stuff
(run-shell-command "setxkbmap -option caps:swapescape")

;; @TODO(renzix): Maybe make this more vim like where as keys can
;; be prefixed with n,p,r instead of postfix
(defmacro make-program-binding (program-name window-class &optional alias)
  "Create run-or-raise and run-or-pull commands for program-name
  window-class is the windows-class
  Also add keybinding to the commands. 
  C-keybinding r calls run-or-raise
  C-keybinding p calls run-or-pull
  C-keybinding n creates a new instance of the program"
  (if (not alias)
      (setf alias program-name))
  `(progn
     (defvar ,(intern (format nil "*~a-map*" alias)) nil)

     (defcommand ,(intern (format nil "~a" alias)) () () (run-shell-command ,program-name))

     (defcommand ,(intern (format nil "run-or-raise-~a" alias)) () ()
		 (run-or-raise ,program-name '(:class ,window-class)))

     (defcommand ,(intern (format nil "run-or-pull-~a" alias)) () ()
		 (run-or-pull ,program-name '(:class ,window-class)))

     (fill-keymap ,(intern (format nil "*~a-map*" alias))
		  (kbd "p") ,(format nil "run-or-pull-~a" alias)
		  (kbd "r") ,(format nil "run-or-raise-~a" alias)
		  (kbd "n") ,(format nil "~a" alias))))

(make-program-binding "firefox" "Firefox" "firefox")
(make-program-binding "thunar" "Thunar" "thunar")
(make-program-binding "urxvt" "urxvt")
(make-program-binding "emacs" "Emacs")
(make-program-binding "spotify" "Spotify" "spotify")
(make-program-binding "discord" "Discord") ; Make this work properly??? (discord doesn't pull properly)
(make-program-binding "pavucontrol" "Pavucontrol")

;; Commands
(defcommand rofi () ()
	    "Does rofi stuff"
	    (run-shell-command "rofi -show run"))

;; Modal Keybinds Stuff
(set-prefix-key (kbd "C-`"))
(define-key *top-map* (kbd "C-~") "command-mode")
(define-key *root-map* (kbd "ESC") "abort") ; can be used to exit command mode (defaults to C-g)
(define-key *root-map* (kbd "C-`") "window-send-string `")
(define-key *root-map* (kbd "C-~") "window-send-string ~")
(define-key *root-map* (kbd "SPC") "rofi")

;; Opens new apps
(define-key *root-map* (kbd "RET") |*urxvt-map*|)
(define-key *root-map* (kbd "i") |*firefox-map*|)
(define-key *root-map* (kbd "e") |*emacs-map*|)
;; Extra keybinds
(defvar *app-map*
  (make-sparse-keymap)
  "Open a bunch of different applications")
(define-key *root-map* (kbd "o") *app-map*)
(define-key *app-map* (kbd "f") |*thunar-map*|)
(define-key *app-map* (kbd "m") |*spotify-map*|)
(define-key *app-map* (kbd "d") |*discord-map*|)
(define-key *app-map* (kbd "a") |*pavucontrol-map*|)
;; @TODO(renzix): Make a rofi with dmenu to run/pull/new

(defvar *frame-map*
  (make-sparse-keymap)
  "Simple monitor keybinds")
(define-key *root-map* (kbd "f") *frame-map*)
(define-key *frame-map* (kbd "1") "fselect 0")
(define-key *frame-map* (kbd "2") "fselect 1")
(define-key *frame-map* (kbd "3") "fselect 2")

;; Moves stuff around
(define-key *root-map* (kbd "h") "move-focus left") ; Just moves focus of windows
(define-key *root-map* (kbd "j") "move-focus down")
(define-key *root-map* (kbd "k") "move-focus up")
(define-key *root-map* (kbd "l") "move-focus right")
(define-key *root-map* (kbd "H") "move-window left") ; These 4 overide current window (and dont move the other one)
(define-key *root-map* (kbd "J") "move-window down")
(define-key *root-map* (kbd "K") "move-window up")
(define-key *root-map* (kbd "L") "move-window right")
(define-key *root-map* (kbd "C-h") "exchange-direction left") ; swaps the current window with one (cant do empty windows)
(define-key *root-map* (kbd "C-j") "exchange-direction down")
(define-key *root-map* (kbd "C-k") "exchange-direction up")
(define-key *root-map* (kbd "C-l") "exchange-direction right")
(define-key *root-map* (kbd "s") "vsplit") ; idk why emacs and stumpwm are different
(define-key *root-map* (kbd "v") "hsplit") ; But im gonna use the emacs version anyway
(define-key *root-map* (kbd "c") "remove-split") ; close frame
(define-key *root-map* (kbd "a") "only") ; alone (closes all other windows in frame)
(define-key *root-map* (kbd "d") "delete-window")
(define-key *root-map* (kbd "D") "kill-window")

;; Emacs keybinds @TODO(renzix): Make it work
;; (define-key *root-map* (kbd "e") *emacs-map*)

;; Status bar @TODO(renzix): Make this work
