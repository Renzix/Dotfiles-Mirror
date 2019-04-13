(in-package :stumpwm)

;; Autostart stuff
(run-shell-command "setxkbmap -option caps:swapescape")
(run-shell-command "xmodmap -e \"clear mod4\" && xmodmap -e \"keycode 133=F1\" && xmodmap -e \"clear mod4\"")
(run-shell-command "xrandr --output DVI-D-0 --left-of DP-4 --auto && xrandr --output DP-3 --right-of DP-4")
;;(run-shell-command "emacs --daemon")

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
     (defcommand ,(intern (format nil "run-~a" alias)) () () (run-shell-command ,program-name))
     (defcommand ,(intern (format nil "run-or-raise-~a" alias)) () ()
		 (run-or-raise ,program-name '(:class ,window-class)))
     (defcommand ,(intern (format nil "run-or-pull-~a" alias)) () ()
		 (run-or-pull ,program-name '(:class ,window-class)))))

(make-program-binding "firefox-bin" "Firefox" "firefox")
(make-program-binding "thunar" "Thunar" "thunar")
(make-program-binding "alacritty -e ion" "Alacritty" "alacritty")
(make-program-binding "emacs" "Emacs")
(make-program-binding "emacsclient -ca \"\"" "Emacs" "emacs_frame")
(make-program-binding "spotify" "Spotify" "spotify")
(make-program-binding "pavucontrol" "Pavucontrol")

(defmacro make-motion-binding (name command &optional keybind)
  "Makes a motion binding for a bunch programs"
  (if (not keybind)
      `(progn
	 (define-key *root-map* (kbd "RET") ,(format nil "~a-alacritty" command))
	 (define-key *root-map* (kbd "e")   ,(format nil "~a-emacs_frame" command))
	 (define-key *root-map* (kbd "f")   ,(format nil "~a-thunar" command))
	 (define-key *root-map* (kbd "i")   ,(format nil "~a-firefox" command)))
      `(progn 
	 (defvar ,(intern (format nil "*~a-map*" name)) nil)
	 (define-key *root-map* ,keybind ,(intern (format nil "*~a-map*" name)))
	 (fill-keymap ,(intern (format nil "*~a-map*" name))
		      (kbd "RET") ,(format nil "~a-alacritty" command)
		      (kbd "e")   ,(format nil "~a-emacs_frame" command)
		      (kbd "f")   ,(format nil "~a-thunar" command)
		      (kbd "i")   ,(format nil "~a-firefox" command)))))

;; Modal Keybinds Stuff
(set-prefix-key (kbd "C-F1"))
(define-key *top-map* (kbd "F1") "command-mode") ; @KEYBIND(renzix): Make a better keybind so i can do somthing like this in emacs too
(define-key *root-map* (kbd "ESC") "abort") ; can be used to exit command mode (defaults to C-g)

(make-motion-binding "run"   "run"          nil)
(make-motion-binding "raise" "run-or-raise" (kbd "r"))
(make-motion-binding "pull"  "run-or-pull"  (kbd "p"))

;; Moves stuff around
(define-key *root-map* (kbd "n") "next")
(define-key *root-map* (kbd "p") "previous")
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
(define-key *root-map* (kbd "a") "only") ; alone (gets rid of all other windows in frame doesn't close them)
(define-key *root-map* (kbd "d") "delete-window")
(define-key *root-map* (kbd "D") "kill-window")

;; Emacs keybinds @TODO(renzix): Make it work
;; (define-key *root-map* (kbd "C-e") *emacs-map*)

;; Status bar @TODO(renzix): Make this good
;; (setf *mode-line-position* :bottom)
;; (stumpwm:toggle-mode-line (stumpwm:current-screen)
;; 			  (stumpwm:current-head))

;; (setf *screen-mode-line-format*
;;       (list "%w | "
;; 	    '(:eval (stumpwm:run-shell-command "date" t))))

