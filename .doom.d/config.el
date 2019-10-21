(setq confirm-kill-emacs nil)
(set-evil-initial-state! 'term-mode    'emacs)
(after! calc
  (set-evil-initial-state! 'calc-mode    'emacs))
(after! vterm
  (set-evil-initial-state! 'vterm-mode   'emacs))
(set-evil-initial-state! 'org-mode     'emacs)
(set-evil-initial-state! 'eshell-mode  'emacs)

(when (display-graphic-p)
  (defvar renzix-weekday (format-time-string "%w"))
  (cond ((string= "0" renzix-weekday) ;; Sunday
         (load-theme 'doom-dracula t))
        ((string= "1" renzix-weekday) ;; Monday
         (load-theme 'doom-opera t))
        ((string= "2" renzix-weekday) ;; Tuesday
         (load-theme 'apropospriate-dark t))
        ((string= "3" renzix-weekday) ;; Wednesday
         (load-theme 'doom-molokai t))
        ((string= "4" renzix-weekday) ;; Thursday
         (load-theme 'doom-nord t))
        ((string= "5" renzix-weekday) ;; Friday
         (load-theme 'monokai t))
        ((string= "6" renzix-weekday) ;; Saturday
         (load-theme 'doom-one t))))

(setq evil-insert-state-cursor   '(bar "#FF00FF")
      evil-normal-state-cursor   '(box "#6666F6")
      evil-motion-state-cursor   '(hollow "#FFF500")
      evil-replace-state-cursor  '(hbar "#BF2222")
      evil-operator-state-cursor '(box "#FFA500")
      evil-visual-state-cursor   '(hollow "#FFFFFF")
      evil-emacs-state-cursor    '(box "#90EE90"))
(setq-default cursor-type 'bar)
(blink-cursor-mode 1)

(global-hl-line-mode)

(set-face-attribute 'region nil :background "#07B") ;; blue

;; @TODO(Renzix): Make this work in all themes?
(setq whitespace-style '(trailing lines-tail space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 81)
(global-whitespace-mode)
;;(add-hook! prog-mode-hook (lambda () (highlight-regexp ".\{80\}\(.\)" 'hi-aquamarine "\\2"))) ;; @TODO(Renzix): Make this work as expected

(global-display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative
              display-line-numbers-current-absolute t
              display-line-numbers-width 3
              display-line-numbers-widen t)

(defun my/rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: "
                                      (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun my/delete-file-and-buffer ()
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

(defun my/get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun my/helm-projectile-find-file-or-project ()
  "Does switch project if not in a project and 'find-file' if in one."
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-find-file)
    (helm-projectile-switch-project)))

(defun my/helm-projectile-find-file-or-find-file ()
  "Does switch project if not in a project and 'find-file' if in one."
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-find-file)
    (helm-find-files nil)))

(defun my/helm-projectile-search-or-project ()
  "Does switch project if not in a project and search all files in said project."
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-ag)
    (helm-projectile-switch-project)))

(defun my/move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

      Move point to the first non-whitespace character on this line.
      If point is already there, move to the beginning of the line.
      Effectively toggle between the first non-whitespace character and
      the beginning of the line.

      If ARG is not nil or 1, move forward ARG - 1 lines first.  If
      point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (move-beginning-of-line 1)
    (when (= orig-point (point))
      (back-to-indentation))))

(defun my/smart-open-line (arg)
  "Insert an empty line after the current line.
      Position the cursor at its beginning, according to the current mode."
  (interactive "P")
  (if arg
      (progn
        (move-beginning-of-line nil)
        (newline-and-indent)
        (forward-line -1)
        (indent-according-to-mode))
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(defun my/smart-indent ()
  "If a region is selected indent that.
If given ARG indent the current line.
Else indent the entire buffer."
  (interactive)
  (save-excursion
    (if (region-active-p) (progn
                            (indent-region (region-beginning) (region-end))
                            (message "Indenting region")))
    (if current-prefix-arg
        (progn
          (indent-region (line-beginning-position) (line-end-position))
          (message "Indenting line"))
      (progn
        (indent-region (point-min) (point-max))
        (message "Identing buffer")))))

(setq org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")

(set-irc-server! "chat.freenode.net"
                 `(
                   :tls t
                   :port 6697
                   :nick "Renzix"
                   :sasl-username ,(+pass-get-user "irc/freenode.net")
                   :sasl-password (lambda (&rest _) (+pass-get-secret "irc/freenode.net"))
                   :channels ("#emacs" "#kisslinux")))

(after! helm
  (setq helm-mode-line-string t))

(after! whole-line-or-region
  (whole-line-or-region-global-mode t))

(after! elcord
  (elcord-mode t))

(map!
 :nve "C-x t"   #'+eshell/here
 :nve "C-x C-t" #'+vterm/here
 (:map override
   :nvei "M-x"   (lambda! (message "use C-; or ; dumbass")))) ;; if i bind C-;...

(map!
 :e "C-x C-k" #'kill-this-buffer
 :e "C-x g"   #'magit-status
 :e "C-a"     #'my/move-beginning-of-line
 :e "C-e"     #'end-of-line
 :e "C-j"     #'avy-goto-char-2
 :e "C-\\"    #'er/expand-region
 :e "C-="     #'my/smart-indent
 :e "C-o"     #'my/smart-open-line
 :e "C-w"     #'whole-line-or-region-kill-region
 :e "C-u"     #'universal-argument ;; Doom rebinds this idk why
 (:map override
   :e "C-;"   #'helm-M-x ;; I dont know if i shoud have this or not
   :e "C-:"   #'evil-ex
   :e "M-p"   #'projectile-command-map
   :e "C-'"   #'helm-find-files
   :e "C-\""  #'helm-mini
   :e "M-'"   #'my/helm-projectile-find-file-or-project
   :e "M-\""  #'helm-projectile-ag))

(map!
 :nv "g p"   #'projectile-command-map
 :nv "g ="   #'my/smart-indent
 (:map override
   :nv ";"   #'helm-M-x
   :nv "|"   #'helm-mini
   :nv "s"   #'helm-find-files
   :nv "S"   #'my/helm-projectile-find-file-or-project
   :nv "U"   #'undo-tree-visualize ;; in vi U is undo line changes so you can undo the undo
   :nv "Q"   #'save-buffers-kill-terminal
   :nv "\\"  #'helm-projectile-ag))
;; @NOTE(Renzix) that I made these from evil functions to emacs function
;; for more compatibility and to make sure it works as expected.
(evil-ex-define-cmd "cfg" (lambda! (progn (find-file "~/Dotfiles/.doom.d/config.org")) (evil-normal-state)))
(evil-ex-define-cmd "pack[age]" (lambda! (find-file "~/Dotfiles/.doom.d/packages.el")))
(evil-ex-define-cmd "init" (lambda! (find-file "~/Dotfiles/.doom.d/init.el")))
(evil-ex-define-cmd "q[uit]" 'delete-window)
(evil-ex-define-cmd "bd" 'kill-this-buffer)

(map! (:map helm-map
        "C-i"   #'helm-select-action
        "C-j"   #'helm-execute-persistent-action
        "<tab>" #'helm-select-action ))

(map! :map org-mode-map
      "TAB" #'org/toggle-fold)

(map! :map vterm-mode-map
      :e "C-a" #'vterm--self-insert
      :e "C-e" #'vterm--self-insert
      :e "C-r" #'vterm--self-insert
      :e "C-s" #'vterm--self-insert
      :e "C-u" #'vterm--self-insert
      :e "C-x u" #'vterm--self-insert
      :e "C-/" #'vterm--self-insert
      :e "C-y" #'vterm--self-insert
      :e "M-y" #'vterm--self-insert)
