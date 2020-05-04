(setq confirm-kill-emacs nil)
(global-auto-revert-mode t) ;; @NOTE(Renzix): For working with IDE's or other editors
;; Minibuffers in minibuffers
(setq enable-recursive-minibuffers t)
;; Mark stuff
(setq transient-mark-mode nil
      set-mark-command-repeat-pop t)
;; Use eww as the default browser cuz its nice
(setq browse-url-browser-function 'eww-browse-url)

(load-theme 'doom-rouge t)
;; (when (display-graphic-p)
;;   (defvar renzix-weekday (format-time-string "%w"))
;;   (cond ((string= "0" renzix-weekday) ;; Sunday
;;          (load-theme 'doom-dracula t))
;;         ((string= "1" renzix-weekday) ;; Monday
;;          (load-theme 'doom-opera t))
;;         ((string= "2" renzix-weekday) ;; Tuesday
;;          (load-theme 'apropospriate-dark t))
;;         ((string= "3" renzix-weekday) ;; Wednesday
;;          (load-theme 'doom-molokai t))
;;         ((string= "4" renzix-weekday) ;; Thursday
;;          (load-theme 'doom-nord t))
;;         ((string= "5" renzix-weekday) ;; Friday
;;          (load-theme 'monokai t))
;;         ((string= "6" renzix-weekday) ;; Saturday
;;          (load-theme 'doom-one t))))

(global-hl-line-mode)

;; (set-face-attribute 'region nil :background "#07B") ;; blue
(setq show-paren-priority 999)
(set-face-background 'region nil)
(global-visible-mark-mode t)

;; @TODO(Renzix): Make this work in all themes?
(setq whitespace-style '(trailing lines-tail space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 81)
(add-hook! prog-mode-hook #'whitespace-mode)
;;(add-hook! prog-mode-hook (lambda () (highlight-regexp ".\{80\}\(.\)" 'hi-aquamarine "\\2"))) ;; @TODO(Renzix): Make this work as expected

(global-display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative
              display-line-numbers-current-absolute t
              display-line-numbers-width 3
              display-line-numbers-widen t)

(setq doom-font (font-spec :family "Fira Code" :weight 'bold :size 14))

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

(defun my/select-line (&optional arg allow-extend)
  "Selects the current line and puts the cursor at the
start of the line.  A plain C-u puts the cursor at the
end of the line and any numbers puts the cursor at the
start and selects multiple lines(positive is down)"
  (interactive "p\np")
  (unless arg (setq arg 1))
  (when (zerop arg)
    (error "Cannot mark zero lines"))
  (cond ((and allow-extend
              (or (and (eq last-command this-command) (mark t))
                  (and transient-mark-mode mark-active)))
         (set-mark
          (save-excursion
            (goto-char (mark))
            (forward-line arg)
            (point))))
        (t
         (forward-line arg)
         (push-mark nil t t)
         (forward-line (* -1 arg)))))

(defun my/select-word (&optional arg allow-extend)
  "Selects the current word and puts the cursor at the
start of the line.  A plain C-u puts the cursor at the
end of the line and any numbers puts the cursor at the
start and selects multiple lines(positive is down)"
  (interactive "p\np")
  (unless arg (setq arg 1))
  (when (zerop arg)
    (error "Cannot mark zero words"))
  (cond ((and allow-extend
              (or (and (eq last-command this-command) (mark t))
                  (and transient-mark-mode mark-active)))
         (set-mark
          (save-excursion
            (goto-char (mark))
            (forward-word arg)
            (point))))
        (t
         (forward-word arg)
         (push-mark nil t t)
         (backward-word arg))))  "Selects the current WORD"

(defun my/avy-goto-url ()
  "Goto url with avy. Accepts http, https and file currently. "
  (interactive)
  (avy--generic-jump "https?s://\\|file://" nil nil))

(after! org
  (setq +pretty-code-symbols
        (org-combine-plists +pretty-code-symbols
                            '(:pi "π"
                                  :tau "τ")))
  (set-pretty-symbols! 'perl6-mode
    :pi "pi"
    :tau "tau"))

(setq lsp-enable-indentation 'nil)

;; Enable this for default emacs-state
;;(after! evil 
;;  (setq evil-default-state 'emacs))

;; Enable this for default evil stuff
(set-evil-initial-state! 'term-mode    'emacs)
(after! calc
  (set-evil-initial-state! 'calc-mode  'emacs))
(after! vterm
  (set-evil-initial-state! 'vterm-mode 'emacs))
(set-evil-initial-state! 'org-mode     'emacs)
(set-evil-initial-state! 'eshell-mode  'emacs)
(set-evil-initial-state! 'matrix-mode  'emacs)

(set-evil-initial-state! 'erc-mode     'emacs)
(set-evil-initial-state! 'circe-mode   'emacs)

(setq evil-insert-state-cursor   '(bar "#FF00FF")
      evil-normal-state-cursor   '(box "#6666F6")
      evil-motion-state-cursor   '(hollow "#FFF500")
      evil-replace-state-cursor  '(hbar "#BF2222")
      evil-operator-state-cursor '(box "#FFA500")
      evil-visual-state-cursor   '(hollow "#FFFFFF")
      evil-emacs-state-cursor    '(box "#90EE90"))
(setq-default cursor-type 'bar)
(blink-cursor-mode 1)

(setq org-directory "~/Nextcloud/Documents"
      org-log-done 'timer
      org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
;; Fix broken thing with emacs 27 where cursor is being hidden inside org mode
(custom-set-faces!
  '((hl-line solaire-hl-line-face org-indent
             outline-1 outline-2 outline-3 outline-4 outline-5 outline-6 outline-7 outline-8)
    :extend t))

(set-irc-server! "chat.freenode.net"
                 `(:tls t
                   :port 6697
                   :nick "Renzix"
                   :sasl-username "renzix"
                   :sasl-password ,(string-trim (my/get-string-from-file "~/.config/freenode-pass"))
                   :channels (:after-auth "#nixhub" "#emacs" "#kisslinux")))

(set-irc-server! "127.0.0.1"
                 `(
                   :port 6667
                   :nick "Renzix"
                   :nickserv-password ,(string-trim (my/get-string-from-file "~/.config/freenode-pass"))))
(after! circe
  (require 'circe-chanop)
  ;; Add some nice commands here - @TODO(Renzix)
  ;; Other config options
  (setq lui-time-stamp-position 'right-margin
        lui-fill-type nil
        lui-track-bar-behavior 'before-switch-to-buffer
        lui-flyspell-p t
        lui-flyspell-alist '((".*" "american")))
  (defun my-lui-setup ()
    (setq fringes-outside-margins t
          right-margin-width 5
          word-wrap t
          wrap-prefix "    "))
  (load "lui-logging" nil t)
  (enable-lui-logging-globally)
  (enable-lui-autopaste)
  (enable-lui-track-bar)
  (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)
  (add-hook 'lui-mode-hook 'my-lui-setup))

(after! company-mode
  (add-to-list 'company-backends #'company-tabnine))

(after! helm
  (defvar helm-source-emacs-commands
    (helm-build-sync-source "Emacs commands"
      :candidates (lambda ()
                    (let ((cmds))
                      (mapatoms
                       (lambda (elt) (when (commandp elt) (push elt cmds))))
                      cmds))
      :coerce #'intern-soft
      :action #'command-execute)
    "A simple helm source for Emacs commands.")
  (setq helm-mode-line-string t)
  (helm-add-action-to-source "Get Help" (lambda (candidate) (helpful-function candidate)) helm-source-emacs-commands 2))
(defun my/helm-M-x ()
  (interactive)
  "My emacs helm-M-x which also does Get Help so i dont have to C-h f"
  (helm :sources helm-source-emacs-commands))

(after! expand-region
  (setq er/try-expand-list nil)
  (setq er/try-expand-list
        (append '(er/mark-method-call
                  er/mark-inside-quotes
                  er/mark-inside-pairs
                  er/mark-comment
                  er/mark-url
                  er/mark-email
                  er/mark-defun)
                er/try-expand-list)))

(after! elcord
  (elcord-mode t))

(use-package! piper :commands piper)

(use-package! helm-twitch
  :commands helm-twitch
  :init (setq twitch-api-oauth-token (string-trim (my/get-string-from-file "~/.config/twitch-oauth"))
              twitch-api-username "therenzix"))

(setq doom-leader-alt-key "C-c"
      doom-localleader-alt-key "C-c l")
(map!
 :nve  "C-x t"   #'+eshell/here
 :nve  "C-x C-t" #'+vterm/here
 :nve  "C-x C-d" #'projectile-dired
 :nvei "C-<tab>"   #'+treemacs/toggle
 (:leader ;; Make this open guix if guixsd???
   :desc "System Packages" "P" #'helm-system-packages
   :desc "Open Link" "o l" #'my/avy-goto-url)
 (:map smerge-mode-map ;; cuz C-c ^ is awful C-c l m is much better
   :localleader
   :desc "Merge" "m" #'smerge-command-prefix))

(map!
 :e "C-x C-k" #'kill-this-buffer
 :e "C-a"     #'my/move-beginning-of-line
 :e "C-e"     #'end-of-line
 :e "C-j"     #'avy-goto-char-2
 :e "M-l"     #'my/select-line
 :e "M-e"     #'my/select-word
 :e "M-a"     #'er/mark-inside-pairs
 :e "M-h"     #'mark-defun
 :e "M-y"     #'yank-pop
 :e "C-\\"    #'er/expand-region
 :e "C-="     #'my/smart-indent
 :e "C-o"     #'my/smart-open-line
 :e "C-M-u"   #'sp-backward-up-sexp
 :e "C-u"     #'universal-argument ;; Doom rebinds this idk why
 (:map override
   :e "C-;"   #'my/helm-M-x ;; I dont know if i shoud have this or not
   :e "C-:"   #'evil-ex
   :e "C-'"   #'helm-find-files
   :e "C-\""  #'helm-mini
   :e "M-'"   #'+helm/projectile-find-file
   :e "M-\""  #'+helm/project-search))

(map!
 :nv "Q"     (kbd "@q")
 :nv "g ="   #'my/smart-indent
 (:map override
   :nvm ";"   #'my/helm-M-x
   :nvm "|"   #'helm-mini
   :nvm "s"   #'helm-find-files
   :nvm "S"   #'+helm/projectile-find-file
   :nv  "U"   #'undo-tree-visualize ;; in vi U is undo line changes so you can undo the undo
   :nvm "\\"  #'+helm/project-search))
;; @NOTE(Renzix) that I made these from evil functions to emacs function
;; for more compatibility and to make sure it works as expected.
(evil-ex-define-cmd "cfg" (lambda! (find-file "~/Dotfiles/.doom.d/config.org")))
(evil-ex-define-cmd "conf[ig]" (lambda! (find-file "~/Dotfiles/.doom.d/config.org")))
(evil-ex-define-cmd "pkg" (lambda! (find-file "~/Dotfiles/.doom.d/packages.el")))
(evil-ex-define-cmd "pack[age]" (lambda! (find-file "~/Dotfiles/.doom.d/packages.el")))
(evil-ex-define-cmd "init" (lambda! (find-file "~/Dotfiles/.doom.d/init.el")))
(evil-ex-define-cmd "blog" (lambda! (find-file "~/Blog/blog.org")))
(evil-ex-define-cmd "a[genda]" #'org-agenda)
(evil-ex-define-cmd "q[uit]" #'delete-window)
(evil-ex-define-cmd "bd" #'kill-this-buffer)

(map! (:map helm-map
        "C-m" #'helm-maybe-exit-minibuffer
        ;;"<return>" #'helm-maybe-exit-minibuffer
        "C-j"   #'helm-execute-persistent-action
        "C-<return>" #'helm-execute-persistent-action
        "C-i"   #'helm-select-action ;; Terminal Tab
        "<tab>" #'helm-select-action))

(map! :map org-mode-map
      :e "C-c e" #'org-edit-src-code) ;; @TODO(Renzix): Make this work on C-c '

(map! :map vterm-mode-map
      :e "C-a"   #'vterm--self-insert
      :e "C-h"   #'vterm--self-insert
      :e "C-j"   #'vterm--self-insert
      :e "C-k"   #'vterm--self-insert
      :e "C-l"   #'vterm--self-insert
      :e "C-e"   #'vterm--self-insert
      :e "C-r"   #'vterm--self-insert
      :e "C-s"   #'vterm--self-insert
      :e "C-u"   #'vterm--self-insert
      :e "C-x u" #'vterm--self-insert
      :e "C-/"   #'vterm--self-insert
      :e "C-y"   #'vterm--self-insert
      :e "M-y"   #'vterm--self-insert
      :e "RET"   #'vterm--self-insert)
