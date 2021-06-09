;;; -*- lexical-binding: t; -*-
(setq display-line-numbers-type 'relative
      confirm-kill-emacs nil
      browse-url-browser-function 'eww-browse-url
      search-whitespace-regexp ".*"
      isearch-lax-whitespace t
      scroll-margin 3
      whitespace-style '(trailing lines-tail space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 81)

(add-hook! prog-mode-hook #'whitespace-mode)
(global-undo-tree-mode t)

(setq doom-font (font-spec :family "Fira Code" :size 12)
      doom-variable-pitch-font (font-spec :family "Fira Code") ; inherits `doom-font''s :size
      doom-unicode-font (font-spec :family "Fira Code" :size 12)
      doom-big-font (font-spec :family "Fira Code" :size 19))
(load-theme 'doom-tomorrow-night t)

(defun my/forward-char-no-newline ()
  (interactive)
  (if
      (let
        ((line-number (line-number-at-pos)))
        (save-excursion
          (forward-char)
        (= line-number (line-number-at-pos))))
      (forward-char)
    (insert " ")))

(defun my/mark-line (arg)
  "Select the current line and move the cursor by ARG lines IF
no region is selected.

If a region is already selected when calling this command, only move
the cursor by ARG lines."
  (interactive "p")
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil))
  (forward-line arg))
(defun my/mark-nothing (arg)
  "Marks nothing"
  (interactive "p")
  (when (not (use-region-p))
    (set-mark-command nil)))

(defun my/move-previous-line ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun my/move-next-line ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun my/run-perl-on-region (value)
    (interactive "sPerl: ")
    (shell-command-on-region
     (region-beginning)
     (region-end)
     (if (eq (region-beginning) (region-end))
         (concat "perl -E '" value "'")
       (concat "perl -pE '" value "'"))
     (buffer-substring (region-beginning) (region-end))
     t
     "*perl errors*"
     nil))

(defun my/run-shell-on-region (value)
  (interactive "s shell: ")
  (shell-command-on-region
   (region-beginning)
   (region-end)
   (concat "sh -c '" value "'")
   (buffer-substring (region-beginning) (region-end))
   t
   "*shell errors*"
   nil))

(defvar fun nil)
(defmacro movement-selection-wrapper (wrappee)
  "Wrapper around a function that adds set-mark-command. "
  (setq fun (eval wrappee))
  (let* ((wrapper (intern (concat (symbol-name fun) "-selection-wrapper")))
        (arglist (make-symbol "arglist")))
  `(defun ,wrapper (&rest ,arglist)
     ,(concat (documentation fun) "\n But I do something more.")
     ,(interactive-form fun)
     (progn
       (set-mark-command nil)
       (apply (quote ,fun) ,arglist)))))

(defun generate-movement-objects (movement)
  (setq result nil)
  (dolist (value movement result)
    (setq result
          (let* ((key (car value))
                (fun (car (cdr value)))
                (thing (movement-selection-wrapper fun)))
            (cons `(,key
                    ,thing)
                  result)))))

(defun my/find-next-char (arg char)
  "Goes to next char. Negitive argument reverses the direction. "
  (interactive "p\ncChar: ")
  (if (> arg 0) (forward-char) (backward-char))
  (search-forward (char-to-string char) nil nil arg)
  (when (> arg 0) (backward-char)))

(defun my/find-previous-char (arg char)
  "Goes to previous char. Negitive argument reverses the direction. "
  (interactive "p\ncChar: ")
  (my/find-next-char (* arg -1) char))

(defun my/till-next-char (arg char)
  "Goes to the character before char. Negitive argument reverses the direction. "
  (interactive "p\ncChar: ")
  (if (> arg 0) (forward-char) (backward-char))
  (my/find-next-char arg char)
  (if (> 0 arg) (forward-char) (backward-char)))

(defun my/till-previous-char (arg char)
  "Goes to previous char. Negitive argument reverses the direction. "
  (interactive "p\ncChar: ")
  (my/till-next-char (* arg -1) char))

;; Credit to boon https://github.com/jyp/boon for this code (also partly god mode)
(defun boon-god-control-swap (event)
  "Swap the control 'bit' in EVENT, unless C-c <event> is a prefix reserved for modes."
  (interactive (list (read-key)))
  (cond
   ((memq event '(9 13 ?{ ?} ?\[ ?\] ?$ ?& ?= ?< ?> ?: ?\; ?/ ?? ?. ?, ?' ?\" )) event)
   ((<= event 27) (+ 96 event))
   ((not (eq 0 (logand (lsh 1 26) event))) (logxor (lsh 1 26) event))
   (t (list 'control event))))

(defun boon-c-god (arg)
  "Input a key sequence, prepending C- to each key (unless such
key is already reserved for minor mode, see
`boon-god-control-swap'), and run the command bound to that
sequence."
  (interactive "P")
  (let ((keys '((control c)))
        (binding (key-binding (kbd "C-c")))
        (key-vector (kbd "C-c"))
        (prompt "C-c-"))
    (while (and binding
                (or (eq binding 'mode-specific-command-prefix)
                    ;; if using universal prefix, the above will happen.
                    (not (commandp binding))))
      (let ((key (read-key (format "%s" prompt))))
        (if (eq key ?h) (describe-bindings key-vector) ;; h -> show help
          (push (boon-god-control-swap key) keys)
          (setq key-vector (vconcat (reverse keys)))
          (setq prompt (key-description key-vector))
          (setq binding (key-binding key-vector)))))
    (cond
     ((not binding) (error "No command bound to %s" prompt))
     ((commandp binding)
      (let ((current-prefix-arg arg)) (call-interactively binding)))
     (t (error "Key not bound to a command: %s" binding)))))

(defun my/replace-char (char)
  "Replaces the current char with a new char. "
  (interactive "cChar: ")
  (delete-char 1)
  (insert-char char)
  (backward-char)
  (ryo-modal-mode t))

(use-package! expand-region)
(use-package! key-chord
  :config (key-chord-mode 1))

(after! counsel
  (setq ivy-use-virtual-buffers t)
  ;; Bind C-k to kill buffer from `ivy-switch-buffer'
  (defun ivy-kill-buffer ()
    (interactive)
    (ivy-set-action 'kill-buffer)
    (ivy-call))

  (define-key ivy-minibuffer-map (kbd "C-k") #'ivy-kill-buffer)

  (defun ivy-delete-file ()
    (interactive)
    (ivy-set-action 'counsel-find-file-delete)
    (ivy-call))

  (define-key ivy-minibuffer-map (kbd "C-d") #'ivy-delete-file)
  (define-key ivy-minibuffer-map (kbd "C-s") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "C-r") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-n") #'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-t") #'ivy-previous-line))
(setq org-log-done 'time)

(after! dap-mode
  (require 'dap-gdb-lldb))

(after! ryo-modal
  (define-globalized-minor-mode my-global-ryo-mode ryo-modal-mode
    (lambda ()
      (unless
          (or (minibufferp)
              (string= major-mode "vterm-mode")
              (string= major-mode "magit-mode"))
        (ryo-modal-mode 1))))

  (my-global-ryo-mode 1)
  (map! "C-t" 'ryo-modal-mode
        "M-t" '+vterm/toggle)
  (key-chord-define-global "aa" 'ryo-modal-mode)
  (define-key key-translation-map (kbd "ESC") (kbd "C-t"))
  (let* ((movement
         '(("n" backward-char)
           ("e" next-line)
           ("i" previous-line)
           ("o" forward-char)
           ("E" crux-move-beginning-of-line)
           ("N" backward-word)
           ("O" forward-word)
           ("I" end-of-line)
           ("M-n" beginning-of-buffer)
           ("M-o" end-of-buffer)
           ("y" my/find-next-char)
           ("Y" my/find-previous-char)
           ("u" my/till-next-char)
           ("U" my/till-previous-char)
           ))
        (text-objects
         `(("s" er/mark-symbol :name "Symbol")
           ("SPC" my/mark-line :name "Line")
           ("q" er/mark-inside-quotes :name "Quotes")
           ("Q" er/mark-outside-quotes :name "Quotes (outside)")
           ("p" er/mark-inside-pairs :name "Pairs")
           ("P" er/mark-outside-pairs :name "Pairs (outside)")
           ("b" mark-whole-buffer :name "Buffer")
           ("<backspace>" my/mark-nothing :name "Nothing")
           ;; TODO: Add ',",{,(,[ and <
           ,@(generate-movement-objects movement))))
    (eval `(ryo-modal-keys
            ("v" ,text-objects)
            ("$" ,text-objects :then '(my/run-shell-on-region))
            ("t" ,text-objects :then '(kill-region))
            ("r" ,text-objects :then '(kill-region) :exit t)
            ("T" ,text-objects :then '(kill-ring-save))
            ("/" ,text-objects :then '(comment-or-uncomment-region))
            ,@movement)))
  (after! org
   (ryo-modal-keys
    ("|" org-fill-paragraph)))
  (ryo-modal-keys
   ;; Random stuff
   ("q" kmacro-end-or-call-macro)
   ("Q" kmacro-start-macro-or-insert-counter)
   ("C-q" counsel-kmacro)
   ("m" set-mark-command)
   ("M-E" crux-duplicate-current-line-or-region)
   ("M-e" my/move-next-line)
   ("M-i" my/move-previous-line)
   ("M-I" crux-duplicate-current-line-or-region)
   ("M-d" crux-smart-open-line)
   ("C-d" crux-smart-open-line-above)
   ("c" boon-c-god)
   ("h" my/replace-char)
   ("k" kill-this-buffer)

   ;; insert mode stuff
   ("d" crux-smart-open-line :exit t)
   ("D" crux-smart-open-line-above :exit t)
   ("a" my/forward-char-no-newline :exit t)
   ("A" end-of-line :exit t)

   ;; Buffer stuff
   ("f" find-file)
   ("F" counsel-locate)
   ("b" crux-switch-to-previous-buffer)
   ("C-f" +ivy/projectile-find-file)
   ("M-f" find-file-other-window)

   ("p" counsel-switch-buffer)
   ("P" counsel-projectile)
   ("C-p" counsel-projectile-switch-to-buffer)
   ("M-p" counsel-switch-buffer-other-window)

   ("M-s" +ivy/project-search)

   ;; undo
   ("z" undo)
   ("Z" undo-redo)
   ("M-z" undo-tree-visualize)

   ;; IDE stuff
   ("g" +lookup/definition)
   ("G" +lookup/references)
   ("M" exchange-point-and-mark)
   ("W" delete-other-windows)

   ;; Changing commands
   ("j" crux-top-join-line)
   ("s" yank)
   ("S" counsel-yank-pop))

  (ryo-modal-keys
   ;; First argument to ryo-modal-keys may be a list of keywords.
   ;; These keywords will be applied to all keybindings.
   (:norepeat t)
   ("SPC" universal-argument)
   ("-" "-")
   ("0" "C-0")
   ("1" "C-1")
   ("2" "C-2")
   ("3" "C-3")
   ("4" "C-4")
   ("5" "C-5")
   ("6" "C-6")
   ("7" "C-7")
   ("8" "C-8")
   ("9" "C-9"))

  (ryo-modal-key
   "x" '(("s" save-buffer)
         ("b" ibuffer)
         ("t" +vterm/toggle)
         ("x" counsel-M-x)
         ("k" save-buffers-kill-terminal)
         ("g" magit-status)
         ("c" +ivy/compile)
         ("o" crux-open-with)
         ("r" crux-rename-file-and-buffer)
         ("d" crux-delete-file-and-buffer)))

  (ryo-modal-key
   "w" '(("w" crux-other-window-or-switch-buffer)
         ("t" +workspace/close-window-or-workspace)
         ("s" split-window-right)
         ("o" delete-other-windows))))
