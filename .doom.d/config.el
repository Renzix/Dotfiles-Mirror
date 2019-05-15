;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;;; Code:
;; Functions
(defun indent-buffer ()
  "Idents the entire buffer."
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

(defun async-copy-file-and-make-directory (source destination)
  "Copies a file and make needed directories asyncrounously."
  (async-start (lambda ()
                 (require 'subr-x)
                 (make-directory (string-join (reverse (cdr (reverse (split-string destination "/")))) "/") 'parent)
                 (copy-file source destination :overwrite-if-already-exists)
                 destination)
               (lambda (results)
                 (message "Saved file to: %s" results))))

(defun backup-file ()
  "Make a backup which change with each folder."
  (interactive)
  (if (buffer-file-name)
      (let* ((top-directory "~/.saves")
             (currentName (buffer-file-name))
             (backupName (concat top-directory currentName
                                 (format-time-string "/%Y/%m/%d/%H%M") ".bak")))
        (async-copy-file-and-make-directory currentName backupName)
        t)
    (progn
      (user-error "Buffer is not a file")
      nil)))

;; This has a small bug that can only trigger if you get 2 or more tarballs and
;; archive twice within 1 second. It will delete your big tarball. Its probably
;; not worth fixing.
(defun archive-backups ()
  "Change backups into a tar.gz with the date archived. Requires
tar, ls and rm. Do not call twice within a second if there are 2
tarballs in the top directory (defaults to ~/.saves)."
  (interactive)
  (let* ((top-directory "~/.saves")
         (archive-name (concat top-directory "/archive-"
                               (format-time-string "%Y%m%d-%H%M%S") ".tar.gz"))
         (tar-list (replace-regexp-in-string "\n" " "
                                             (shell-command-to-string (format "ls %s/*.tar.gz" top-directory))))
         (dir-list (replace-regexp-in-string "\n" ""
                                             (shell-command-to-string (format "ls -d %s/*" top-directory)))))
    (async-start (lambda ()
                   (require 'subr-x)
                   (when (not (string= tar-list archive-name)) ;; @BUG(renzix): possible bug cause async
                     (shell-command (format "tar -c -z -f %s %s" archive-name dir-list))
                     (shell-command (format "tar --concatenate --file=%s %s" archive-name tar-list))
                     (shell-command (format "rm -r %s" dir-list))
                     (shell-command (format "rm %s" tar-list))))
                 (lambda (results)
                   (message "Completed archive of backups")))))


(defun projectile-run-vterm ()
  "Run vterm in projectile directory."
  (interactive)
  (+vterm/open t))


;; Variables/Hooks
(add-hook 'after-save-hook (lambda () (bookmark-set (buffer-name) nil)))
(add-hook 'after-save-hook (lambda () (bookmark-set "LastSave" nil)))
(add-hook 'after-save-hook #'backup-file)
(add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
(setq display-line-numbers-type 'relative
      display-line-numbers-current-absolute t
      display-line-numbers-width 4
      display-line-numbers-widen t
      make-backup-files nil
      auto-save-file-name-transforms
      `((".*" "~/.cache/emacs/saves/" t))
      initial-buffer-choice 'eshell)


;; Keybindings
(map!
 :n       ";"     #'counsel-M-x
 :n       ","     (general-simulate-key "SPC m")
 :n       ">"     #'evil-snipe-repeat
 :n       "<"     #'evil-snipe-repeat-reverse
 :n       "`"     #'magit-status;; @TODO(renzix): Make this open in a new tab???
 :n       "\\"    (general-simulate-key "SPC p")
 :n       "|"     #'+eshell/open
 :n       "g ="   #'indent-buffer
 :nvimor  "M-h"   #'evil-window-left
 :nvimor  "M-j"   #'evil-window-down
 :nvimor  "M-k"   #'evil-window-up
 :nvimor  "M-l"   #'evil-window-right
 :nvimor  "M-s"   #'evil-window-split
 :nvimor  "M-v"   #'evil-window-vsplit
 :nvimor  "M-d"   #'evil-delete-buffer
 :nvimor  "M-c"   #'evil-window-delete
 :nvimor  "M-t"   #'+workspace:new
 :nvimor  "M-T"   #'+workspace:delete
 :nvimor  "M-]"   #'+workspace:switch-next
 :nvimor  "M-["   #'+workspace:switch-previous)
;; Leader stuff
(map! :leader
      (:prefix ("g" . "git")
        :desc "Magit Status"                 "`" #'magit-status)
      (:prefix ("p" . "project")
        (:prefix ("x" . "terminal")
          :desc "Open vterm in project"      "v" #'projectile-run-vterm)))

;; Ex commands
(evil-ex-define-cmd "conf[ig]"    'doom/open-private-config)
(evil-ex-define-cmd "bl"          '+ivy/switch-buffer)
(evil-ex-define-cmd "buffer-list" '+ivy/switch-buffer)
(evil-ex-define-cmd "nt" '+workspace:new)
(evil-ex-define-cmd "ct" '+workspace:delete)
(evil-ex-define-cmd "n[ext]" '+workspace:switch-next) ;; @TODO(renzix):Maybe get rid of this later???
(evil-ex-define-cmd "p[revious]" '+workspace:switch-previous) ;; same as above???
(evil-ex-define-cmd "rename"      'rename-current-buffer-and-file)
(evil-ex-define-cmd "rn"          'rename-current-buffer-and-file)
(evil-ex-define-cmd "rel[oad]"    'doom/reload)
(evil-ex-define-cmd "ref[resh]"   'doom//refresh)
(evil-ex-define-cmd "es[hell]"    '+eshell/open-popup)
(evil-ex-define-cmd "vt[erminal]" '+vterm/open-popup)

(after! eshell
  (set-eshell-alias!
   "em" "find-file $1"))

(after! vterm
  (setq vterm-shell "ion"))

(after! evil-snipe
  (setq evil-snipe-spillover-scope 'buffer))
