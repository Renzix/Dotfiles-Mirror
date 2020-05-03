(package-refresh-contents)
(org-babel-load-file "~/Dotfiles/.config/emacs/config.org")

(if (<= emacs-major-version 26)
    (package-initialize)
  (package-refresh-contents))
(setq enable-local-eval nil
      enable-local-variables nil)
(let ((elc-loc "~/Dotfiles/.config/emacs/config.elc")
      (org-loc "~/Dotfiles/.config/emacs/config.org"))
  (when (not (file-exists-p elc-loc))
    (org-babel-load-file org-loc t))
  (let* ((org-time (time-convert (file-attribute-modification-time (file-attributes org-loc)) 'integer))
         (elc-time (time-convert (file-attribute-modification-time (file-attributes elc-loc)) 'integer)))
    (if (> org-time (+ 20 elc-time))
        (org-babel-load-file org-loc t)
      (load-file elc-loc))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/Documents/agenda/home.org" "~/Documents/agenda/work.org"))
 '(package-selected-packages
   '(wand dired-filter all-the-icons-dired dired-hacks-utils paredit yasnippet-snippets which-key vterm visible-mark try rustic rainbow-delimiters projectile-ripgrep powershell perl6-mode nix-mode magit-todos lua-mode lsp-ui lsp-python-ms lsp-java git-timemachine git-gutter flycheck-perl6 flx expand-region erc-image erc-hl-nicks emms easy-kill doom-themes doom-modeline deadgrep dap-mode crux counsel-projectile company-lsp comment-dwim-2 command-log-mode circe browse-kill-ring auctex anzu amx 0x0)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
