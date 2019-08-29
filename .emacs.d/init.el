(if (<= emacs-major-version 26)
    (package-initialize)
  (package-refresh-contents))
(setq enable-local-eval nil
      enable-local-variables nil)
(org-babel-load-file "~/Dotfiles/.emacs.d/config.org")
