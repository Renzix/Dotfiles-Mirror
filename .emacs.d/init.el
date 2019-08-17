(if (<= emacs-major-version 26)
    (package-initialize)
  (package-refresh-contents))
(org-babel-load-file "~/Dotfiles/.emacs.d/config.org")
