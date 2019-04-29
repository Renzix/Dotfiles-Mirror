{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
    use-package
    helm
    magit
    evil
    nix-mode
    helm-nixos-options
    company
    company-nixos-options
    nix-sandbox
    darcula-theme
    quelpa
    auto-package-update
    evil-god-state
    evil-magit
    git-timemachine
    git-gutter
    treemacs
    rust-mode
    rustic
    cargo
    lsp-mode
    clippy
    racer
    general
    quelpa-use-package
    org-evil
    evil-collection
    helm-projectile
    forge
    company-jedi
    irony
    irony-eldoc
    flycheck-irony
    company-irony
    company-irony-c-headers
    ensime
    haskell-mode
    lsp-haskell
    flycheck-haskell
    autopair
    helm-flyspell
    ox-pandoc
    htmlize
    webpaste
    emms
    elcord
    circe
    helm-circe
    tracking
    perl6-mode
    slime
    lsp-java
    #lsp-scala ??? Why this no work???
  ]) ++ (with epkgs.elpaPackages; [
    minimap
    which-key
    undo-tree
  ]) ++ [
    pkgs.notmuch
    pkgs.pandoc
    pkgs.irony-server
    pkgs.clang
  ])
