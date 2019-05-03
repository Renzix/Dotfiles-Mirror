{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  
  nixpkgs.config.allowUnfree = true;
  home.packages = with pkgs; [
    firefox
    xfce.xfce4-whiskermenu-plugin
    neofetch
    exa
    ripgrep
    discord
    libreoffice-fresh
    aspellDicts.en
    aspellDicts.en-computers
    (import ./emacs.nix { inherit pkgs; })
  ];

  home.sessionVariables = {
    EDITOR = "vim";
    VISUAL = "emacsclient -ca \"\"";
  };
  
  programs.git = {
    enable = true;
    userName = "Renzix";
    userEmail = "eragon9981@gmail.com";
  };

  programs.home-manager.enable = true;
}
