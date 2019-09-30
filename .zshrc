#!/usr/bin/env zsh
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd extendedglob nomatch
unsetopt appendhistory beep notify
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/archzix/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Syntax highlighting
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh


autoload -U colors && colors
setopt PROMPT_SUBST
export PROMPT='%(?.$fg[cyan].$fg[yellow])%1d%#%{$reset_color%} '
export RPROMPT="%F{yellow}%?%f"

alias sudo="sudo "
alias e="nvim"
alias f="emacs -nw ."

alias ls="exa"
alias ll="exa -al"
alias ht="htop"
