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

[[ -d /usr/share/zsh/site-contrib/zsh-syntax-highlighting ]] && source /usr/share/zsh/site-contrib/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh # Gentoo
[[ -d /usr/share/zsh/plugins/zsh-syntax-highlighting ]] && source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh # Arch

autoload -U colors && colors
setopt PROMPT_SUBST
export PROMPT='%(?.$fg[cyan].$fg[yellow])%1~%#%{$reset_color%} '

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
