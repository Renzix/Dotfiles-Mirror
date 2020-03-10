#!/usr/bin/env zsh

# History
export HISTFILE="$HOME/.config/zsh/zsh-history"
export HISTSIZE=40000
export SAVEHIST=$HISTSIZE
setopt hist_ignore_all_dups

if [[ $(wc -l < $HISTFILE) -gt 20000 ]]; then
	try sed -i 1,10000d $HISTFILE || 
		{ 
			TMPFILE = `mktemp`
			tail -n +4 $HISTFILE > $TMPFILE
			mv $TMPFILE $HISTFILE
		}
fi


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
[[ -d /usr/local/share/zsh-syntax-highlighting ]] && source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh # MacOS

autoload -U colors && colors
setopt PROMPT_SUBST
PROMPT='%{%(?.$fg[green].$fg[red])%}%1~%  %{$reset_color%}% '

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
