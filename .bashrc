# /etc/skel/.bashrc
#
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !


# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi

# Put your fun stuff here.

# Nice functions
# DEBUG=t
info() {
	[ -z "$DEBUG" ] ||
		printf "[%s]:$1\n" "$(date +'%D %T')";
}

[[ $(type -P "exa") ]] && {
	info "Exa found"
	alias ls="exa"
	alias la="exa -a"
	alias ll='exa -lhF --color=always --group-directories-first --time-style=full-iso'
} || {
	info "Exa not found"
	alias l="ls --color=auto -h"
	alias la="ls -ah"
	alias ll='ls -lh'
}

# Variables and aliases
# Default editor
[[ $(type -P "nvim") ]] && {
	info "Found nvim so using that"
	export EDITOR="nvim"
} || [[ $(type -P "vim") ]] && {
	info "Found vim so using that"
	export EDITOR="vim"
} || [[ $(type -P "emacs") ]] && {
	info "Couldnt find any vi so using emacs"
	export EDITOR="emacs -nw"
} || [[ $(type -P "nano") ]] && {
	info "Couldnt find any vi or emacs so using nano"
	export EDITOR="nano"
}
alias vi="${EDITOR:-vi}"

# System Shutdown stuff
alias rb="sudo reboot"
alias sd="sudo shutdown -h now"

# git
alias gc="git commit"
alias gp="git push"

#Other
alias ht="htop"
alias kill-skyrim="env WINEPREFIX=~/.local/share/Steam/steamapps/compatdata/72850/pfx wineserver -k" # because im lazy only works on 1 cpu
alias kill-sse="env WINEPREFIX=~/.local/share/Steam/steamapps/compatdata/489830/pfx wineserver -k" # because im lazy only works on 1 cpu
alias vortex="WINEPREFIX=\"$HOME/Games/vortex\" /home/genzix/.local/share/lutris/runners/wine/tkg-4.0-x86_64/bin/wine /home/genzix/Games/vortex/drive_c/Program\ Files/Black\ Tree\ Gaming\ Ltd/Vortex/Vortex.exe"


cb() {
	read -rt .1 input
	echo -e "\033]52;c;$(base64 <<< $input )\a"
}

__prompt_command() {
	if [[ $? != 0 ]]; then
		PS1="\e[0;31m\$\e[m "
	else
		PS1="\e[0;32m\$\e[m "
	fi
	export PS1
}

PROMPT_COMMAND=__prompt_command

export NIX_AUTO_RUN=1
