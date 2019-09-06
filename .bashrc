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

if hash "exa" 2>/dev/null ; then
	info "Exa found"
	alias ls="exa"
	alias la="exa -a"
	alias ll='exa -lhF --color=always --group-directories-first --time-style=full-iso'
else
	info "Exa not found"
	alias l="ls --color=auto -h"
	alias la="ls -ah"
	alias ll='ls -lh'
fi

if hash "nvim" 2>/dev/null ; then
    info "Neovim found"
    alias v="neovim"
else
    if hash "vim" 2>/dev/null ; then
        info "Vim found"
        alias v="vim"
    else
        info "using vi"
        alias v="vi"
    fi
fi
    
# Some nice keybindings and aliases
# emacs keybindings
alias e="emacs -nw"
bind -m emacs -x '"\ee":"emacs -nw ."'
alias f="emacs -nw ."
bind -m emacs -x '"\C-xd":"emacs -nw ."'
alias v=""

# System Shutdown stuff
alias rb="sudo reboot"
alias sd="sudo shutdown -h now"

# git
alias gc="git commit"
alias gp="git push"

#Other
alias ht="htop"

## im lazy xd ##
alias kill-skyrim="env WINEPREFIX=~/.local/share/Steam/steamapps/compatdata/72850/pfx wineserver -k"
alias kill-sse="env WINEPREFIX=~/.local/share/Steam/steamapps/compatdata/489830/pfx wineserver -k"
alias vortex="WINEPREFIX=\"$HOME/Games/vortex\" /home/genzix/.local/share/lutris/runners/wine/tkg-4.0-x86_64/bin/wine /home/genzix/Games/vortex/drive_c/Program\ Files/Black\ Tree\ Gaming\ Ltd/Vortex/Vortex.exe"
alias kill-vortex="WINEPREFIX=\"$HOME/Games/vortex\" wineserver -k"


cb() {
	read -rt .1 input
	echo -e "\033]52;c;$(base64 <<< $input )\a"
}

pwdp() {
	PS1='\[\e[$([[ $? = 0 ]] && printf 32 || printf 31);1m\]\W\[\e[m\] '
}

tw() {
	mpv "https://twitch.tv/$1"
}

# Random prompt from the ones above
#rand_prompt() {
#	prompts=('pwdp' 'timep' 'usrp' 'verp' 'datep')
#	RANDOM=$$$(date +%s)
#	rand=$[$RANDOM % ${#prompts[@]}]
#	${prompts[$rand]}
#}

#PROMPT_COMMAND=rand_prompt
pwdp

export NIX_AUTO_RUN=1
