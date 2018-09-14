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

## Aliases
#LS
alias l="ls"
alias la="ls -a"
alias ll="ls -lh"

#Wine
alias wine_league="wine /home/genzix/.wine/drive_c/Riot\ Games/League\ of\ Legends/LeagueClient.exe"
alias wine_battlenet="wine /home/genzix/.wine/drive_c/Program\ Files\ \(x86\)/Battle.net/Battle.net\ Launcher.exe"
alias wine_steam="wine /home/genzix/.wine/drive_c/Program\ Files\ \(x86\)/Steam/Steam.exe"

#Text Editors
alias vi="nvim $@"
alias ff="emacsclient -nw -a \"\" $@"
alias sb="subl $@"
alias mc="micro $@"

#System Shutdown stuff
alias rb="sudo reboot"
alias sd="sudo shutdown -h now"

#git
alias gc="git commit -a -m $@"
alias gp="git push $@"

#Other
alias ht="htop $@"
alias dd="dd status=progress $@"

tw(){
    streamlink twitch.tv/$1 best
}

## Environment vars
export BSPC=".config/bspwm/bspwmrc"
export SXHKD=".config/sxhkd/sxhkdrc"

## Other Stuff
# For pywal :ree:
(cat ~/.cache/wal/sequences &)
# For tty pywal :ree:
source ~/.cache/wal/colors-tty.sh

neofetch
