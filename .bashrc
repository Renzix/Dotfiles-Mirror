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
neofetch
function _update_ps1() {
    PS1=$(/home/genzix/.local/bin/powerline-shell $?)
}

if [[ $TERM != linux && ! $PROMPT_COMMAND =~ _update_ps1 ]]; then
    PROMPT_COMMAND="_update_ps1; $PROMPT_COMMAND"
fi

alias wine_league="wine /home/genzix/.wine/drive_c/Riot\ Games/League\ of\ Legends/LeagueClient.exe"
alias wine_battlenet="wine /home/genzix/.wine/drive_c/Program\ Files\ \(x86\)/Battle.net/Battle.net\ Launcher.exe"
alias wine_steam="wine /home/genzix/.wine/drive_c/Program\ Files\ \(x86\)/Steam/Steam.exe"
