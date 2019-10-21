#!/usr/bin/env bash


# Adds thing to clickboard if piped (terminal needs to support os52)
cb() {
    read -rt .1 input
    echo -e "\033]52;c;$(base64 <<< $input )\a"
}

pwdp() {
    export PS1='\[\e[$([[ $? = 0 ]] && printf 32 || printf 31);1m\]\W\[\e[m\] '
}
pwdp

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
