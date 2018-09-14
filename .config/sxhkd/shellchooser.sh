#!/bin/bash

echo "Please choose your shell
     1. bash
     2. zsh
     3. ipython
     4. pwsh
     5. eshell
"

read choice

case $choice in
    1)
        echo "starting bash"
        clear
        bash
        ;;
    2)
        echo "starting zsh"
        clear
        zsh
        ;;
    3)
        echo "starting ipython"
        clear
        ipython
        ;;
    4)
        echo "starting pwsh"
        clear
        $HOME/Computer/powershell/pwsh
        ;;
    5)
        echo "starting eshell"
        clear
	      nohup emacsclient -ca "" -e "(eshell)" & disown
	      sleep 1
        ;;
    *)
        echo "Entered a invalid please retry"
        script=$(readlink -f "$0")
        exec "$script"
        ;;
esac

