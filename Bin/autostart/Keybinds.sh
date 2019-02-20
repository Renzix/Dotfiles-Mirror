#!/bin/bash

# Emulates a buncha keybindings that i may or maynot use in the future????

# Bash XD all it does is get location of source file if a symlink
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null && pwd )"

TIMEOUT=1000
xmodmap ${DIR}/.Xmodmap
xmodmap -e "keycode any = Escape" 
xmodmap -e "keycode any = Caps_Lock" 
xmodmap -e "keycode any = Tab" 

