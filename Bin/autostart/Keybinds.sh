#!/bin/sh

set -e

DISPLAY=":0.0"
HOME=/home/genzix/
export DISPLAY HOME

main() {
    setxkbmap -option ctrl:nocaps
    xcape -e 'Control_L=Escape'
    xcape -e 'Shift_L=BackSpace'
}

main "$@"
