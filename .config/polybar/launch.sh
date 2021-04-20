#!/usr/bin/env bash

killall -q polybar
echo "---" | tee -a /tmp/polybar1.log
polybar bar1 2>&1 | tee -a /tmp/polybar1.log & disown
if type "xrandr"; then
  for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
    MONITOR=$m polybar bar2 2>&1 | tee -a /tmp/polybar2.log & disown
  done
else
  polybar bar2 2>&1 | tee -a /tmp/polybar1.log & disown
fi
echo "Bar launched"
