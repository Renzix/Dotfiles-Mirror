#!/usr/bin/env bash

killall -q polybar
echo "---" | tee -a /tmp/polybar1.log
echo "---" | tee -a /tmp/polybar2-1.log
echo "---" | tee -a /tmp/polybar2-2.log
echo "---" | tee -a /tmp/polybar2-3.log
echo "---" | tee -a /tmp/polybar3.log
polybar bar1 2>&1 | tee -a /tmp/polybar1.log & disown
polybar bar3 2>&1 | tee -a /tmp/polybar3.log & disown
# probably could do this better but the resolutions arent the same
MONITOR=DVI-D-0 WIDTH=12% OFFSET=44% polybar bar2 2>&1 | tee -a /tmp/polybar2-1.log & disown
MONITOR=DP-2 WIDTH=12.5% OFFSET=43.75% polybar bar2 2>&1 | tee -a /tmp/polybar2-2.log & disown
MONITOR=DP-4 WIDTH=12% OFFSET=44% polybar bar2 2>&1 | tee -a /tmp/polybar2-3.log & disown
echo "Bar launched"
