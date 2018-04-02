#!/bin/bash

#Set newsink to idle
newSink=$(pactl list short sinks | grep IDLE | awk '{ print $1}')

#for each sink-input find stream ID and move each streamID to newSink
pactl list short sink-inputs|while read stream; do
    streamId=$(echo $stream|cut '-d ' -f1)
    echo "moving stream $streamId"
    pactl move-sink-input "$streamId" "$newSink"
done
