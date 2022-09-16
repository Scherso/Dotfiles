#!/bin/bash

MUTE=$(pactl get-sink-mute @DEFAULT_SINK@ | awk '{print $2}')
VOLUME=$(pactl get-sink-volume @DEFAULT_SINK@ | awk '{print $5}' | sed 's/%//')

if [ "$MUTE" = "yes" ] ; then 
    echo "<fc=#6B7089,#4a5260:0> </fc>"
elif [ "$MUTE" = "no" ] && [ "$VOLUME" -eq 0 ] ; then
    echo "<fc=#6B7089,#4a5260:0> $VOLUME% </fc>"
elif [ "$MUTE" = "no" ] && [ "$VOLUME" -gt 0 ] ; then
    echo " $VOLUME%"
fi
