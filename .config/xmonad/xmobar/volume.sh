#!/bin/bash

MUTE=$(pulseaudio-ctl full-status | awk '{print $2}')
VOLUME=$(pulseaudio-ctl full-status | awk '{print $1}')

if [ "$MUTE" = "yes" ]; then
    echo "<fc=#696B71> \ </fc>"
elif [ "$MUTE" = "no" ] && [ "$VOLUME" -eq 0 ]; then
    echo "<fc=#696B71> $VOLUME% </fc>"
else
    echo " $VOLUME%"
fi
