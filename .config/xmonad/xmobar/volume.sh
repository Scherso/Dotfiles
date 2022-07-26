#!/bin/bash

INPUTMUTE=$(pulseaudio-ctl full-status | awk '{print $3}')
MUTE=$(pulseaudio-ctl full-status | awk '{print $2}')
VOLUME=$(pulseaudio-ctl full-status | awk '{print $1}')

if [ "$MUTE" = "yes" ] && [ "$INPUTMUTE" = "yes" ]; then
    echo "<fc=#6B7089,#4a5260:0> </fc><fc=#6B7089,#4a5260:0></fc> "
elif [ "$MUTE" = "yes" ] && [ "$INPUTMUTE" = "no" ]; then
    echo "<fc=#6B7089,#4a5260:0> </fc> "
elif [ "$MUTE" = "no" ] && [ "$VOLUME" -eq 0 ] && [ "$INPUTMUTE" = "yes" ]; then
    echo "<fc=#6B7089,#4a5260:0> $VOLUME% </fc><fc=#6B7089,#4a5260:0></fc> "
elif [ "$MUTE" = "no" ] && [ "$VOLUME" -eq 0 ] && [ "$INPUTMUTE" = "no" ]; then
    echo "<fc=#6B7089,#4a5260:0> $VOLUME% </fc> "
elif [ "$MUTE" = "no" ] && [ "$VOLUME" -gt 0 ] && [ "$INPUTMUTE" = "no" ]; then
    echo " $VOLUME% "
elif [ "$MUTE" = "no" ] && [ "$VOLUME" -gt 0 ] && [ "$INPUTMUTE" = "yes" ]; then
    echo " $VOLUME% <fc=#6B7089,#4a5260:0></fc>"
fi
