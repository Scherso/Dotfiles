#!/bin/sh

#
# ~/.xinitrc
#
# Executed by startx or sx sh $HOME/.xinitrc preferably  
#

xrandr -s 0

exec xmonad > $HOME/.xmonad/log.txt
