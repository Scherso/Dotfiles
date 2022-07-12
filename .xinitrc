#!/bin/sh

#
# ~/.xinitrc
#
# Executed by startx or sx sh $HOME/.xinitrc preferably  
#

dbus-launch xmonad > $HOME/.config/xmonad/log.txt
