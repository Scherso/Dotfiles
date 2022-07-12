#!/bin/sh

#
# ~/.xinitrc
#
# Executed by startx or sx sh $HOME/.xinitrc preferably  
#

exec xmonad > $HOME/.config/xmonad/log.txt
