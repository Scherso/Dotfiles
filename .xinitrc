#!/bin/sh

#
# ~/.xinitrc
#
# Executed by startx or sx sh $HOME/.xinitrc preferably  
#

gentoo-pipewire-launcher & 

dbus-launch --sh-syntax --exit-with-session xmonad > "${HOME}/.config/xmonad/log.txt"
