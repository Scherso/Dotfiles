#!/bin/sh

#
# ~/.xinitrc
#
# Executed by startx or sx sh $HOME/.xinitrc preferably  
#

if command -v dbus-launch >/dev/null && test -z "${DBUS_SESSION_BUS_ADDRESS}"; then
	val $(dbus-launch --sh-syntax --exit-with-session)
fi

gentoo-pipewire-launcher & 

dbus-launch --exit-with-session xmonad > $HOME/.config/xmonad/log.txt
