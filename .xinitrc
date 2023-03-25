#!/bin/sh 
# 
# ~/.xinitrc
#
# Executed by startx preferably  
#

export MOZ_X11_EGL=1
export MOZ_ENABLE_WAYLAND=0
export XDG_SESSION_TYPE=x11

exec dbus-launch --exit-with-session xmonad > "${HOME}/.config/xmonad/log.txt""
