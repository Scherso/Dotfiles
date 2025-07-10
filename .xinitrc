#!/bin/sh

export MOZ_X11_EGL=1
export MOZ_ENABLE_WAYLAND=0
export XDG_SESSION_TYPE=x11

[[ -f "${HOME}/.Xresources" ]] && xrdb -merge "${HOME}/.Xresources"

xhost +SI:localuser:root

xrandr --output DisplayPort-0 --primary
xrandr --output DisplayPort-1 --left-of DisplayPort-0 
xrandr --output DisplayPort-1 --rotate left

exec dbus-launch --exit-with-session xmonad > "${HOME}/.config/xmonad/log.txt"
