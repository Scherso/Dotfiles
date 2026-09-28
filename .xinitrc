#!/bin/sh

if [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
    eval "$(dbus-launch --sh-syntax --exit-with-session)"
fi

eval "$(gnome-keyring-daemon --start --components=secrets)"
export SSH_AUTH_SOCK

export MOZ_X11_EGL=1
export MOZ_ENABLE_WAYLAND=0
export XDG_SESSION_TYPE=x11

[ -f "$HOME/.Xresources" ] && xrdb -merge "$HOME/.Xresources"

# Hacky solution to make cursor theme apply to chromium based applications.
XCURSOR_THEME=$(xrdb -query | awk -F':[[:space:]]*' '/^Xcursor\.theme:/ { print $2 }')
XCURSOR_SIZE=$(xrdb -query | awk -F':[[:space:]]*' '/^Xcursor\.size:/ { print $2 }')
export XCURSOR_THEME XCURSOR_SIZE

xrandr --output DisplayPort-0 --primary
xrandr --output DisplayPort-1 --left-of DisplayPort-0
xrandr --output DisplayPort-1 --rotate right

xset s 600 10
if command -v xss-lock >/dev/null 2>&1; then
    xss-lock --transfer-sleep-lock -- betterlockscreen -l blur -- --nofork &
else
    xautolock -time 10 -locker "betterlockscreen -l blur" -detectsleep &
fi

exec xmonad > "$HOME/.config/xmonad/log.txt"