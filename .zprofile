#!/usr/bin/env zsh

# Firefox
export MOZ_WEBRENDER=1
export MOZ_ENABLE_WAYLAND=0
export MOZ_X11_EGL=1

# XDG File Directories 
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-"${HOME}/.config"}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-"${HOME}/.local/share"}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-"${HOME}/.cache"}"
# Ensuring that XDG_RUNTIME_DIR is set, according to the Gentoo Wiki. 
unset XDG_RUNTIME_DIR
export XDG_RUNTIME_DIR="$(mktemp -d /tmp/$(id -u)-runtime-dir.XXX)"

# Java Profile Options (Fixes Swing Font Rendering)
export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel"
