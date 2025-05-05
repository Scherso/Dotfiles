# /etc/skel/.bash_profile

# This file is sourced by bash for login shells.  The following line
# runs your .bashrc and is recommended by the bash info pages.
if [[ -f "${HOME}/.bashrc" ]] ; then
	. "${HOME}/.bashrc" 
fi

if [[ -f "${HOME}/.env" ]] ; then
	. "${HOME}/.env"
fi

if [[ -f "${HOME}/.cargo/env" ]] ; then
	. "${HOME}/.cargo/env"
fi

if test -z "${XDG_RUNTIME_DIR}" ; then
	export XDG_RUNTIME_DIR=$(mktemp -d /tmp/$(id -u)-runtime-dir.XXX)
fi
