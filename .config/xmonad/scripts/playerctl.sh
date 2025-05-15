#!/usr/bin/env bash

FORMATBG="#31353F:5"
BACKGROUND="#282C34"
RED="#E06C75"
GREEN="#98C379"
YELLOW="#E5C07B"
BLUE="#61AFEF"
WHITE="#ABB2BF"
WHITE2="#777C85"
GREY="#6B7089"

PLAY="playerctl --player=spotify play"
PAUSE="playerctl --player=spotify pause"
STOP="playerctl stop"
SONG_1="playerctl --player=spotify -a pause && playerctl --player=spotify play"

PREV="<action=playerctl previous><fc=${WHITE2},${FORMATBG}><fn=2>玲</fn></fc></action>"
NEXT="<action=playerctl next><fc=${WHITE2},${FORMATBG}><fn=2>怜</fn></fc></action>"

printf "\n"
	
song() {
	playerctl --player=spotify metadata --format '{{ trunc(title, 40) }}'
}

artist() {
	playerctl --player=spotify metadata --format '{{ artist }}'
}

icon() {
	# _status defined in while loop `while read -r _status ; do`
	if [[ "${_status}" = "Playing"* ]] ; then
		printf '<action=`%s` button=3><action=`%s` button=2><action=%s><fc=%s,%s><fn=2>  </fn></fc></action></action></action>' \
			"${STOP}" "${SONG_1}" "${PAUSE}" "${GREEN}" "${FORMATBG}"
	else
		printf '<action=`%s` button=3><action=`%s` button=2><action=%s><fc=%s,%s><fn=2>  </fn></fc></action></action></action>' \
			"${STOP}" "${SONG_1}" "${PLAY}" "${RED}" "${FORMATBG}"
	fi
}

main() {	
	playerctl --player=spotify metadata -f '{{ status }} {{ title }} {{ artist }}' -F |
		while read -r _status ; do
			SONG="<fc=${WHITE},${FORMATBG}>$(song)</fc>"
			ARTIST="<fc=${WHITE},${FORMATBG}>$(artist)</fc>"
			if [[ -z "${_status}" ]] ; then
				printf "\n"
			else
				printf "<fc=${FORMATBG},${BACKGROUND}:7><fn=2></fn></fc>%s%s%s %s <fc=${GREY},${FORMATBG}>-</fc> %s<fc=${FORMATBG},${BACKGROUND}><fn=2> </fn></fc>\n" \
					"${PREV}" "$(icon)" "${NEXT}" "${SONG}" "${ARTIST}"
			fi
        done
}

main "${@}"


