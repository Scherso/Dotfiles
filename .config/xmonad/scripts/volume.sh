#!/usr/bin/env bash 

BACKGROUND="#31353F:5"
GREEN="#98C379"
YELLOW="#E5C07B"
BLUE="#61AFEF"
WHITE="#ABB2BF"
GREY="#6B7089"
WHITE2="#777C85"

VOLUME_UP="wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"
VOLUME_DOWN="wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"

get_volume() {
	MUTE=$(pactl get-sink-mute @DEFAULT_SINK@ | awk '{print $2}')
	VOLUME=$(pactl get-sink-volume @DEFAULT_SINK@ | awk '{print $5}' | sed "s/%//")
	VOLUME=${VOLUME%%%}

	if [[ "${MUTE}" = "yes" ]] ; then
		printf "<fc=%s,%s><fn=2></fn></fc>\n" \
			"${GREY}" "${BACKGROUND}"
	elif [[ "${VOLUME}" -gt 0 ]] ; then
		printf "<fc=%s,%s><fn=2> </fn></fc><fc=%s,%s>%s%%</fc>\n" \
			"${WHITE2}" "${BACKGROUND}" "${WHITE}" "${BACKGROUND}" "${VOLUME}"
	elif [[ "${VOLUME}" -eq 0 ]] ; then
		printf "<fc=%s,%s><fn=2> </fn></fc><fc=%s,%s>%s%%</fc>\n" \
			"${GREY}" "${BACKGROUND}" "${GREY}" "${BACKGROUND}" "${VOLUME}"
	fi
}

main() {
	stdbuf -oL alsactl monitor default |
		while read -r _ ; do 
			get_volume
		done
}

main "${@}"
