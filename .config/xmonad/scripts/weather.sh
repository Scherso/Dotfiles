#!/usr/bin/env bash

declare -r DNS="1.1.1.1"
declare -r BACKGROUND="#31353F:5"
declare -r GREY="#777C85"
declare -r WHITE="#ABB2BF"

is_night() {
	local HOUR=$(date +%H)

	[[ "${HOUR}" -ge 19 ]] && ICON=${*}
	[[ "${HOUR}" -le 4  ]] && ICON=${*}
}

refresh() {
	for _ in 1 2 3 4 5 ; do
		if ping -q -c 1 -W 1 "${DNS}" >/dev/null 2>&1 ; then
			WEATHER=$(curl -s 'wttr.in/?m&format=%x+%t\n') && break
		else
			sleep 2s
		fi
	done

	[[ -z "${WEATHER}" ]] && return

	CONDITION="${WEATHER% *}"
	TEMPERATURE="${WEATHER##* }"

	case "${CONDITION}" in
		"?") 
			ICON=""
			;;
                "mm") 
			ICON="" 
			;;
                "=")
			ICON=""
                        is_night ""
                        ;;
                "///") 
			ICON=""
			;;
                "//") 
			ICON=""
			;;
                "**") 
			ICON=""
			;;
                "*/*") 
			ICON=""
			;;
                "/")
                        ICON=""
                        is_night ""
                        ;;
                ".")
                        ICON=""
                        is_night ""
                        ;;
                "x")
                        ICON=""
                        is_night ""
                        ;;
                "x/")
                        ICON=""
                        is_night ""
                        ;;
                "*")
                        ICON=""
                        is_night ""
                        ;;
                "*/")
                        ICON=""
                        is_night ""
                        ;;
                "m")
                        ICON=""
                        is_night ""
                        ;;
                "o")
                        ICON=""
                        is_night ""
                        ;;
                "/!/") 
			ICON=""
			;;
                "!/") 
			ICON=""
			;;
                "*!*")
                        ICON=""
                        is_night ""
                        ;;
                "mmm") 
			ICON="" 
			;;
                *) 
			ICON="${CONDITION}" 
			;;
        esac
}

get_text_display() {
        while : ; do
                refresh
                if [[ -z "${WEATHER}" ]] ; then
                        printf 'Offline\n'
                        sleep 5m
                else
                        printf '<fc=%s,%s>%s </fc><fc=%s,%s> %s</fc>\n' \
                                "${GREY}" "${BACKGROUND}" "${ICON}" "${WHITE}" "${BACKGROUND}" "${TEMPERATURE}"
                        sleep 15m
                fi
        done
}

main() {
	case "${1}" in
		*) 
			while : ; do 
				get_text_display 
				sleep 5m
			done
			;;
		bar)
			refresh
			[[ -z "${WEATHER}" ]] &&
				printf "\033[01;31mERROR:\033[0m Check your network connection.\n" ||
				printf "%s  %s\n" "$ICON" "$TEMPERATURE"
			;;
	esac
}

main "${@}"
