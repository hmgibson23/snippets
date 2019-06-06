#!/bin/bash

STD="#ffb5b2af"
BKG="#ff282522"
DBL="#ff4C626D"
LBL="#ff6A7A8F"
GRN="#ff838A60"
MAG="#ff7D6A79"
RED="#ff8C4C4C"
YLW="#ff907245"


clk(){
	date '+%a %d %b %H:%M'
}

work(){
	SPACE_NUM=$(xprop -root -notype _NET_CURRENT_DESKTOP | cut -d= -f2);
		case "$SPACE_NUM" in
			" 0")
				WORKSPACE='• ◦ ◦ ◦ ◦ ◦';;
			" 1")
				WORKSPACE='◦ • ◦ ◦ ◦ ◦';;
			" 2")
				WORKSPACE='◦ ◦ • ◦ ◦ ◦';;
			" 3")
				WORKSPACE='◦ ◦ ◦ • ◦ ◦';;
			" 4")
				WORKSPACE='◦ ◦ ◦ ◦ • ◦';;
			" 5")
				WORKSPACE='◦ ◦ ◦ ◦ ◦ •';;

		esac
	echo "$WORKSPACE"
}


wifi(){
        WID=$(ssid)
	echo "⇅ ${WID// /}"
}

battery(){
	BAT=$(bperc)
        echo "⭫$BAT"
}

vol(){
	VOL=$(vol)
	echo "⊲ $VOL"
}

mpd(){
	if [[ $(mpc status | awk 'NR==2 {print $1}') == "[playing]" ]]; then
		TTL=$(mpc current --format "%title%")
		echo "♫ $TTL"
	else
		echo "♫ Paused"
	fi
}


while :; do

	echo "%{l}%{B$YLW}  $(work)  %{l}%{r}%{B$RED}  $(vol)  %{B$MAG}  $(wifi)  %{B$LBL}  $(battery)  %{B$DBL}  $(mpd)  %{B$GRN}  ⭧ $(clk)  %{B$BKG}%{r}"

sleep 1s
done
