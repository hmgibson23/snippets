#!/bin/bash

clock() {
    echo $(date +"%H:%M %p ")
}

volume() {
    NOTMUTED=$(amixer -M | head -5 | grep "\[on\]")
    AUDIO=$(amixer -M | head -5 | grep -o -m 1 -E "[[:digit:]]+%")

    if [ -z "$NOTMUTED" ];
    then
        echo " muted "
    else
        case "$AUDIO" in
            0%|[0-9]%) echo " $AUDIO " ;;
            1?%|2?%|3?%) echo " $AUDIO " ;;
            4?%|5?%|6?%) echo " $AUDIO " ;;
            *) echo " $AUDIO " ;;
        esac
    fi
}

while true
do
    xsetroot -name " $(clock)                                                                                                     $(volume)"
    sleep 1
done
