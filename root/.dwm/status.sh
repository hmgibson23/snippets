#!/bin/bash

init_daemons () {
# mail daemon
dash -c 'while true ; do
    mail=$(checkmail)
    echo -n "$mail" > /tmp/bar/mail
    sleep 60
    done' > /dev/null 2>&1 &

# vol daemon
dash -c 'while true; do
    vol > /tmp/bar/vol
    sleep 2
done' &


    # time_daemon
    dash -c 'while date "+%a, %I:%M" > /tmp/bar/date
        do sleep 59
    done' &
}

get_mail() {
    if [ -f /tmp/bar/mail ] ; then
         mail="$(< /tmp/bar/mail)"
         if [ "$mail" -gt "0" ] ; then
             echo -e "\x04"
         else
             echo ""
         fi
    fi
}


clock() {
    echo $(date +"%I:%M")
}

vol() {
    NOTMUTED=$(amixer -M | head -5 | grep "\[on\]")
    vol="$(< /tmp/bar/vol)"
    if [ -z "$NOTMUTED" ];
    then
        echo -e ""
    else
        case ${vol%??} in
            10|[5-9]) echo -e "\x03\\uf028 $vol" ;;
            [1-4]) echo -e "\x03\\uf027 $vol" ;;
            *) echo -e "\x03\\uf026 $vol"
        esac
    fi
}

init_daemons
while true
do
    xsetroot -name "$(< /tmp/bar/date)                                     $(get_mail) | $(vol)"
    sleep 3
done
