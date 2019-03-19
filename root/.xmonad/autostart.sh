#!/bin/sh

mkdir -p "/tmp/bar"
echo -n "0" > /tmp/bar/mail

init_daemons () {
# mail daemon
bash -c 'while true ; do
    mail=$(checkmail)
    echo -n "$mail" > /tmp/bar/mail
    sleep 45
    done' > /dev/null 2>&1 &
}

xset +fp $HOME/.local/share/fonts
xset fp rehash

setxkbmap gb -variant colemak
xmodmap $HOME/.Xmodmap
xautolock -disable

compton &
killall nm-applet ; nm-applet &
dunst &

init_daemons

$HOME/.config/polybar/launch.sh
cat /tmp/.xmonad-title-log
