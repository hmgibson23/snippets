#!/bin/sh

mkdir -p "/tmp/bar"
echo -n "0" > /tmp/bar/mail

xmodmap $HOME/.Xmodmap
xautolock -disable

stalonetray &

compton &
dunst &

feh --bg-fill $HOME/.xmonad/background.jpg

$HOME/git/tools/bin/lemonbar &
