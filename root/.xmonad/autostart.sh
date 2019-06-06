#!/bin/sh

mkdir -p "/tmp/bar"
echo -n "0" > /tmp/bar/mail


stalonetray &

feh --bg-fill $HOME/.xmonad/background.jpg

$HOME/git/tools/bin/lemonbar &
