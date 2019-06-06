#!/bin/sh

mkdir -p "/tmp/bar"
echo -n "0" > /tmp/bar/mail

xmodmap $HOME/.Xmodmap
xautolock -disable

compton &
dunst &
feh --bg-fill $HOME/wallpapers/mist_by_aenami_d9sgs13-fullview.jpg
