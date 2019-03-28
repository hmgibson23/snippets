#!/bin/sh

mkdir -p "/tmp/bar"
echo -n "0" > /tmp/bar/mail

xset +fp $HOME/.local/share/fonts
xset fp rehash

setxkbmap gb -variant colemak
xmodmap $HOME/.Xmodmap
xautolock -disable

stalonetray &
compton &

dunst &

feh --bg-fill $HOME/.xmonad/background.jpg
# $HOME/.config/polybar/launch.sh
$HOME/.xmonad/lemonbar &
cat /tmp/.xmonad-title-log
