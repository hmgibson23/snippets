#!/bin/sh

setxkbmap gb -variant colemak
xmodmap $HOME/.Xmodmap
compton &
killall nm-applet ; nm-applet &
fluxgui &
dunst &
nitrogen --restore &

$HOME/.dwm/status.sh &
