#!/bin/sh

xset +fp /home/hugo/.local/share/fonts
xset fp rehash
setxkbmap gb -variant colemak

xmodmap $HOME/.Xmodmap
xautolock -disable
compton &
dunst &
$HOME/.fehbg &
$HOME/git/tools/bin/dzconky
