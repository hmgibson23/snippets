#!/usr/bin/env sh

xmodmap $HOME/.Xmodmap
xautolock -disable

compton &
dunst &

feh --bg-fill $HOME/wallpapers/sky_of_neptune_by_justv23_dd3pbcc-fullview.jpg
