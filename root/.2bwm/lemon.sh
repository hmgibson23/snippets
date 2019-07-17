#!/bin/bash

FG='#aaaaaa'
BG='#e22626'
FONT='Iosevka Nerd Font:size 10'
conky | dzen2 -e - -h '26' -w '600' -x '0' -y '1060' -ta l -fg $FG -bg $BG -fn "$FONT"
