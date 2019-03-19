#!/bin/bash

mail=$(checkmail)

if [ ! -f /tmp/bar/mail ]; then
   exit 0
fi

if [ "$mail" -gt "0" ]; then
    echo "%{F#A54242}"
else
    echo ""
fi
