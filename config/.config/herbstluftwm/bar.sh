#!/usr/bin/env sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

# Launch
polybar workspace -c $HOME/.config/herbstluftwm/bar_conf &
polybar stats -c $HOME/.config/herbstluftwm/bar_conf

echo "Bar launched..."
