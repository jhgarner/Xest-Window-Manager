#!/usr/bin/bash
# xrdb -merge /home/jack/.Xresources
xrandr --output eDP-1 --primary --mode 2560x1440
# setxkbmap -layout 3l &
# compton &
dunst &
# /home/jack/.fehbg &
(sleep 1 && polybar example &> /home/jack/polybar.txt) &
# echo $DISPLAY > /home/jack/5.txt
# /home/jack/.local/bin/neXtWM-exe 1 &> /home/jack/.xtest.txt
#echo 6 > /home/jack/5.txt
