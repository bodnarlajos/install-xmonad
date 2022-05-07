#!/bin/sh
ISRUNPANEL=$(ps ax|egrep -c '^.+[0-9]{2}\sbudgie-panel$')
ISRUNCOMPTON=$(ps ax|egrep -c '^.+[0-9]{2}\scompton$')
ISRUNALTTAB=$(ps ax|egrep -c '^.+[0-9]{2}\salttab$')

if [ $ISRUNPANEL -gt 0 ];then
		killall budgie-panel
fi
if [ $ISRUNCOMPTON -gt 0 ];then
 		killall compton
fi
if [ $ISRUNALTTAB -gt 0 ];then
 		killall alttab
fi
xrandr --output eDP-1 --dpi 96

sleep 1

budgie-daemon --replace &
alttab -w 1 -d 1 -sc 1 -i 64x64 -theme Yaru -font xft:Hack-14 &
compton &
budgie-panel &

feh --bg-fill ~/Pictures/wall.jpg &
