/usr/bin/xsetroot -cursor_name left_ptr &
alttab -w 1 -d 1 -sc 1 -i 64x64 -font xft:Hack-14 &
xrdb -merge ~/.Xresources
ibus-daemon &
xrandr --output eDP-1 --dpi 166
dbus-launch $HOME/.local/bin/my-xmonad
