#!/bin/sh

sudo apt install -y tint2 dunst dbus-x11 alacritty brightnessctl

BuildDir=./bin
LocalBin=~/.local/bin

# copy xmonad
mkdir -p $LocalBin
cp $BuildDir/xmonad $LocalBin
cp $BuildDir/my-xmonad $LocalBin
cp $BuildDir/my-app-menu.sh $LocalBin
cp $BuildDir/video_brightnessdown.sh $LocalBin
cp $BuildDir/video_brightnessup.sh $LocalBin

# copy init for xorg
cp $BuildDir/xinitrc ~/.xinitrc
cp $BuildDir/Xresources ~/.Xresources

# copy setup for xorg
sudo cp $InstallDir/30-touchpad.conf /etc/X11/xorg.conf.d/

# copy and setup the xsecure lock screen
cp $BuildDir/xsecurelock@.service /etc/systemd/system/
systemctl enable xsecurelock@lbodnar.service 
systemctl start xsecurelock@lbodnar.service 
