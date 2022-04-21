#!/bin/sh

sudo apt install -y xsecurelock feh budgie-core suckless-tools xterm

InstallDir=~/Projects/install
BuildDir=~/Projects/xmonad

cd ~/Projects/xmonad
stack build
stack install
cp $InstallDir/xinitrc ~/.xinitrc
cp $InstallDir/Xresources ~/.Xresources
cp -f $InstallDir/xmonad-startup.sh ~/.local/bin/xmonad-startup.sh
chmod 755 ~/.local/bin/xmonad-startup.sh
cp $InstallDir/30-touchpad.conf /etc/X11/xorg.conf.d/
cp $InstallDir/xsecurelock@.service /etc/systemd/system/
systemctl enable xsecurelock@lbodnar.service 
systemctl start xsecurelock@lbodnar.service 
