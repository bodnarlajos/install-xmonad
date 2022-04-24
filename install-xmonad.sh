#!/bin/sh

sudo apt install -y xsecurelock feh budgie-core suckless-tools xterm

InstallDir=.
BuildDir=./bin

# copy xmonad
mkdir -p ~/.local/bin
cp $BuildDir/xmonad ~/.local/bin
cp $BuildDir/my-xmonad ~/.local/bin

# copy init for xorg
cp $InstallDir/xinitrc ~/.xinitrc
cp $InstallDir/Xresources ~/.Xresources

# copy init for xmonad
cp -f $InstallDir/xmonad-startup.sh ~/.local/bin/xmonad-startup.sh
chmod 755 ~/.local/bin/xmonad-startup.sh

# copy setup for xorg
cp $InstallDir/30-touchpad.conf /etc/X11/xorg.conf.d/

# copy app menu script
# just link the frequently used apps to the ~/.local/bin/app-menu folder
mkdir -p ~/.local/bin/app-menu
cp $InstallDir/my-app-menu.sh /.local/bin/my-app-menu.sh

# copy and setup the xsecure lock screen
cp $InstallDir/xsecurelock@.service /etc/systemd/system/
systemctl enable xsecurelock@lbodnar.service 
systemctl start xsecurelock@lbodnar.service 
