#!/bin/sh
MENU_PATH="/home/lbodnar/.local/bin/app-menu"
RESULT=$(ls $MENU_PATH|dmenu -fn "Monospace-14")
exec "$MENU_PATH/$RESULT" &
