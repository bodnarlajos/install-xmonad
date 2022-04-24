#!/bin/sh

RESULT=$(ls ~/.local/bin/app-menu|dmenu)
exec $RESULT
