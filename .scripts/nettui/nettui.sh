#!/usr/bin/env bash
# nmcli Management Script
# description: A TUI for managing nmcli in a window manager without a systray
# author: Dean Smith -Tue Mar 11 01:52:09AM 2025

selTasks=$(gum choose "network connect" "network disconnect" "network status")
    if [ "$selTasks" == "network connect" ]; then
    gum confirm 'Are you sure you want to connect?' &&
        nmcli -p -w 5 dev up wlan0
    fi
    if [ "$selTasks" == "network disconnect" ]; then
    gum confirm 'Are you sure you want to disconnect?' &&
        nmcli -p -w 5 dev down wlan0
    fi
    if [ "$selTasks" == "network status" ]; then
        nmcli -p dev status
    fi
