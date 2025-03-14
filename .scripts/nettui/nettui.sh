#!/usr/bin/env bash
# nmcli Management Script
# description: A TUI for managing nmcli in a window manager without a systray
# author: Dean Smith -Tue Mar 11 01:52:09AM 2025

selTasks=$(gum choose "network connect" "network disconnect" "network status")
    if [ "$selTasks" == "network connect" ]; then
    gum confirm 'Are you sure you want to connect?' &&
        sleep 5
        nmcli dev up wlan0
    fi
    if [ "$selTasks" == "network disconnect" ]; then
    gum confirm 'Are you sure you want to disconnect?' &&
        sleep 5
        nmcli dev down wlan0
    fi
    if [ "$selTasks" == "network status" ]; then
        nmcli dev status
    fi
