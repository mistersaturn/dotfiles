#!/bin/bash

# Get the current track information from cmus
track_info=$(cmus-remote -Q | grep 'tag title' | cut -d' ' -f3- | awk '{print tolower($0)}')
artist_info=$(cmus-remote -Q | grep 'tag artist' | cut -d' ' -f3- | awk '{print tolower($0)}')

# Format the output
if [ -n "$track_info" ] && [ -n "$artist_info" ]; then
    echo "   $artist_info - $track_info"
else
    echo " 󰝛 "
fi
