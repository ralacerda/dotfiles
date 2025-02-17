#!/usr/bin/env bash

gh repo list skylar-ai --limit 100 | awk '{print $1}' | rofi -dmenu | xargs -I {} gh browse -R {}