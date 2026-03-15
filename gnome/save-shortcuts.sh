#!/bin/bash
set -e
cd "$(dirname "$0")"
dconf dump /org/gnome/settings-daemon/plugins/media-keys/ > shortcuts-media-keys.ini
dconf dump /org/gnome/desktop/wm/keybindings/ > shortcuts-wm.ini
echo "Shortcuts saved."
