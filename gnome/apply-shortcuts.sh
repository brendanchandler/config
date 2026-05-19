#!/bin/bash
set -e
cd "$(dirname "$0")"
dconf load /org/gnome/settings-daemon/plugins/media-keys/ < shortcuts-media-keys.ini
dconf load /org/gnome/desktop/wm/keybindings/ < shortcuts-wm.ini
dconf load /org/gnome/shell/keybindings/ < shortcuts-shell.ini
echo "Shortcuts applied."
