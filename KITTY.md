# Kitty Setup

Add to `~/.config/kitty/kitty.conf`:

```
include ~/path/to/config/kitty/kitty.conf
```

Or symlink the file directly:

```sh
mkdir -p ~/.config/kitty
ln -s ~/path/to/config/kitty/kitty.conf ~/.config/kitty/kitty.conf
```

Reload inside a running session: `ctrl+shift+f5`
