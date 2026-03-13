# i3 Keybindings

`Mod` = **Super / Win key**

Key cells show:

```
┌──────┐
│  KEY │
│ Mod  │  ← Mod+key action  (blank = unbound)
│ +Sft │  ← Mod+Shift+key action
└──────┘
```

---

## Function Keys

```
┌──────┬──────┬──────┬──────┬──────┬──────┬──────┬──────┬──────┬──────┬──────┬──────┐
│  F1  │  F2  │  F3  │  F4  │  F5  │  F6  │  F7  │  F8  │  F9  │  F10 │  F11 │  F12 │
│  ws1 │  ws2 │  ws3 │  ws4 │  ws5 │  ws6 │  ws7 │  ws8 │  ws9 │ ws10 │ ksnip│reload│
│ ►ws1 │ ►ws2 │ ►ws3 │ ►ws4 │ ►ws5 │ ►ws6 │ ►ws7 │ ►ws8 │ ►ws9 │►ws10 │      │      │
└──────┴──────┴──────┴──────┴──────┴──────┴──────┴──────┴──────┴──────┴──────┴──────┘
```
`Shift+F1`–`F10` (►) = move focused container to that workspace

---

## Main Keys  (Mod + ...)

```
    ┌──────┬──────┬──────┬──────┬──────┬──────┬──────┬──────┬──────┬──────┐
    │  Q   │  W   │  E   │  R   │  T   │  Y   │  U   │  I   │  O   │  P   │
    │      │      │      │      │      │stack │fc~flt│fc par│ dmenu│      │
    │      │      │ exit │restrt│      │tabbed│ float│fc chd│ d-dsk│      │
    └──────┴──────┴──────┴──────┴──────┴──────┴──────┴──────┴──────┴──────┘
      ┌──────┬──────┬──────┬──────┬──────┬──────┬──────┬──────┬──────┬──────┬──────┐
      │  A   │  S   │  D   │  F   │  G   │  H   │  J   │  K   │  L   │ ;  : │ ' "  │
      │      │      │      │      │      │ fc ← │ fc ↓ │ fc ↑ │ fc → │fullsc│splt v│
      │      │      │      │      │      │ mv ← │ mv ↓ │ mv ↑ │ mv → │      │splt h│
      └──────┴──────┴──────┴──────┴──────┴──────┴──────┴──────┴──────┴──────┴──────┘
        ┌──────┬──────┬──────┬──────┬──────┬──────┬──────┬──────┬──────┬──────┐
        │  Z   │  X   │  C   │  V   │  B   │  N   │  M   │  ,   │  .   │  /   │
        │      │      │      │      │      │      │      │      │      │tglspl│
        │      │      │      │      │      │      │      │ ws ← │ ws → │resize│
        └──────┴──────┴──────┴──────┴──────┴──────┴──────┴──────┴──────┴──────┘
```

`fc` = focus · `mv` = move · `fc~flt` = toggle focus tiling/floating · `fc par/chd` = focus parent/child
`tglspl` = layout toggle split · `resize` = enter resize mode · `d-dsk` = i3-dmenu-desktop

---

## Special Keys

| Shortcut | Action |
|----------|--------|
| `Mod+Return` | Open terminal |
| `Mod+Shift+\|` | Kill focused window |
| `Mod+Shift+,` | Move workspace to left monitor |
| `Mod+Shift+.` | Move workspace to right monitor |

---

## Resize Mode  (`Mod+?` to enter · `Enter` / `Esc` / `Mod+R` to exit)

| Key | Action |
|-----|--------|
| `J` / `←` | Shrink width |
| `;` / `→` | Grow width |
| `K` / `↓` | Grow height |
| `L` / `↑` | Shrink height |

---

## Mouse

| Action | Result |
|--------|--------|
| `Mod` + drag | Move floating window |
| Middle click (release) | Kill window |
| Right click (release) | Resize mode |
| Scroll up (release) | Toggle floating |

---

## Audio

| Key | Action |
|-----|--------|
| `XF86AudioRaiseVolume` | Volume +10% |
| `XF86AudioLowerVolume` | Volume -10% |
| `XF86AudioMute` | Toggle output mute |
| `XF86AudioMicMute` | Toggle mic mute |
