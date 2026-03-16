# Bash Config

## Setup

Add to `~/.bashrc`:

```bash
source ~/src/config/bash/bashrc
```

Add to `~/.bash_profile`:

```bash
source ~/src/config/bash/bash_profile
source ~/src/config/bash/bashrc
```

## What's included

- **bashrc** — env vars (EDITOR, PATH, library paths) safe for non-interactive shells
- **bash_profile** — interactive-only: history settings, aliases, prompt with timer/exit code, fzf, zoxide
