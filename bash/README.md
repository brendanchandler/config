# Bash Config

## Setup

Add to `~/.bashrc`:

```bash
source ~/src/config/bash/bashrc
```

Add to `~/.inputrc`:

```
$include ~/src/config/bash/inputrc
```

## What's included

- **bashrc** — env vars (EDITOR, PATH, library paths), history settings,
  aliases, prompt with timer/exit code, fzf, zoxide
- **completion** — loads system `bash-completion` in non-login shells, sources
  custom snippets
- **inputrc** — readline options (TAB cycles through matches); extras left
  commented with descriptions
- **completions/** — custom completion scripts, sourced at startup

## Adding a completion

- Repo-tracked: drop a script in `completions/`.
- Machine-local: drop it in `~/.local/share/bash-completion/completions.d/`.
- Lazy-loaded (cheapest): name the file after the command and put it in
  `~/.local/share/bash-completion/completions/`; it loads on first TAB.
