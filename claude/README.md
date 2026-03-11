# Claude Config

Custom skills for Claude Code.

## Setup

Link the skills directory into your `.claude/` directory:

```bash
ln -s /path/to/this/repo/skills ~/.claude/skills
```

## Skills

| Skill | Trigger | Description |
|-------|---------|-------------|
| `/research [dir]` | `/research src/mymodule` | Deep-dive a directory and write a `README.md` report |
| `/rf [name]` | `/rf auth-feature` | Plan a new feature (Requirements + Functionality); saves to `docs/<name>.md` |
| `/planit [name]` | `/planit auth-feature` | Generate a concise `plan.md` from `docs/<name>.md` |
| `/planit-update [file]` | `/planit-update` | Revise `plan.md` (or specified file) based on embedded `NOTE:` feedback |
| `/planit-impl [file]` | `/planit-impl` | Implement all tasks in `plan.md` (or specified file), marking items complete |

## Typical Workflow

```
/rf my-feature          # draft requirements & functionality doc
# review/edit docs/my-feature.md
/planit my-feature      # generate plan.md from that doc
# add NOTE: comments in plan.md if changes needed
/planit-update          # incorporate feedback into plan.md
/planit-impl            # implement the plan
```
