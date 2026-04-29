---
name: debug
description: Scaffold a debug.md investigation document from the user's problem description.
user-invocable: true
---

## Instruction

1. Read the user's problem description from $ARGUMENTS.
2. If `debug.md` already exists in the current working directory, print a warning and stop.
3. Create `debug.md` with:
   - **Problem** — the user's description, cleaned up for clarity.
   - **Environment** — stub fields (OS/platform, relevant versions, reproduction steps).
   - **Ongoing Investigations** — one empty `### [INV-1]` stub with Hypothesis/Method/Result fields.
   - **Dead Ends** — empty placeholder (investigations whose paths were exhausted go here as condensed bullets).
   - **Conclusion** — empty placeholder.
   - **Status: open**
4. Do NOT investigate yet. Only scaffold the document.
5. Tell the user to fill in Environment details then run `/debug-update` to begin.

Problem description: $ARGUMENTS
