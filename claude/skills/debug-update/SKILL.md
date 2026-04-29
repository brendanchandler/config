---
name: debug-update
description: Investigate the problem in debug.md, append findings, and address NOTE: entries.
user-invocable: true
---

## Instruction

1. Read `debug.md` from the current working directory.
2. Review existing investigations and any inline `NOTE:` entries.
3. Determine what to investigate based on open hypotheses, `NOTE:` entries, and $ARGUMENTS.
4. Run investigations using available tools (grep, read files, run commands, web search, etc.).
5. Append new `### [INV-N]` entries under **Ongoing Investigations** with:
   - **Hypothesis** — what was being tested.
   - **Method** — commands/queries/reads performed (include key output snippets).
   - **Result** — findings and their significance.
6. For any investigation whose hypothesis has been falsified or whose path is exhausted:
   - Condense it to a single bullet in a **Dead Ends** section: `- [INV-N] brief summary of what was tried and why it was ruled out, plus any facts that remain relevant`.
   - Remove the full `### [INV-N]` block.
7. Remove `NOTE:` entries after incorporating them.
8. If the root cause is now clear, write or update the **Conclusion** section:
   - **Root Cause** — one concise paragraph naming the specific cause.
   - **Recommendation** — bullet-point steps to fix or work around it; include code snippets in unified diff format for file changes, or plain blocks for commands.
   - Set Status to `resolved`.
9. Otherwise, set Status to `in-progress`.
10. Summarize findings and suggest next steps for the user.

Additional user feedback: $ARGUMENTS
