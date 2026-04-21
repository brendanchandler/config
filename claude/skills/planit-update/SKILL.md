---
name: planit-update
description: Address notes in plan.md and update the document accordingly
user-invocable: true
---

## Instruction
1. Read the plan.md file and review any "NOTE:" entries embedded in it.
2. Refine Plan by updating plan.md based on user feedback and "NOTE:" sections.
   a. Fix any errors or incorrect assumptions.
   b. Add missing details.
   c. Clarify ambiguous sections
   d. Remove redundancy and text which is no longer relevant.
4. Remove inline NOTE: entries as you address them.
5. Keep it concise.
6. Show code snippets in unified diff format when needed.
7. The following is additional feedback provided by the user: $ARGUMENTS.

**Important:**
- Maintain brevity. Plans should be concise and scannable, not exhaustive documentation.
- Do not implement yet - only update the plan.md document.
