---
name: planit-update
description: Address notes in plan.md and update the document accordingly
user-invocable: true
---

# Update Plan

Read a plan file and review any notes or feedback embedded in it.

## Arguments

The skill accepts an optional argument specifying the plan file to update:
- If an argument is provided (e.g., `docs/new-feature.md`), update that file
- If no argument is provided, default to `plan.md` in the project root

## Update Process

1. **Read Current Plan**: Load the specified plan file (or plan.md if not specified)
2. **Identify Notes**: Look for "NOTE:" or inline feedback
3. **Refine Plan**: Update based on feedback:
   - Fix errors or incorrect assumptions
   - Add missing critical details
   - Clarify ambiguous sections
   - Remove redundancy and verbosity

4. **Mark Resolution**: Remove inline notes as you address them
5. **Keep It Concise**:
   - Remove unnecessary explanations
   - Convert paragraphs to bullet points where possible
   - Trim verbose sections
   - Focus on actionable items

Update the file in place and ensure the plan is ready for implementation.

**Important:** Maintain brevity. Plans should be concise and scannable, not exhaustive documentation.

Do not implement yet - only update the plan document.
