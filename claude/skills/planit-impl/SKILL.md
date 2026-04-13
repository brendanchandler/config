---
name: planit-impl
description: Implement all tasks from plan.md, marking completed items, without unnecessary comments
disable-model-invocation: true
context: fork
agent: general-purpose
user-invocable: true
---

# Implement Plan

Implement all tasks from the plan.md file.

## Implementation Process
1. Load the plan.md and understand all tasks
2. If not already created, add a TODO list to plan.md detailing every
   step needed to implement the plan.
2. Implement each task:
   - Create/edit required files
   - Write clean, production-ready code
   - Follow project conventions
   - No verbose comments - code should be self-documenting
   - Only add comments for non-obvious logic or clarifying examples
3. Mark progress as you complete each task:
   - Update the plan file to mark tasks with [x] if using checklist format
   - Or add a COMPLETED timestamp to each task
4. Run any tests to verify implementation works

## Code Quality

- Follow existing code style and patterns
- Write clean code without over-commenting
- Include only essential inline comments (why, not what)
- Assume the reader understands basic programming concepts

## Important

Do NOT commit changes to git - user will review and commit themselves.

Complete ALL tasks in the plan before stopping. Do not stop until everything is implemented.

