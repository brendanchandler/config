---
name: planit-impl
description: Implement all tasks from plan.md, marking completed items, without unnecessary comments
disable-model-invocation: true
context: fork
agent: general-purpose
user-invocable: true
---

# Implement Plan

Implement all tasks from a plan file.

## Arguments

The skill accepts an optional argument specifying the plan file to implement:
- If an argument is provided (e.g., `docs/authentication.md`), implement tasks from that file
- If no argument is provided, default to `plan.md` in the project root

## Implementation Process

1. **Read Plan**: Load the specified plan file (or plan.md if not specified) and understand all tasks
2. **Implement Each Task**:
   - Create/edit required files
   - Write clean, production-ready code
   - Follow project conventions
   - No verbose comments - code should be self-documenting
   - Only add comments for non-obvious logic or clarifying examples

3. **Mark Progress**: As you complete each task:
   - Update the plan file to mark tasks with [x] if using checklist format
   - Or add a COMPLETED timestamp to each task
   - Don't add commentary - just mark as done

4. **Test**: Run any tests to verify implementation works

## Code Quality

- Follow existing code style and patterns
- Write clean code without over-commenting
- Include only essential inline comments (why, not what)
- Assume the reader understands basic programming concepts

## Important

Do NOT commit changes to git - user will review and commit themselves.

Complete ALL tasks in the plan before stopping. Do not stop until everything is implemented.
