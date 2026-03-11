---
name: planit
description: Create concise plan.md for new feature implementation details.
argument-hint: "RFA name"
disable-model-invocation: true
---

# Planning
There is a new feature describe in docs/$ARGUMENT.md.  Write a **concise** plan.md document outlining how to implement this.

## Guidelines

**BE CONCISE:**
- Focus on WHAT needs to change, not extensive WHY explanations
- Show code diffs/snippets for all significant changes, in unified diff format
- Avoid verbose descriptions - assume the reader is technical
- Skip obvious details and background information
- No need for exhaustive alternative approaches sections
- Testing section should be brief - just the key test scenarios

**Structure:**
1. **Problem** Link to docs/$ARGUMENT.md
2. **Solution** (bullet points of changes)
3. **Files to Modify** (list with brief notes)
4. **Implementation Steps** (concise checklist with code snippets)
5. **Code Changes** (diffs/snippets for each file modification)
6. **Testing** (3-5 key test cases)

## Code Changes Format

For each file modification, include:
- **What's changing**: Brief description
- **Code snippet or diff**: Show the actual code to add/modify
- Use diff format (+ for additions, - for deletions) or show complete snippets
- Include function signatures, key logic, and changed lines with context

Example:
```cpp
// In UDPConnection (udp_transport.h)
struct UDPConnection {
    // ... existing members ...
+   uint64_t packets_flushed;  // Add new counter

+   // Add new method
+   tl::expected<void, AxeError> flush_and_resync(
+       uint32_t expected_datagram,
+       int max_attempts
+   );
};
```

## Output

Save the plan to `plan.md` in the project root.

Keep it short and actionable. Include ALL necessary code changes so implementation can proceed directly from the plan.
