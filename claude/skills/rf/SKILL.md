---
name: rf
description: Plan a new feature using the RF format (Requirements, Functionality, Architecture). Use when the user wants to plan a new feature, design a feature, or create a feature specification.
user-invocable: true
---

# RF Feature Planning Mode

You are now in RF Feature Planning mode. Your task is to help create a comprehensive plan for a new feature following the RF format.

## RF Planning Framework

Create a detailed feature plan document with the following sections:

### 1. Requirements
Capture and document:
- **Pain Points**: What problems do users currently face without this feature? What friction exists?
- **Current Workflows**: How do users currently accomplish related tasks? What are the workarounds?

### 2. Functionality
Describe HOW the feature works from a user perspective (like user documentation):
- **Feature Overview**: High-level description of what the feature does
- **User Flows**: Step-by-step user interactions and workflows
- **UI/UX Elements**: New screens, buttons, dialogs, forms, etc.
- **New Functions/Commands**: APIs, functions, or commands exposed to users
- **Configuration Options**: Settings, parameters, or preferences (PVs)
- **Edge Cases**: How the feature behaves in various scenarios

## Process

1. **Explore the codebase** (if applicable):
   - Understand existing architecture
   - Identify relevant files and components
   - Check for similar patterns or features

2. **Create the RF document**:
   - Save it as `docs/[feature-name].md`
   - Use clear headings and formatting
   - Add diagrams or ASCII art if beneficial

3. **Review with the user**:
   - Present the plan
   - Gather feedback
   - Iterate as needed

## Important Guidelines

- Be thorough but concise
- Focus on clarity and actionability
- Include concrete examples
- Consider edge cases and failure modes
- Think about backwards compatibility
- Document assumptions and open questions


