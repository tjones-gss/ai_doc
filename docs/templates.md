---
title: Templates
description: Starter files and reusable templates
sidebar_position: 7
last_updated: 2025-10-10
tags: [templates, examples, starter-files]
---

# Templates

This page provides starter templates and examples for common files and workflows in the AI-assisted development process.

## PROJECT_MEMORY.md Template

Use this template to initialize your project memory file.

```markdown
# Project Memory (Durable Context)

**Last Updated:** [Date]
**Project:** [Project Name]
**Maintainer:** [Your Name/Team]

---

## Problem Space

### What we're trying to solve

- [Describe the business problem, user need, or technical challenge]
- [Why this matters to the business]

### Business rules

- [Critical business rule #1]
- [Critical business rule #2]
- [Constraints or compliance requirements]

---

## Architecture / Data Flow

### Systems touched

- **[System Name]** - [Brief description and role]
- **[Database/Table]** - [Purpose and key fields]
- **[External Interface]** - [Integration points]

### Key files/modules

- **[FILENAME.cob]** - [Purpose and responsibilities]
- **[CP-XXXXX.cpy]** - [Copybook description]
- **[CONFIG.cfg]** - [Configuration purpose]

### Inputs/outputs

- **Input:** [Source, format, frequency]
- **Processing:** [Key transformations or logic]
- **Output:** [Destination, format, consumers]

### Data flow diagram
```

[Source] → [PROGRAM1] → [Database] → [PROGRAM2] → [Output]

```

---

## Known Patterns & Gotchas

### Reusable routines
- **[Pattern Name]:** [Description and when to use]
  - Example: `PERFORM ERROR-HANDLER` after file operations
- **[Another Pattern]:** [Description]

### Known pitfalls
- **[Pitfall #1]:** [What to watch out for and how to avoid]
- **[Pitfall #2]:** [Common mistake and solution]

### Performance considerations
- [Key performance constraint or optimization]

---

## Glossary

| Term | Definition |
|------|------------|
| [Term] | [Clear, concise definition] |
| [Acronym] | [Full name and meaning in context] |
| [Jargon] | [Plain language explanation] |

---

## Open Questions

- [ ] [Question that needs answering] → **Owner:** [Name] | **Due:** [Date]
- [ ] [Technical uncertainty or decision needed] → **Owner:** [Name]

---

## Decisions Made

### [Date]: [Decision Title]
**Decision:** [What was decided]
**Rationale:** [Why this decision was made]
**Alternatives Considered:** [What else was evaluated]
**Implications:** [Impact on architecture, code, or workflow]
**References:** [Link to CHANGE_HISTORY entry or external doc]

---

## References
- [Link to external documentation]
- [Link to architecture diagrams]
- [Link to business requirements]
```

## CHANGE_HISTORY.md Template

Use this template to initialize your change history file.

````markdown
# Change History (Append-Only)

**Project:** [Project Name]
**Purpose:** Chronological log of all proposed and approved changes

> **⚠️ IMPORTANT:** Never delete or modify existing entries. Always append new entries at the top.

---

## [YYYY-MM-DD HH:MM] ([Author Name] / [AI Tool])

### Intent

[Clear, one-sentence description of what you're trying to accomplish and why]

### Proposed Diff (before -> after)

```diff
File: [FILENAME.ext]

--- [Section/Function Name]
+++ [Section/Function Name]
- [Old line(s) of code]
+ [New line(s) of code]

File: [ANOTHER-FILE.ext]
[Additional changes...]
```
````

### Impact

- **Affects:** [List of files, systems, or users impacted]
- **Risk level:** [Low/Medium/High] - [Brief explanation]
- **Performance:** [Expected performance impact, if any]
- **Dependencies:** [Other systems or changes this depends on]

### Rollback Plan

```bash
# Command(s) to undo this change
svn revert [FILE]
# OR
svn update -r [REVISION] [FILE]
```

### Status

⏳ **PROPOSED** - Awaiting review and approval

<!-- Update status as change progresses:
✅ **APPROVED** - Implemented and tested on [date]
🔄 **ROLLED BACK** - Reverted on [date] because [reason]
-->

### Test Results

<!-- Add after testing -->

- [ ] Test case 1: [Description]
- [ ] Test case 2: [Description]
- [ ] Edge case: [Description]

### Notes

[Any additional context, learnings, or follow-up items]

---

<!-- Previous entries below... -->

````

## SPEC Template

Use this template to document feature specifications.

```markdown
# Spec: [Feature Name]

**Author:** [Your Name]
**Date:** [Date]
**Status:** [Draft / Review / Approved]

---

## Objective

[Clear, concise statement of what you're trying to accomplish and why it matters]

---

## Scope

### In Scope
- [What this feature will include]
- [Functionality being added or changed]
- [Systems or components affected]

### Out of Scope
- [What this feature explicitly will NOT include]
- [Related work deferred to future phases]

---

## Requirements

### Functional Requirements
1. [Specific functionality requirement]
2. [User-facing behavior or feature]
3. [Input/output requirement]

### Non-Functional Requirements
- **Performance:** [Response time, throughput, scalability needs]
- **Reliability:** [Uptime, error handling, data integrity]
- **Security:** [Authentication, authorization, data protection]
- **Compatibility:** [Browser, platform, or version requirements]

---

## Constraints

- [Technical limitation or restriction]
- [Resource constraint (time, budget, personnel)]
- [Integration or compatibility requirement]
- [Regulatory or compliance requirement]

---

## User Stories / Use Cases

### Use Case 1: [Title]
**Actor:** [Who is performing this action]
**Goal:** [What they want to accomplish]
**Steps:**
1. [Step 1]
2. [Step 2]
3. [Expected outcome]

**Edge Cases:**
- [Unusual scenario or error condition]

---

## Data Model / Schema

### New Tables/Files
````

TABLE: [NAME]
Fields:

- [FIELD-NAME] - [Type] - [Description]

```

### Modified Tables/Files
```

TABLE: [NAME]
Changes:

- ADD: [New field and purpose]
- MODIFY: [Changed field and reason]

```

---

## Architecture / Design

### High-Level Approach
[Brief description of how this will be implemented]

### Components Affected
- **[Component 1]:** [What changes and why]
- **[Component 2]:** [What changes and why]

### Data Flow
```

[Input] → [Processing Step 1] → [Processing Step 2] → [Output]

```

---

## Success Criteria

- [ ] [Measurable criterion 1]
- [ ] [Measurable criterion 2]
- [ ] [Acceptance test 1 passes]
- [ ] [Performance benchmark met]

---

## Testing Plan

### Unit Tests
- [Test scenario 1]
- [Test scenario 2]

### Integration Tests
- [End-to-end workflow test]

### Edge Cases
- [Unusual input or condition to test]

---

## Rollout Plan

1. **Development:** [Timeline]
2. **Testing:** [Who, when, environment]
3. **Staging Deployment:** [Date]
4. **Production Deployment:** [Date]

---

## Risks & Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| [Risk description] | [Low/Med/High] | [Low/Med/High] | [How to prevent or handle] |

---

## Open Questions

- [ ] [Question needing answer before implementation] → **Owner:** [Name]

---

## References

- [Link to related specs or documentation]
- [Link to mockups or diagrams]
```

## AGENTS.md Template

Use this template to configure AI agent behavior.

```markdown
# AI Agent Instructions

**Project:** [Project Name]
**Last Updated:** [Date]

---

## Agent Behavior: General Guidelines

### 1. Always read context files first

Before starting any work, the agent **must read**:

- `PROJECT_MEMORY.md` - For project conventions and patterns
- `CHANGE_HISTORY.md` - For recent changes and lessons learned
- `SPEC-[feature].md` - For current task requirements (if applicable)

### 2. Follow established patterns

- Use `PROJECT_MEMORY.md` to recall project conventions, naming, and constraints
- Check `CHANGE_HISTORY.md` to avoid repeating past mistakes
- Never deviate from established patterns without explicit justification

### 3. Maintain documentation

- **CHANGE_HISTORY.md:** Append an entry for every significant change
  - Include: date, intent, diff, impact, rollback plan, status
- **PROJECT_MEMORY.md:** Propose updates when discovering new patterns or gotchas

### 4. Guardrails

- Never delete or rewrite prior entries in `CHANGE_HISTORY.md`
- Never override memory without clear rationale
- Always reference file paths, versions, and line ranges
- All updates should be **additive or corrective via append**

---

## Coding Standards

### Naming Conventions

- [Project-specific naming rules]
- [File naming patterns]
- [Variable naming standards]

### Code Structure

- [Organizational preferences]
- [Comment style and requirements]
- [Function/module size guidelines]

### Error Handling

- [Standard error handling pattern]
- [Logging requirements]

---

## Workflow Preferences

### Before implementing changes

1. Read relevant context files
2. Check for similar past changes in CHANGE_HISTORY
3. Create or reference a SPEC if this is a new feature
4. Propose a plan before coding

### During implementation

1. Work incrementally
2. Follow established patterns from PROJECT_MEMORY
3. Test each step before proceeding

### After implementation

1. Append entry to CHANGE_HISTORY.md
2. Update PROJECT_MEMORY.md if new patterns emerged
3. Suggest test cases

---

## Project-Specific Rules

[Add any project-specific guidelines here]
```

## Common Prompts

### Starting a New Feature

```
Plan: Implement [feature name] per SPEC-[feature-name].md

@SPEC-[feature-name].md
@PROJECT_MEMORY.md
@CHANGE_HISTORY.md
@[relevant-files]

Create a detailed implementation plan including:
- Step-by-step breakdown
- Files to be modified/created
- Potential risks and mitigations
- Testing approach
- Todo list for execution

Check CHANGE_HISTORY for similar past work and learn from it.
Follow patterns from PROJECT_MEMORY.
```

### Implementing a Step

```
Act: Implement step [N] of the plan - [step description]

@SPEC-[feature-name].md
@[relevant-files]
@PROJECT_MEMORY.md

Follow the plan we created.
Apply our coding standards.
Include appropriate error handling per our established pattern.
```

### Code Review

```
Code Review: Review changes for [feature name]

@SPEC-[feature-name].md
@[modified-files]
@PROJECT_MEMORY.md

I did not write this code. Review it objectively for:
- Alignment with spec requirements
- Code quality and adherence to standards
- Potential bugs or edge cases
- Performance concerns
- Error handling completeness
- Documentation adequacy

Provide specific, actionable feedback.
```

### Updating Memory

```
@PROJECT_MEMORY.md

Update memory with new learnings from today's work:

New patterns discovered:
- [Pattern description]

Gotchas encountered:
- [Issue and solution]

Decisions made:
- [Decision and rationale]

Add these to the appropriate sections in PROJECT_MEMORY.
```

### Appending to Change History

```
@CHANGE_HISTORY.md

Create a new entry for today's change using the standard template:

Intent: [What we did and why]
Files: [List of modified files]
Changes: [Summary of key changes]

Include: diff, impact analysis, rollback plan, and current status (APPROVED).
```

## Quick Start Checklist

Use this checklist when setting up a new project with AI assistance:

```markdown
# AI-Assisted Development Setup Checklist

## Project Initialization

- [ ] Create `PROJECT_MEMORY.md` from template
- [ ] Create `CHANGE_HISTORY.md` from template
- [ ] Create `AGENTS.md` or `.cursor/rules/` files
- [ ] Add project-specific coding standards to memory
- [ ] Document key architecture in memory

## Cursor Setup

- [ ] Install Cursor IDE
- [ ] Configure model settings (GPT-4 or Claude Sonnet 4.5)
- [ ] Initialize memory bank: `npx cursor-bank init`
- [ ] Add workspace folders (copybooks, utilities, etc.)
- [ ] Test with a small prompt

## ChatGPT Setup

- [ ] Set up Custom Instructions
- [ ] Create a Project for this codebase
- [ ] Upload reference documentation
- [ ] Test with a planning question

## Codex Setup

- [ ] Install NodeJS
- [ ] Install Codex CLI: `npm install -g @openai/codex`
- [ ] Run `codex` and authenticate
- [ ] Test with a simple code analysis task

## Version Control

- [ ] Set up separate WIP repository (optional)
- [ ] Configure SVN ignore for AI temp files
- [ ] Document commit conventions

## First Task

- [ ] Create a SPEC for your first small task
- [ ] Use Plan → Code → Review workflow
- [ ] Update memory and change history
- [ ] Commit successful changes
```

---

## Next Steps

- Review [Troubleshooting](./troubleshooting.md) for common issues
- Explore [Prompt Library](./prompt-library.md) for more examples
- Start with a small task to test your setup
