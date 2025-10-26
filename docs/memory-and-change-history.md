---
title: Memory & Change History
description: Context tracking and knowledge management system
sidebar_position: 6
last_updated: 2025-10-10
tags: [memory, change-history, documentation, context]
---

# Memory & Change History System

The Memory and Change History system keeps a running "brain" (Memory) so the next person or agent starts with context, and a "black box recorder" (Change History) so we can audit exactly what changed and why.

## The Two-Document System

### PROJECT_MEMORY.md - Durable Context

**Purpose:** Store stable, reusable knowledge that persists across sessions and agents.

**Think of it as:** The project's long-term memory and knowledge base.

**Contains:**

- Problem space and business rules
- Architecture and data flow
- Known patterns and gotchas
- Terminology glossary
- Open questions and decisions

### CHANGE_HISTORY.md - Append-Only Timeline

**Purpose:** Record a chronological log of all proposed and approved changes.

**Think of it as:** The project's black box flight recorder.

**Contains:**

- Date and author/agent of each change
- Intent and motivation
- Before/after diffs
- Impact analysis
- Status (proposed, approved, rolled back)

## Setup

Create these two files in your workspace root:

```
project-root/
├── PROJECT_MEMORY.md
├── CHANGE_HISTORY.md
├── .cursor/
│   └── rules/
└── [your code files]
```

## PROJECT_MEMORY.md Template

```markdown
# Project Memory (Durable Context)

## Problem Space

### What we're trying to solve

- [Describe the business problem or need]

### Business rules

- [Key business rules that constrain or guide the solution]

## Architecture / Data Flow

### Systems touched

- [List of systems, databases, external interfaces]

### Key files/modules

- **CUSTMAINT.cob** - Customer maintenance program
- **CP-CUSTHDR.cpy** - Customer header copybook
- [etc.]

### Inputs/outputs

- Input: Customer update transactions from screen
- Output: Updated CUSTOMER table, audit records to CUST-AUDIT
- [etc.]

## Known Patterns & Gotchas

### Reusable routines

- **Error handling:** Use PERFORM ERROR-HANDLER pattern with ERR-CODE
- **File operations:** Always check FILE-STATUS after I/O
- [etc.]

### Known pitfalls

- Credit card fields must be encrypted before storing
- Date validation: Account for leap years
- [etc.]

## Glossary

- **Clinic** - Internal testing environment for running programs
- **CP-** - Prefix for copybook files
- **SVN** - Subversion version control system
- [etc.]

## Open Questions

- [ ] Should we archive audit records after 7 years or keep indefinitely? → Owner: [Name]
- [ ] What's the maximum concurrent users we need to support? → Owner: [Name]
- [etc.]

## Decisions Made

### 2025-10-10: Use separate audit table instead of triggers

**Decision:** Implement audit trail via explicit calls in COBOL rather than database triggers.
**Rationale:** More control, easier debugging, works in clinic environment.
**Implications:** Must remember to add audit calls to all update points.

[etc.]
```

## CHANGE_HISTORY.md Template

````markdown
# Change History (Append-Only)

> **⚠️ IMPORTANT:** Never delete or modify existing entries. Always append new entries at the top.

---

## 2025-10-10 14:30 (John Doe / Cursor AI)

### Intent

Add credit limit validation to CUSTMAINT to prevent invalid amounts.

### Proposed Diff (before -> after)

```diff
File: CUSTMAINT.cob

--- WORKING-STORAGE SECTION
+++ WORKING-STORAGE SECTION
+   77 MAX-CREDIT-LIMIT    PIC 9(7)V99 VALUE 1000000.00.
+   77 MIN-CREDIT-LIMIT    PIC 9(7)V99 VALUE 0.

--- Paragraph VALIDATE-INPUT
+++ Paragraph VALIDATE-INPUT
+   PERFORM VALIDATE-CREDIT-LIMIT.
+
+++ New paragraph:
+   VALIDATE-CREDIT-LIMIT.
+       IF CUST-CREDIT-LIMIT > MAX-CREDIT-LIMIT OR
+          CUST-CREDIT-LIMIT < MIN-CREDIT-LIMIT
+           MOVE "ERR-001" TO ERR-CODE
+           MOVE "Credit limit must be between 0 and 1,000,000" TO ERR-MSG
+           PERFORM ERROR-HANDLER
+       END-IF.
```
````

### Impact

- Affects: CUSTMAINT.cob only
- Risk level: Low - validation only, no data changes
- Performance: Negligible - simple numeric comparison

### Rollback Plan

```bash
svn revert CUSTMAINT.cob
# or revert to revision 12345
svn update -r 12345 CUSTMAINT.cob
```

### Status

✅ **APPROVED** - Implemented and tested on 2025-10-10 15:00

### Test Results

- ✅ Valid amount ($50,000) - Accepted
- ✅ Zero amount - Accepted
- ✅ Negative amount - Rejected with ERR-001
- ✅ Amount over limit ($2,000,000) - Rejected with ERR-001

---

## 2025-10-08 10:15 (Jane Smith / ChatGPT)

[Previous entry...]

---

````

## Agent Synchronization Rules

To ensure Cursor and other AI agents respect and maintain these documents, add this to your `AGENTS.md` or `.cursor/rules/`:

```markdown
## Agent Behavior: Synchronizing with Change History & Project Memory

### 1. Monitor existing docs
- Before starting new work, the agent **must read** `CHANGE_HISTORY.md` and `PROJECT_MEMORY.md`.
- Use them as **reference** to avoid repeating previous ideas or conflicting with past decisions.

### 2. Use history & memory in reasoning
- When proposing changes, check if a similar change was already made (per `CHANGE_HISTORY.md`).
- Use `PROJECT_MEMORY.md` to recall project conventions, patterns, naming, and constraints.
- If new insight is discovered (edge case, rule, pattern), propose an update to memory.

### 3. How to update docs
- **CHANGE_HISTORY.md**: For every proposed or accepted change, append a new entry.
  - Include date, your identity, intent, diff, impact, rollback plan, and status.
- **PROJECT_MEMORY.md**: For stable, reusable knowledge (not one-off decisions), propose adding or refining entries.
  - Do *not* erase or override past memory entries without clear rationale.

### 4. Guardrails & integrity
- Never delete or rewrite prior entries in `CHANGE_HISTORY.md` or older memory.
- All updates should be **additive or corrective via append** (unless explicitly asked).
- Always reference file paths, versions, and line ranges when writing history or memory entries.

### 5. Workflow placement
- At session start: agent reads both docs before accepting further prompts.
- Before proposing changes: agent cross-checks history & memory.
- After producing a diff: agent outputs the new entry blocks for both docs.
- In multi-turn tasks: agent preserves memory-awareness across turns.
````

## Workflow Integration

### During Spec Phase

**Read PROJECT_MEMORY.md:**

```
@PROJECT_MEMORY.md

Before creating the spec, what existing patterns and constraints should I know about?
```

### During Plan Phase

**Check CHANGE_HISTORY.md:**

```
@CHANGE_HISTORY.md

Has anyone attempted something similar before? What can we learn from past changes?
```

### During Code Phase

**Reference both documents:**

```
@PROJECT_MEMORY.md
@CHANGE_HISTORY.md

Implement the change following our established patterns and avoiding past pitfalls.
```

### After Code Phase

**Update both documents:**

1. **Add to PROJECT_MEMORY.md** if you discovered:
   - A new reusable pattern
   - A gotcha or edge case
   - A clarification to terminology
   - An architectural decision

2. **Append to CHANGE_HISTORY.md** for:
   - Every significant code change
   - New features
   - Bug fixes
   - Refactoring
   - Configuration changes

## Best Practices

### For PROJECT_MEMORY.md

#### Do:

- ✅ Add stable, reusable knowledge
- ✅ Document patterns that will be used again
- ✅ Clarify terminology and conventions
- ✅ Record architectural decisions and rationale
- ✅ Keep it concise and scannable

#### Don't:

- ❌ Document one-off changes (those go in CHANGE_HISTORY)
- ❌ Include implementation details (link to code instead)
- ❌ Let it become stale (review and update regularly)
- ❌ Delete old information without documenting why

### For CHANGE_HISTORY.md

#### Do:

- ✅ Always append new entries at the top
- ✅ Include complete context (intent, diff, impact)
- ✅ Reference specific file paths and line numbers
- ✅ Document rollback procedures
- ✅ Update status as changes progress
- ✅ Include test results

#### Don't:

- ❌ Ever delete or modify old entries
- ❌ Skip entries for "trivial" changes (they may matter later)
- ❌ Omit the rollback plan
- ❌ Forget to update status to APPROVED after testing

## Prompts for Maintaining Memory

### Update PROJECT_MEMORY.md

```
@PROJECT_MEMORY.md

We just implemented [feature]. We discovered:
- [New pattern we used]
- [Gotcha we encountered]
- [Decision we made]

Update PROJECT_MEMORY.md with these learnings in the appropriate sections.
```

### Append to CHANGE_HISTORY.md

```
@CHANGE_HISTORY.md

Create a new entry for today's change:
- Intent: [What we tried to do]
- Files modified: [List]
- Key changes: [Summary]

Use the standard template with diff, impact, and rollback plan.
```

### Review and Clean Up Memory

```
@PROJECT_MEMORY.md

Review this memory file for:
- Outdated information that should be archived
- Missing patterns we've been using
- Unclear terminology that needs definition
- Open questions that have been answered

Suggest updates to keep this current and useful.
```

## Example: Memory System in Action

### Scenario: Implementing Audit Trail

**Step 1 - Before Starting (Check Memory):**

Developer reads `PROJECT_MEMORY.md`:

- Sees standard error handling pattern
- Notes glossary: "CP-" prefix for copybooks
- Recalls decision to avoid database triggers

Developer reads `CHANGE_HISTORY.md`:

- Sees previous attempt at logging failed due to file locking
- Notes that similar feature in INVMAINT uses sequential writes

**Step 2 - After Implementing (Update Memory):**

**Update PROJECT_MEMORY.md:**

```markdown
## Known Patterns & Gotcas

### Reusable routines

[existing content...]

- **Audit logging:** Use sequential write to AUDIT-FILE with error handling.
  Pattern established in CUSTMAINT, reusable for other entities.

### Known pitfalls

[existing content...]

- **Audit file locking:** Always open AUDIT-FILE at program start, keep open
  throughout. Opening/closing for each write causes contention.
```

**Append to CHANGE_HISTORY.md:**

```markdown
## 2025-10-10 16:00 (Developer / Cursor AI)

### Intent

Add audit trail to customer updates per SPEC-audit-trail.md

### Proposed Diff (before -> after)

[Complete diff here...]

### Impact

- Affects: CUSTMAINT.cob, new file CP-CUST-AUDIT.cpy
- Risk level: Medium - adds file I/O to update path
- Performance: less than 50ms per update (measured in clinic)

### Rollback Plan

svn revert CUSTMAINT.cob
rm CP-CUST-AUDIT.cpy

### Status

✅ APPROVED - Tested and deployed 2025-10-10

### Test Results

[Test results here...]
```

## Troubleshooting

### Memory File Getting Too Large

**Problem:** PROJECT_MEMORY.md is hundreds of lines and hard to navigate.

**Solution:**

- Split into multiple files (PROJECT_MEMORY-architecture.md, PROJECT_MEMORY-patterns.md)
- Archive old decisions to PROJECT_MEMORY-archive.md
- Link from main memory file

### Change History Getting Unwieldy

**Problem:** CHANGE_HISTORY.md has years of entries.

**Solution:**

- Archive old years: CHANGE_HISTORY-2024.md
- Keep current year in main file
- Link to archives at top of current file

### Agents Not Reading Memory

**Problem:** AI agents ignore memory files.

**Solution:**

- Add explicit rule in AGENTS.md or .cursor/rules
- Pin memory files with @ in prompts
- Use "Add Context" in Cursor to always include memory

### Conflicting Information

**Problem:** CHANGE_HISTORY and PROJECT_MEMORY have conflicting information.

**Solution:**

- CHANGE_HISTORY is authoritative for "what happened"
- PROJECT_MEMORY is authoritative for "current truth"
- Add note in PROJECT_MEMORY referencing the relevant change history entry

---

## Next Steps

- Download [Templates](./templates.md) for quick start
- Set up your [Cursor rules](./cursor.md#rules-files-best-practices) to reference memory
- Review [Troubleshooting](./troubleshooting.md) for common issues
