---
title: Workflow - Spec → Plan → Code → Review
description: AI-assisted development methodology
sidebar_position: 5
last_updated: 2025-10-10
tags: [workflow, methodology, best-practices]
---

# Spec → Plan → Code → Review

This workflow represents our recommended methodology for AI-assisted development. It ensures quality, maintains context, and prevents the AI from going off-track.

## The Four Phases

### 1. Spec - Document What You're Building

Create a specification document that clearly defines what you're building and why.

**Where:** Create a spec file in your project (e.g., `SPEC-feature-name.md`)

**What to Include:**

- **Objective** - What are you trying to accomplish?
- **Scope** - What's in scope and out of scope?
- **Requirements** - Functional and non-functional requirements
- **Constraints** - Limitations, dependencies, compatibility needs
- **Success Criteria** - How do you know when you're done?

**Example Spec:**

```markdown
# Spec: Add Audit Trail to Customer Updates

## Objective

Track all changes to customer records for compliance and debugging purposes.

## Scope

### In Scope

- Log all updates to CUSTOMER table
- Capture: user ID, timestamp, old values, new values
- Store audit records in CUST-AUDIT table

### Out of Scope

- Audit trail for other tables (future phase)
- UI for viewing audit history (future phase)

## Requirements

- Minimal performance impact (<50ms per update)
- Must work with existing CUSTMAINT program
- Retain audit records for 7 years
- Use existing error handling patterns

## Constraints

- Must use Fujitsu COBOL syntax
- Cannot modify CUSTOMER table structure
- Must work with current clinic environment

## Success Criteria

- All customer updates logged successfully
- No degradation in CUSTMAINT performance
- Audit records queryable by customer ID and date range
- Zero data loss in audit trail
```

**Why This Matters:**

> **💡 Tip:** The spec keeps the AI focused on exactly what it should be working on or looking at. It prevents scope creep and hallucinations.

### 2. Plan - Generate a Gameplan

Have the AI create a detailed plan before writing any code.

**Where:** In Cursor chat or ChatGPT

**Prompt Template:**

```
Plan: [Your task description]

Read the spec in SPEC-[feature-name].md and create a detailed implementation plan.
Include:
- Step-by-step breakdown
- Files that need to be modified
- New files that need to be created
- Potential risks and mitigations
- Testing approach

Generate a todo list that we'll follow during implementation.
```

**Example Plan Request:**

```
Plan: Implement the audit trail feature described in SPEC-audit-trail.md

@SPEC-audit-trail.md
@CUSTMAINT.cob
@PROJECT_MEMORY.md

Create a thorough gameplan and build a todo list that adheres to our coding standards.
```

**What Good Plans Include:**

1. **File-level breakdown** - Which files change and why
2. **Sequence** - Order of operations
3. **Dependencies** - What needs to happen first
4. **Risk identification** - What could go wrong
5. **Rollback plan** - How to undo if needed
6. **Testing strategy** - How to validate each step

**Review the Plan:**

- Does it address all requirements?
- Are there any gaps or risks?
- Is the approach sound?
- Does it align with existing patterns?

> **⚠️ Note:** Don't skip the review step. A bad plan leads to bad code. Fix the plan before proceeding.

### 3. Code - Execute the Plan

Once you have a good plan, execute it with AI assistance.

**Where:** Cursor (for most coding tasks)

**Prompt Template:**

```
Act: Implement step [N] of the plan

@SPEC-[feature-name].md
@[relevant files]

Follow the plan we created. Implement [specific step].
Apply our coding standards from systemPatterns.md.
```

**Best Practices During Coding:**

1. **Work incrementally** - One step at a time
2. **Verify each step** - Test before moving on
3. **Update todo list** - Mark steps complete
4. **Version control** - Commit working changes
5. **Update memory** - Add learnings to PROJECT_MEMORY.md

**Example Coding Prompts:**

```
Act: Implement step 1 - Create CUST-AUDIT copybook

@SPEC-audit-trail.md
@CP-CUSTHDR.cpy

Create the copybook for audit trail records following our copybook standards.
```

```
Act: Implement step 2 - Add audit logging to CUSTMAINT

@SPEC-audit-trail.md
@CUSTMAINT.cob
@CP-CUST-AUDIT.cpy

Add the audit trail logic to CUSTMAINT following the plan.
Use our standard error handling pattern.
```

**Version Control During Coding:**

> **💡 Tip:** Commit after each successful step. This allows you to roll back if the AI goes off track.

```bash
svn commit -m "Step 1: Add CUST-AUDIT copybook"
```

```bash
svn commit -m "Step 2: Add audit logging to CUSTMAINT"
```

### 4. Review - Fresh Eyes on the Changes

Use a separate AI instance to review the changes. This prevents the coding AI from defending its decisions.

**Where:** Open a new Cursor chat window OR use ChatGPT

**Why New Instance?**

The AI that wrote the code will have biases:

- It "knows" why it made certain decisions
- It may defend flawed logic
- It's less likely to catch its own mistakes

A fresh AI instance:

- Has no attachment to the code
- Reviews objectively
- Catches issues the original AI missed

**Prompt Template:**

```
Code Review: Review the changes made for [feature name]

@SPEC-[feature-name].md
@[modified files]
@CHANGE_HISTORY.md

Review these changes for:
- Alignment with spec
- Code quality and standards
- Potential bugs or edge cases
- Performance concerns
- Error handling completeness
- Documentation adequacy

Provide specific feedback and suggested improvements.
```

**Example Review Request:**

```
Code Review: Audit trail implementation

@SPEC-audit-trail.md
@CUSTMAINT.cob
@CP-CUST-AUDIT.cpy

This was implemented by another developer. Review for:
- Does it meet all requirements in the spec?
- Are there any bugs or edge cases not handled?
- Is error handling complete?
- Does it follow our coding standards?
- Are there any performance concerns?
```

**Review Checklist:**

- [ ] All requirements from spec addressed
- [ ] Code follows standards (naming, formatting, comments)
- [ ] Error handling complete
- [ ] Edge cases handled
- [ ] No performance regressions
- [ ] Documentation adequate
- [ ] Tests cover key scenarios
- [ ] No security issues

**After Review:**

1. **Address feedback** - Fix issues found
2. **Re-review if major changes** - Get fresh eyes again
3. **Update CHANGE_HISTORY.md** - Document what changed and why
4. **Final commit** - Commit reviewed and approved code

## Complete Workflow Example

### Scenario: Add Customer Credit Limit Validation

#### Phase 1: Spec

Create `SPEC-credit-limit-validation.md`:

```markdown
# Spec: Credit Limit Validation

## Objective

Prevent customer credit limits from being set to unrealistic values.

## Requirements

- Credit limit must be between $0 and $1,000,000
- Warning if credit limit > $100,000
- Error and reject if outside valid range
- Log validation failures to error log

## Constraints

- Modify CUSTMAINT.cob only
- Use existing error handling pattern
- Must work in current clinic environment
```

#### Phase 2: Plan

**Cursor Chat:**

```
Plan: Implement credit limit validation per SPEC-credit-limit-validation.md

@SPEC-credit-limit-validation.md
@CUSTMAINT.cob
@PROJECT_MEMORY.md
```

**AI Response:**

```
Todo List:
1. Add credit limit constants to WORKING-STORAGE
2. Create VALIDATE-CREDIT-LIMIT paragraph
3. Add validation call to customer update logic
4. Add error messages for invalid amounts
5. Update error log routine to capture validation failures
6. Test with edge cases

Estimated files to modify:
- CUSTMAINT.cob

Risks:
- Existing customers with limits >$1M (mitigation: grandfather existing)
- Performance impact (minimal - simple numeric comparison)
```

#### Phase 3: Code

**Step 1:**

```
Act: Implement todo item 1 - Add credit limit constants

@SPEC-credit-limit-validation.md
@CUSTMAINT.cob
```

**Verify, commit:**

```bash
svn commit -m "Add credit limit validation constants"
```

**Step 2:**

```
Act: Implement todo item 2 - Create VALIDATE-CREDIT-LIMIT paragraph

Following our paragraph structure standards.
```

**Continue for each step...**

#### Phase 4: Review

**New Cursor Chat:**

```
Code Review: Credit limit validation implementation

@SPEC-credit-limit-validation.md
@CUSTMAINT.cob

Review this implementation. I did not write this code.
Check for bugs, edge cases, and standards compliance.
```

**AI Feedback:**

```
Issues Found:
1. Credit limit of exactly $0 should be allowed per spec, but validation rejects it
2. Warning message for >$100k is displayed but not logged
3. Missing comment in VALIDATE-CREDIT-LIMIT paragraph

Suggestions:
1. Change validation from > 0 to >= 0
2. Add logging call after warning message
3. Add paragraph description comment
```

**Fix issues, re-review, commit:**

```bash
svn commit -m "Credit limit validation - addressed review feedback"
```

## Integration with Memory System

Throughout this workflow, maintain your memory system:

### During Spec Phase

- Check PROJECT_MEMORY.md for existing patterns
- Reference constraints and conventions

### During Plan Phase

- Check CHANGE_HISTORY.md for similar past changes
- Avoid repeating past mistakes

### During Code Phase

- Update PROJECT_MEMORY.md with new patterns discovered
- Append to CHANGE_HISTORY.md for each significant change

### During Review Phase

- Verify alignment with PROJECT_MEMORY.md
- Ensure CHANGE_HISTORY.md entry is complete

## When to Use This Workflow

### Always Use For:

- New features
- Complex bug fixes
- Refactoring efforts
- Architecture changes

### Can Skip For:

- Trivial changes (typo fixes, comment updates)
- Emergency hotfixes (but document after)
- Experimental spikes (but spec before committing)

## Common Pitfalls

### Skipping the Spec

**Problem:** AI doesn't know what you really want
**Result:** Wasted time, wrong implementation
**Solution:** Always spec, even if brief

### Not Reviewing the Plan

**Problem:** Bad plan leads to bad code
**Result:** Rework, bugs, frustration
**Solution:** Critically review plans before coding

### Using Same AI for Review

**Problem:** AI defends its own decisions
**Result:** Bugs slip through
**Solution:** Always use fresh AI instance for review

### Skipping Version Control

**Problem:** Can't roll back when AI goes wrong
**Result:** Lost work, manual recovery
**Solution:** Commit after each successful step

---

## Next Steps

- Set up your [Memory and Change History](./memory-and-change-history.md) system
- Explore [Templates](./templates.md) for specs and prompts
- Review [Troubleshooting](./troubleshooting.md) for common issues
