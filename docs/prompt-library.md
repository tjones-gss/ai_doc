---
title: Prompt Library
description: Reusable prompts for common development tasks
sidebar_position: 9
last_updated: 2025-10-10
tags: [prompts, examples, best-practices]
---

# Prompt Library

This library contains reusable prompts for common development tasks. Copy and adapt these prompts for your specific needs.

## Planning & Analysis

### Analyze Requirements

```
@[requirements-doc]
@PROJECT_MEMORY.md

Analyze these requirements and:
1. Identify ambiguities or missing information
2. List technical challenges or risks
3. Suggest clarifying questions to ask stakeholders
4. Outline a high-level implementation approach

Present findings in a structured format.
```

### Create Implementation Plan

```
Plan: Implement [feature name] per SPEC-[feature].md

@SPEC-[feature].md
@PROJECT_MEMORY.md
@CHANGE_HISTORY.md
@[relevant-files]

Create a detailed implementation plan including:
- Step-by-step breakdown with estimated effort
- Files to be modified/created
- Dependencies and prerequisites
- Potential risks and mitigation strategies
- Testing approach and key test scenarios
- Todo list for execution

Check CHANGE_HISTORY for similar past work and incorporate lessons learned.
Follow established patterns from PROJECT_MEMORY.
```

### Architectural Analysis

```
@[main-program.cob]
@[related-files]
@PROJECT_MEMORY.md

Analyze the architecture of this system and provide:
1. High-level component diagram (text-based)
2. Data flow description
3. Key dependencies and integration points
4. Potential bottlenecks or scalability concerns
5. Recommendations for improvement

Focus on [specific aspect if relevant, e.g., "error handling", "performance", "maintainability"]
```

## Code Implementation

### Implement Feature Step

```
Act: Implement step [N] - [brief description]

@SPEC-[feature].md
@[relevant-files]
@PROJECT_MEMORY.md

Following the plan we created:
- Implement [specific functionality]
- Apply our coding standards from PROJECT_MEMORY
- Use our established error handling pattern
- Include appropriate comments per our style guide
- Ensure backward compatibility with existing code

Stop after completing this step so we can test before proceeding.
```

### Refactor Code

```
@[file-to-refactor]
@PROJECT_MEMORY.md

Refactor [specific section/function] to improve [readability/performance/maintainability].

Requirements:
- Preserve existing functionality exactly (no behavior changes)
- Follow our coding standards
- Improve [specific aspect: naming, structure, efficiency]
- Add comments to clarify complex logic
- Maintain backward compatibility

Show before/after comparison for the refactored section.
```

### Add Error Handling

```
@[file-name]
@PROJECT_MEMORY.md

Review [specific section/function] and add comprehensive error handling.

Include:
- Input validation
- File operation error checking
- Database error handling
- Meaningful error messages per our ERR-XXXX format
- Logging per our established pattern
- Graceful degradation where appropriate

Follow our standard error handling pattern from PROJECT_MEMORY.
```

### Generate Test Cases

```
@[file-name]
@SPEC-[feature].md

Generate comprehensive test cases for [specific function/feature].

Include:
- Happy path scenarios (valid inputs)
- Edge cases (boundary conditions, empty inputs, max values)
- Error scenarios (invalid inputs, missing data)
- Integration scenarios (interactions with other components)

For each test case provide:
- Test case name/ID
- Input data
- Expected output
- Preconditions/setup
- Test steps
```

## Code Review

### Comprehensive Code Review

```
Code Review: [feature/change name]

@SPEC-[feature].md
@[modified-files]
@PROJECT_MEMORY.md

I did not write this code. Review it objectively for:

**Correctness:**
- Does it meet all spec requirements?
- Are there logic errors or bugs?
- Are edge cases handled properly?

**Quality:**
- Does it follow our coding standards?
- Is naming clear and consistent?
- Are comments adequate?
- Is the code maintainable?

**Performance:**
- Are there performance concerns?
- Any inefficient algorithms or operations?

**Security:**
- Input validation adequate?
- Potential vulnerabilities?
- Sensitive data handled properly?

**Error Handling:**
- Comprehensive error handling?
- Meaningful error messages?
- Proper logging?

Provide specific, actionable feedback with file:line references.
```

### Security Review

```
Security Review: [feature/change name]

@[modified-files]

Review this code specifically for security issues:

1. **Input Validation:**
   - Are all user inputs validated?
   - SQL injection prevention?
   - Command injection prevention?

2. **Data Protection:**
   - Sensitive data encrypted?
   - Passwords/credentials handled securely?
   - PII data protected?

3. **Access Control:**
   - Proper authorization checks?
   - No privilege escalation vulnerabilities?

4. **Error Handling:**
   - Error messages don't leak sensitive info?
   - Failures handled securely?

5. **Dependencies:**
   - External libraries secure?
   - Known vulnerabilities?

Highlight any concerns with severity level (Critical/High/Medium/Low).
```

### Performance Review

```
Performance Review: [feature/change name]

@[modified-files]
@SPEC-[feature].md

Analyze this code for performance implications:

1. **Algorithmic Efficiency:**
   - Are algorithms optimal for the use case?
   - Any unnecessary O(n²) operations?

2. **Database/File I/O:**
   - Minimize I/O operations?
   - Batching opportunities?
   - Proper indexing?

3. **Memory Usage:**
   - Memory allocation efficient?
   - Potential memory leaks?

4. **Scalability:**
   - How does this perform at scale?
   - Bottlenecks under load?

5. **Caching:**
   - Opportunities to cache results?
   - Redundant calculations?

Provide specific optimization suggestions with expected impact.
```

## Documentation

### Generate Code Documentation

```
@[file-name]

Generate comprehensive documentation for this code.

Include:
1. **Overview:** Purpose and responsibilities
2. **Dependencies:** Required files, copybooks, databases
3. **Key Functions/Paragraphs:** Description of each major section
4. **Data Structures:** Important variables and their purpose
5. **Error Handling:** How errors are handled
6. **Usage Examples:** How to call/use this code
7. **Known Limitations:** Constraints or gotchas

Format as markdown suitable for our documentation site.
```

### Update PROJECT_MEMORY

```
@PROJECT_MEMORY.md

Update PROJECT_MEMORY with learnings from today's work on [feature]:

**New Patterns Discovered:**
- [Pattern name]: [Description and when to use]

**Gotchas Encountered:**
- [Issue]: [Explanation and solution]

**Decisions Made:**
- [Decision]: [Rationale and implications]

**Glossary Updates:**
- [New term]: [Definition]

Add these to the appropriate sections in PROJECT_MEMORY.
Maintain existing structure and formatting.
```

### Create CHANGE_HISTORY Entry

```
@CHANGE_HISTORY.md

Create a new entry for today's change using the standard template:

**Context:**
- Feature: [Feature name]
- Intent: [What we did and why]
- Files modified: [List]
- Changes summary: [Brief description]

Include:
- Proper diff format showing before/after
- Impact analysis (affected systems, risk level, performance)
- Rollback plan with specific commands
- Status: APPROVED (if tested) or PROPOSED (if pending)
- Test results summary

Append to top of CHANGE_HISTORY.md.
```

## Debugging & Troubleshooting

### Debug Issue

```
@[file-with-issue]
@[error-log or test output]
@PROJECT_MEMORY.md

Debug this issue:

**Symptoms:**
[Describe the problem]

**Expected Behavior:**
[What should happen]

**Actual Behavior:**
[What's happening instead]

**Steps to Reproduce:**
1. [Step 1]
2. [Step 2]

Analyze the code and:
1. Identify the root cause
2. Explain why it's happening
3. Suggest a fix
4. Identify if this could affect other parts of the code
5. Recommend preventive measures

Reference our error handling patterns from PROJECT_MEMORY.
```

### Analyze Error Log

```
@[error-log-file]
@[relevant-code-files]

Analyze this error log and:

1. **Identify Patterns:**
   - Frequency of each error type
   - Correlation between errors
   - Temporal patterns

2. **Root Causes:**
   - What's causing each error?
   - Are they symptoms of a deeper issue?

3. **Impact Assessment:**
   - Severity of each error
   - User impact
   - Data integrity concerns

4. **Recommendations:**
   - Immediate fixes needed
   - Long-term improvements
   - Monitoring/alerting suggestions

Prioritize findings by severity and frequency.
```

### Investigate Performance Issue

```
@[slow-program]
@PROJECT_MEMORY.md

Investigate performance issue in [specific function/area]:

**Observations:**
- [What's slow]
- [How slow: timing/measurements]
- [Conditions under which it's slow]

Analyze and provide:
1. **Profiling:** Where is time being spent?
2. **Bottlenecks:** Specific operations causing delays
3. **Root Cause:** Why these operations are slow
4. **Optimization Opportunities:** Ranked by potential impact
5. **Recommended Approach:** Step-by-step optimization plan

Consider our performance patterns from PROJECT_MEMORY.
```

## Data & Migration

### Generate SQL Query

```
Generate a SQL query to [describe what you need].

Requirements:
- Database: [database type]
- Tables involved: [list tables]
- Conditions: [filters, joins, etc.]
- Output format: [what columns, ordering]

Include:
- The complete SQL query
- Explanation of each part
- Expected result format
- Any performance considerations
```

### Create Data Migration Script

```
Create a data migration script to [describe migration].

**Source:**
- [Source table/file format]
- [Sample data structure]

**Target:**
- [Target table/file format]
- [Required transformations]

**Requirements:**
- Validate data before migration
- Handle errors gracefully
- Log all transformations
- Rollback capability
- Performance: [size of data, timing constraints]

Include:
- Complete migration script
- Validation checks
- Error handling
- Rollback procedure
- Test data samples
```

### Generate Test Data

```
Generate realistic test data for [table/file].

**Schema:**
[Provide table structure or copybook]

**Requirements:**
- [Number] records
- Cover edge cases: [list specific cases]
- Realistic values based on [domain/business rules]
- Include both valid and invalid cases for testing

Provide data in [format: SQL inserts, CSV, test file, etc.]
```

## Learning & Exploration

### Explain Code

```
@[file-name]

Explain how this code works to someone unfamiliar with [COBOL/the project].

Provide:
1. **High-level overview:** What does this code do and why?
2. **Step-by-step walkthrough:** Explain the flow and logic
3. **Key concepts:** Technical concepts a beginner should understand
4. **Gotchas:** Non-obvious behaviors or edge cases
5. **Best practices:** What this code does well (or could improve)

Use plain language and analogies where helpful.
```

### Research Best Practices

```
Research best practices for [specific topic] in the context of [your technology/domain].

Consider:
- Industry standards
- Security implications
- Performance considerations
- Maintainability
- Our constraints: [list relevant constraints from PROJECT_MEMORY]

Provide:
1. Summary of best practices
2. How they apply to our situation
3. Examples or code snippets
4. Recommended approach for our project
5. References for further reading
```

### Compare Approaches

```
Compare different approaches for [specific task/problem]:

**Approach 1:** [Description]
**Approach 2:** [Description]
**Approach 3:** [Description]

For each approach, analyze:
- **Pros:** Advantages and benefits
- **Cons:** Disadvantages and limitations
- **Complexity:** Implementation difficulty
- **Performance:** Speed and efficiency
- **Maintainability:** Long-term sustainability
- **Fit:** How well it suits our context

Recommend the best approach for our situation given:
- [Constraint 1]
- [Constraint 2]
- [Priority: e.g., "performance over simplicity"]
```

## Collaboration

### Handoff to Next Developer

```
@PROJECT_MEMORY.md
@CHANGE_HISTORY.md
@[work-in-progress-files]

Create a handoff document for the next developer working on [feature/project].

Include:
1. **Current Status:** What's been completed, what's in progress, what's remaining
2. **Context:** Why we're doing this, key decisions made
3. **Current Blockers:** Issues preventing progress
4. **Next Steps:** Recommended actions with priority
5. **Key Files:** Important files to know about
6. **Gotchas:** Things to watch out for
7. **Resources:** Documentation, references, contacts

Make it easy for someone to pick up where you left off.
```

### Code Review Response

```
@[code-review-feedback]
@[modified-files]

Address this code review feedback:

[Paste review comments]

For each comment:
1. Acknowledge the concern
2. Explain your approach or make the requested change
3. If disagreeing, provide rationale and alternative
4. Update code as needed

Respond professionally and be open to feedback.
Show updated code snippets where relevant.
```

---

## Tips for Effective Prompts

### Be Specific

**Bad:**

```
Fix the bug
```

**Good:**

```
@CUSTMAINT.cob:450-500

Fix the bug where credit limit validation fails for amounts exactly $100,000.
The issue is in the VALIDATE-CREDIT-LIMIT paragraph around line 475.
Expected: amounts up to and including $100,000 should be accepted.
Actual: $100,000 is rejected.
```

### Provide Context

**Bad:**

```
Add error handling
```

**Good:**

```
@CUSTMAINT.cob
@PROJECT_MEMORY.md

Add comprehensive error handling to the FILE-OPERATIONS section (lines 200-250).
Follow our standard error handling pattern from PROJECT_MEMORY.
Use our ERR-XXXX error code format and log to AUDIT-FILE.
```

### Break Down Complex Tasks

**Bad:**

```
Build the entire customer management feature
```

**Good:**

```
Plan: Build customer management feature per SPEC-customer-mgmt.md

First, create a detailed plan breaking this into 10-15 implementable steps.
Each step should be testable independently.
Then we'll implement step by step.
```

### Use File References

**Bad:**

```
Look at the customer program
```

**Good:**

```
@CUSTMAINT.cob
@CP-CUSTHDR.cpy
@PROJECT_MEMORY.md

Analyze the customer update logic in CUSTMAINT.cob paragraph UPDATE-CUSTOMER
```

---

## Next Steps

- Apply these prompts to your projects
- Adapt them to your specific context
- Share successful prompts with your team
- Add new prompts to this library as you discover them
