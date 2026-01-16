---
title: AI Prompting Guide
description: Master the art of effective prompting for AI-assisted COBOL modernization with ChatGPT Pro 5.2, Codex 5.2, Augment AI, Claude Code, and Cursor
sidebar_position: 7
last_updated: 2026-01-16
tags: [prompting, best-practices, chatgpt, codex, augment-ai, claude-code, cursor, cobol]
---

# AI Prompting Guide

Master the art of effective prompting to maximize AI productivity in your COBOL modernization work. This guide covers universal principles and tool-specific techniques for ChatGPT Pro 5.2, Codex 5.2, Augment AI, Claude Code, and Cursor.

## Why Effective Prompting Matters

Legacy COBOL systems are complex, with decades of business logic embedded in millions of lines of code. Effective prompting is the bridge between your domain expertise and AI capabilities. Poor prompts lead to:

- Generic responses that miss COBOL-specific nuances
- Wasted tokens and time on clarifying exchanges
- Suggestions that violate business rules or coding standards
- Incomplete analysis that misses critical dependencies

Effective prompts lead to:

- Precise, actionable recommendations
- Accurate understanding of copybook structures and data flows
- Consistent code generation following your standards
- Faster modernization with fewer iterations

> **💡 Tip:** Think of prompting as pair programming with AI. The clearer your communication, the better your partner performs.

## Core Prompting Principles

These principles apply across all AI tools and form the foundation of effective prompting.

### 1. Be Specific and Provide Context

AI models don't have access to your institutional knowledge. Always include:

- **File names and paths** - Be explicit: `INVMAINT.cob`, not "the inventory program"
- **Line numbers or paragraph names** - Focus attention: "CALC-TOTALS paragraph at line 450"
- **Business context** - Explain why: "This calculates discounts for preferred customers"
- **Constraints** - Mention limitations: "Must maintain backward compatibility with CUST-FILE format"

**Before:**

```
Fix the calculation bug
```

**After:**

```
In INVMAINT.cob, the CALC-INVENTORY-VALUE paragraph (lines 380-420) incorrectly
calculates inventory value when WS-QUANTITY is zero. Expected: return zero.
Actual: returns previous calculation. Fix while maintaining our standard
error handling pattern from PROJECT_MEMORY.md.
```

### 2. Use Structured Prompts (RTFC Framework)

Structure complex prompts using the RTFC framework:

| Component       | Purpose                         | Example                                                   |
| --------------- | ------------------------------- | --------------------------------------------------------- |
| **Role**        | Set the AI's expertise level    | "Act as a senior COBOL developer with Fujitsu experience" |
| **Task**        | Define what to accomplish       | "Refactor the file I/O operations"                        |
| **Format**      | Specify output structure        | "Provide changes as diff format with explanations"        |
| **Constraints** | Set boundaries and requirements | "Maintain backward compatibility, follow COBOL-85 syntax" |

**Example Structured Prompt:**

```
Role: Act as a senior COBOL developer specializing in Fujitsu COBOL modernization.

Task: Analyze the ORDER-PROCESSING section of ORDPROC.cob and identify
deprecated syntax that should be updated.

Format: For each finding, provide:
1. Line number and current code
2. Why it's deprecated
3. Recommended replacement
4. Risk level (Low/Medium/High)

Constraints:
- Only flag syntax deprecated in COBOL-2014 or earlier
- Preserve exact program behavior
- Consider our Fujitsu compiler compatibility requirements
```

### 3. Chain-of-Thought Prompting

For complex reasoning tasks, ask the AI to think step-by-step:

```
Analyze the data flow in CUSTMAINT.cob step by step:

1. First, identify all input sources (files, parameters, databases)
2. Then, trace how WS-CUSTOMER-RECORD is populated and modified
3. Next, map which paragraphs transform the data
4. Finally, identify all output destinations

Show your reasoning at each step before providing the final data flow diagram.
```

This technique is especially valuable for:

- Debugging complex logic
- Understanding call hierarchies
- Tracing data transformations through copybooks
- Analyzing error propagation paths

### 4. Few-Shot Examples

Provide examples of the output format you expect:

```
Generate documentation for each paragraph in the format below.

Example:
---
**VALIDATE-CUSTOMER**
- Purpose: Validates customer record fields before database update
- Inputs: WS-CUSTOMER-RECORD (from CP-CUSTHDR.cpy)
- Outputs: WS-VALIDATION-STATUS (88-level flags)
- Calls: VALIDATE-ADDRESS, VALIDATE-CREDIT
- Error Handling: Sets ERR-1001 for invalid name, ERR-1002 for invalid address
---

Now document these paragraphs: CALC-DISCOUNT, UPDATE-INVENTORY, WRITE-AUDIT-LOG
```

### 5. Iterative Refinement

Prompting is a conversation, not a single query. Use iterative refinement:

1. **Start broad** - Get initial analysis or suggestions
2. **Drill down** - Ask for details on specific areas
3. **Correct course** - Provide feedback on what's missing or wrong
4. **Validate** - Confirm understanding before implementation

**Iteration Example:**

```
[Prompt 1] Analyze INVMAINT.cob for potential performance issues.

[Response 1] Found 3 areas: file I/O in loop, SEARCH ALL usage, nested PERFORMs...

[Prompt 2] Focus on the file I/O issue. Show me the specific lines and
explain why it's a problem with our typical data volumes of 50,000 records.

[Response 2] Lines 280-295 open/close the file inside the PROCESS-ORDERS loop...

[Prompt 3] Good analysis. Now suggest a refactored approach that uses
our standard batch processing pattern from PROJECT_MEMORY.md.
```

---

## Tool-Specific Prompting Guides

Each AI tool has unique strengths and optimal prompting patterns. Match your prompts to the tool's capabilities.

### ChatGPT Pro 5.2 Prompts

**Best for:** Architecture analysis, code explanation, documentation, research, planning

ChatGPT Pro 5.2 with GPT-5.2 excels at high-level reasoning and multi-turn conversations. Use it as your senior architect for planning and analysis.

#### Key Features to Leverage

| Feature                | How to Use                                                                        |
| ---------------------- | --------------------------------------------------------------------------------- |
| **Effort Parameter**   | Use `low` for quick queries, `medium` for daily work, `high` for complex analysis |
| **Context Compaction** | Long conversations automatically maintain context—don't restart for follow-ups    |
| **Document Upload**    | Upload copybooks, specs, and code files for analysis                              |
| **Projects**           | Create dedicated projects for major modernization efforts                         |

#### Optimal Prompt Patterns

**For Architecture Analysis:**

```
[Upload: ORDPROC.cob, CP-ORDHDR.cpy, CP-ORDDETL.cpy]

Act as a senior architect reviewing this COBOL order processing system.

Using high effort reasoning:
1. Map the program structure and control flow
2. Identify all data transformations between input and output
3. Document dependencies on external systems (databases, files, APIs)
4. Highlight potential modernization opportunities
5. Create a text-based architecture diagram

Consider our Fujitsu COBOL environment and batch processing patterns.
```

**For Research Questions:**

```
What are best practices for error handling in COBOL-2014 when working with
indexed files? Include:
- Standard error codes to check
- Recovery strategies
- Logging approaches
- How this applies to Fujitsu NetCOBOL specifically
```

**Template: COBOL Analysis with ChatGPT Pro 5.2**

```
[Upload relevant files or paste code]

Effort: high (for deep analysis) / medium (for standard review)

Role: Senior COBOL architect with 20+ years of enterprise experience

Analyze this [program/module/system] and provide:

1. **Executive Summary** (2-3 sentences for non-technical stakeholders)
2. **Technical Analysis**
   - Program flow and structure
   - Key business logic (numbered list)
   - Data dependencies (copybooks, files, databases)
3. **Modernization Assessment**
   - Code quality rating (1-10)
   - Technical debt items
   - Refactoring priorities
4. **Recommendations** (prioritized action items)

Context: [Describe your specific situation, constraints, or goals]
```

### Codex 5.2 Prompts

**Best for:** Code generation, file editing, batch operations, automated refactoring

Codex 5.2 with GPT-5.2-Codex is your coding specialist. Use it for hands-on implementation work.

#### Key Features to Leverage

| Feature                | How to Use                                                               |
| ---------------------- | ------------------------------------------------------------------------ |
| **Approval Modes**     | Use `/approvals read-only` for planning, `/approvals auto` for execution |
| **Skills System**      | Create SKILL.toml files for repeatable tasks                             |
| **CLI Commands**       | Direct file operations and shell command execution                       |
| **Context Compaction** | Use `/compact` in long sessions                                          |

#### Optimal Prompt Patterns

**For Code Generation:**

```
Create a new paragraph VALIDATE-CUSTOMER-CREDIT that:
1. Checks WS-CREDIT-LIMIT from CP-CUSTHDR.cpy
2. Validates against WS-ORDER-TOTAL
3. Sets 88-level flags: CREDIT-OK, CREDIT-EXCEEDED, CREDIT-REVIEW-NEEDED
4. Returns appropriate status in WS-VALIDATION-RESULT

Follow our error handling pattern using ERR-2xxx codes for credit errors.
Include comments per our documentation standards.
```

**For Batch Refactoring:**

```
/approvals read-only

Scan all .cob files in /src for:
1. Nested IF statements deeper than 3 levels
2. GOTO statements (flag for review)
3. Missing error handling on file operations

Generate a report with:
- File name and line numbers
- Severity (High/Medium/Low)
- Suggested refactoring approach
```

**Template: COBOL to Modern Pattern Conversion**

```
Convert the following COBOL code pattern to [target pattern]:

Original Code:
[Paste COBOL code block]

Target Pattern: [e.g., "structured exception handling", "EVALUATE instead of nested IF"]

Requirements:
- Preserve exact business logic and behavior
- Use COBOL-2014 compatible syntax
- Follow naming convention: [your pattern]
- Include before/after comparison
- Add comments explaining the transformation

Copybook dependencies: [List relevant copybooks]
```

### Augment AI/Auggie Prompts

**Best for:** Codebase-wide context, IDE-integrated workflows, semantic search, code review

Augment AI's Context Engine provides industry-leading codebase understanding. Use it for cross-file analysis and IDE-integrated work.

#### Key Features to Leverage

| Feature             | How to Use                                                   |
| ------------------- | ------------------------------------------------------------ |
| **Context Engine**  | Automatically understands relationships across your codebase |
| **@Mentions**       | Use @filename for precise file targeting                     |
| **Task Lists**      | Break complex work into tracked steps                        |
| **Semantic Search** | `auggie search "error handling for file I/O"`                |
| **Memories**        | Persistent context across sessions                           |

#### Optimal Prompt Patterns

**For Cross-File Analysis:**

```
@CUSTMAINT.cob @ORDPROC.cob @CP-CUSTHDR.cpy

Analyze how WS-CUSTOMER-RECORD flows between these programs:
1. Where is it first populated?
2. What transformations occur in each program?
3. Which paragraphs modify specific fields?
4. Are there any inconsistencies in how it's handled?

Map the complete data journey from input to output.
```

**For IDE-Integrated Refactoring:**

```
@PROJECT_MEMORY.md

Refactor the error handling in the current file to use our standard pattern.

Create a task list:
1. Identify all file operations without error checking
2. Add appropriate FILE-STATUS checks
3. Implement our ERR-xxxx logging format
4. Add graceful recovery where possible
5. Update error counters

Execute step by step, pausing after each for my review.
```

**Template: Legacy Code Refactoring with Augment**

```
@[source-file]
@[related-copybooks]
@PROJECT_MEMORY.md

Refactor [specific section/paragraph] to improve [goal: readability/performance/maintainability].

Context Engine Query: Find all callers of this code and usages of [specific variable].

Requirements:
- Zero behavior change (preserve all business logic)
- Follow our coding standards from PROJECT_MEMORY
- Update all affected files consistently
- Add/update documentation comments

Break into tasks and show progress. Pause before making changes to files
other than [primary-file] for my approval.
```

### Claude Code Prompts

**Best for:** Complex reasoning, multi-file edits, planning, long-horizon refactoring

Claude Code with Claude Opus 4.5 excels at autonomous, long-running coding sessions and complex reasoning tasks.

#### Key Features to Leverage

| Feature                 | How to Use                                        |
| ----------------------- | ------------------------------------------------- |
| **Plan Mode**           | `claude --plan "task"` generates editable plan.md |
| **Effort Parameter**    | `--effort xhigh` for complex legacy analysis      |
| **Session Persistence** | `claude --resume` continues previous work         |
| **Context Compaction**  | `/compact` optimizes long sessions                |
| **GitHub Integration**  | Automated PR reviews and descriptions             |

#### Optimal Prompt Patterns

**For Complex Planning:**

```bash
claude --plan "Refactor the inventory management module to use modern COBOL patterns"
```

This generates a `plan.md` file you can review and edit before execution:

```
# Review and edit this plan before running:
# claude --execute plan.md
```

**For Multi-File Refactoring:**

```
Refactor error handling across the ORDER-PROCESSING system.

Analysis scope: ORDPROC.cob, ORDVALID.cob, ORDINV.cob, and all related copybooks

Requirements:
1. Identify all current error handling patterns
2. Design a unified approach using our ERR-xxxx standard
3. Implement consistently across all files
4. Maintain a log of all changes for CHANGE_HISTORY.md

Use xhigh effort for thorough analysis before making changes.
Pause after the design phase for my review.
```

**Template: COBOL Modernization Planning with Claude Code**

```
claude --effort xhigh --plan "Modernize [module-name]"

COBOL Modernization Plan Request

Target: [Program/module name]
Files: [List primary files]
Copybooks: [List dependencies]

Goals:
1. [Primary goal, e.g., "Improve maintainability"]
2. [Secondary goal, e.g., "Add comprehensive error handling"]
3. [Tertiary goal, e.g., "Update to COBOL-2014 syntax"]

Constraints:
- Must maintain 100% backward compatibility
- Preserve all existing interfaces
- No changes to file layouts or copybook structures
- Follow standards in PROJECT_MEMORY.md

Generate a detailed plan with:
- Phase breakdown (Analysis → Design → Implementation → Testing)
- Risk assessment for each phase
- Estimated effort per step
- Rollback strategy
- Validation criteria

Save as plan.md for review before execution.
```

### Cursor Prompts

**Best for:** Real-time code assistance, inline completions, interactive editing, quick refactoring

Cursor provides fast, contextual assistance directly in your IDE. Use it for hands-on coding work.

#### Key Features to Leverage

| Feature           | How to Use                                 |
| ----------------- | ------------------------------------------ |
| **@ References**  | @filename, @folder, @codebase for context  |
| **Cmd+K**         | Quick inline edits and generations         |
| **Composer Mode** | Multi-file edits in a single session       |
| **Rules Files**   | .cursorrules for project-specific guidance |

#### Optimal Prompt Patterns

**For Quick Analysis (Cmd+K):**

```
Explain what this paragraph does and identify any potential issues
```

**For Inline Editing (Cmd+K on selection):**

```
Refactor this nested IF to use EVALUATE. Preserve exact behavior.
```

**For Composer Multi-File Edits:**

```
@CUSTMAINT.cob
@CP-CUSTHDR.cpy
@PROJECT_MEMORY.md

Add a new field WS-CUSTOMER-STATUS to CP-CUSTHDR.cpy and update
CUSTMAINT.cob to:
1. Initialize the field in INIT-CUSTOMER paragraph
2. Validate it in VALIDATE-CUSTOMER paragraph
3. Display it in DISPLAY-CUSTOMER paragraph

Follow our coding standards. Show all file changes together.
```

**Template: COBOL Code Review with Cursor**

```
@[file-to-review]
@PROJECT_MEMORY.md

Code Review Request

Review this code for:

**Correctness:**
- Logic errors or bugs
- Edge case handling
- Business rule compliance

**Quality:**
- Adherence to our coding standards
- Naming conventions
- Comment adequacy
- Code structure

**Performance:**
- Inefficient patterns
- Unnecessary I/O operations
- Optimization opportunities

**Error Handling:**
- Missing error checks
- Incomplete recovery logic
- Logging adequacy

Provide findings as:
| Line | Issue | Severity | Recommendation |
|------|-------|----------|----------------|

Focus on actionable improvements, not style nitpicks.
```

---

## Prompt Templates Library

Copy and adapt these battle-tested templates for common COBOL modernization tasks.

### Template 1: COBOL Analysis Template

```
@[program.cob]
@[related-copybooks]

Comprehensive COBOL Program Analysis

Provide a complete analysis including:

**1. Program Overview**
- Purpose and business function
- Main inputs and outputs
- Processing summary

**2. Structure Analysis**
- Division breakdown
- Paragraph inventory with purposes
- Control flow diagram (text-based)

**3. Data Analysis**
- Working-Storage key variables
- File definitions and record layouts
- Copybook dependencies

**4. Logic Analysis**
- Core business rules implemented
- Calculation logic
- Validation rules

**5. Quality Assessment**
- Code quality score (1-10)
- Technical debt items
- Modernization candidates

**6. Recommendations**
- Priority improvements
- Risk areas
- Refactoring suggestions

Context: [Your specific goals or concerns]
```

### Template 2: Code Conversion Template

````
Convert this COBOL code to [target: structured COBOL-2014 / pseudo-code / documentation]:

**Source Code:**
```cobol
[Paste COBOL code here]
```

**Conversion Requirements:**
- Preserve exact business logic
- Maintain all edge case handling
- Document any assumptions made
- Flag any ambiguous logic for human review

**Output Format:**
1. Converted code with inline comments
2. Mapping table: original line → converted section
3. List of any behavior changes (should be empty)
4. Verification checklist

**Context:**
- Source dialect: [Fujitsu NetCOBOL / IBM Enterprise COBOL / etc.]
- Target: [COBOL-2014 / documentation / test cases]
- Constraints: [Any specific requirements]
````

### Template 3: Documentation Generation Template

```
@[file-to-document]

Generate Technical Documentation

Create comprehensive documentation for this COBOL program.

**Document Sections:**

1. **Header**
   - Program name and ID
   - Last modified date
   - Author/maintainer
   - Version history summary

2. **Purpose**
   - Business function
   - When/why it runs
   - Key stakeholders

3. **Interfaces**
   - Input files (format, source, frequency)
   - Output files (format, destination, consumers)
   - Database interactions
   - Called programs
   - Calling programs

4. **Processing Logic**
   - Main processing flow (numbered steps)
   - Key business rules
   - Error handling approach

5. **Data Structures**
   - Key working-storage items
   - Copybook usage
   - Record layouts

6. **Operational Notes**
   - Dependencies
   - Known limitations
   - Performance considerations
   - Troubleshooting tips

**Format:** Markdown suitable for documentation site
```

### Template 4: Test Case Generation Template

```
@[program-to-test]
@[copybooks]

Generate Test Cases

Create comprehensive test cases for [specific function/paragraph].

**Test Scope:** [Describe what to test]

**Generate These Test Categories:**

1. **Happy Path Tests** (valid inputs, expected flow)
   - Typical use case scenarios
   - Standard business transactions

2. **Boundary Tests** (edge values)
   - Minimum values
   - Maximum values
   - Exact boundary values

3. **Error Condition Tests** (invalid inputs)
   - Missing required data
   - Invalid formats
   - Out-of-range values
   - Null/empty handling

4. **Integration Tests** (component interactions)
   - File I/O scenarios
   - Database interactions
   - Cross-program calls

**For Each Test Case Provide:**
| ID | Name | Description | Input Data | Expected Output | Preconditions |
|----|------|-------------|------------|-----------------|---------------|

**Additional Requirements:**
- Include test data examples
- Note any required setup/teardown
- Flag tests requiring special environment
```

### Template 5: Bug Investigation Template

```
@[file-with-bug]
@[error-logs-or-symptoms]
@PROJECT_MEMORY.md

Bug Investigation Request

**Symptoms:**
[Describe what's happening]

**Expected Behavior:**
[Describe what should happen]

**Actual Behavior:**
[Describe the incorrect behavior]

**Reproduction Steps:**
1. [Step 1]
2. [Step 2]
3. [Step 3]

**Environment:**
- [Compiler version, OS, relevant configuration]

**Investigation Request:**

1. **Root Cause Analysis**
   - What is causing this behavior?
   - Show the specific code causing the issue
   - Explain the logic error

2. **Impact Assessment**
   - What else might be affected?
   - Are there related issues?
   - Data integrity concerns?

3. **Fix Recommendation**
   - Proposed code change (diff format)
   - Explanation of why this fixes it
   - Any side effects to consider

4. **Prevention**
   - How could this have been prevented?
   - Suggested tests to add
   - Code review checklist items

Reference our error handling patterns from PROJECT_MEMORY.md.
```

### Template 6: Code Review Template

```
Code Review: [Feature/Change Name]

@[modified-files]
@SPEC-[feature].md (if applicable)
@PROJECT_MEMORY.md

**Review Perspective:** I did not write this code. Review objectively.

**Review Checklist:**

□ **Specification Compliance**
  - Does it meet all requirements in the spec?
  - Are acceptance criteria satisfied?

□ **Correctness**
  - Logic errors or bugs?
  - Edge cases handled?
  - Calculations accurate?

□ **Code Quality**
  - Follows our coding standards?
  - Naming clear and consistent?
  - Comments adequate?
  - Appropriate abstraction level?

□ **Error Handling**
  - All error conditions handled?
  - Using our ERR-xxxx format?
  - Proper logging?
  - Recovery/rollback logic?

□ **Performance**
  - Efficient algorithms?
  - Minimal I/O operations?
  - No unnecessary loops?

□ **Maintainability**
  - Easy to understand?
  - Easy to modify?
  - Properly modular?

□ **Security**
  - Input validation adequate?
  - Sensitive data protected?
  - No information leakage in errors?

**Output Format:**
| File:Line | Finding | Severity | Recommendation |
|-----------|---------|----------|----------------|

Severity: Critical / High / Medium / Low / Suggestion
```

## Common Patterns That Work

These proven patterns consistently produce high-quality results across all AI tools.

### Role-Based Prompting

Set the AI's expertise level to match your needs:

```
Act as a senior COBOL developer with 20+ years of experience in Fujitsu COBOL
and enterprise batch processing systems. You understand copybook structures,
file I/O patterns, and legacy modernization challenges.
```

```
Act as a code reviewer who has never seen this code before. Review critically
and objectively, flagging anything that seems unclear or potentially problematic.
```

```
Act as a technical writer creating documentation for developers who will
maintain this code after you're gone.
```

### Step-by-Step Analysis

Request structured reasoning for complex tasks:

```
Step by step, analyze how WS-ORDER-TOTAL is calculated:
1. First, identify where the variable is defined
2. Then, find all statements that modify it
3. Next, trace the data sources for each modification
4. Finally, summarize the complete calculation logic
```

### Copybook Context Pattern

Always include relevant copybooks for accurate analysis:

```
Given this COBOL copybook (CP-CUSTHDR.cpy):
[Paste copybook content]

And this program section that uses it:
[Paste program section]

Explain how the CUSTOMER-RECORD structure is populated and validated.
```

### Before/After Pattern

Request explicit comparisons when refactoring:

```
Refactor the VALIDATE-INPUT paragraph to use EVALUATE instead of nested IF.

Show:
1. BEFORE: The original code (unchanged)
2. AFTER: The refactored code
3. ANALYSIS: Confirm that behavior is identical
4. BENEFITS: What improvements this provides
```

### Constraint-First Pattern

Lead with constraints to prevent unwanted changes:

```
CONSTRAINTS (must follow exactly):
- Zero behavior changes
- COBOL-85 compatible syntax only
- Preserve all existing error codes
- Maintain file record formats

TASK:
Improve the readability of the PROCESS-ORDERS paragraph by adding comments
and improving variable names where possible without changing functionality.
```

### Incremental Complexity Pattern

Start simple, then add complexity:

```
[Prompt 1] List all paragraphs in INVMAINT.cob with one-line descriptions.

[Prompt 2] For the CALC-INVENTORY-VALUE paragraph, provide detailed
documentation of the business logic.

[Prompt 3] Now identify potential bugs or edge cases in that paragraph.

[Prompt 4] Suggest improvements, ranked by risk and impact.
```

### Reference File Pattern

Point to standards and examples:

```
@PROJECT_MEMORY.md
@examples/STANDARD-ERROR-HANDLING.cob

Following the error handling pattern shown in our standard example,
add error handling to the FILE-OPERATIONS section of CUSTMAINT.cob.
```

### Generate Unit Tests Pattern

```
Generate unit tests that cover:
1. Normal operation with valid inputs
2. Boundary conditions (min/max values)
3. Error conditions (invalid data, missing files)
4. Edge cases specific to this business logic

For each test, provide:
- Test case ID
- Description
- Input data
- Expected output
- Pass/fail criteria
```

---

## Anti-Patterns to Avoid

These common mistakes lead to poor results. Learn to recognize and avoid them.

### ❌ Vague Prompts Without Context

**Bad:**

```
Fix the bug
```

```
Make this better
```

```
Analyze the code
```

**Why it fails:** The AI doesn't know what bug, what "better" means, or what to focus on. You'll get generic, unhelpful responses.

**Better:**

```
In INVMAINT.cob at line 347, the CALC-DISCOUNT paragraph incorrectly
returns 0 when WS-QUANTITY is exactly 100. Expected: 10% discount.
Investigate and fix.
```

### ❌ Information Overload

**Bad:**

```
Here's our entire 5,000 line program, 12 copybooks, the database schema,
our coding standards, and the full requirements document. Please review
everything and tell me what to improve.
```

**Why it fails:** Too much information drowns the signal. The AI can't prioritize or focus effectively.

**Better:**

```
Review the ORDER-VALIDATION section (lines 400-500) of ORDPROC.cob for
error handling gaps. Focus specifically on file I/O operations.

Relevant copybook: CP-ORDHDR.cpy
Our error handling standard: ERR-xxxx format with AUDIT-FILE logging
```

### ❌ Not Iterating on Responses

**Bad:**

```
[Prompt] Generate complete documentation for INVMAINT.cob

[AI Response - not quite right]

[User gives up and writes it manually]
```

**Why it fails:** First responses are rarely perfect. The real value comes from iteration.

**Better:**

```
[Prompt 1] Generate documentation outline for INVMAINT.cob

[Response] Here's the outline...

[Prompt 2] Good structure. Expand section 3 (Business Logic) with more detail
on the discount calculations.

[Response] Here's the expanded section...

[Prompt 3] The discount logic explanation is missing the volume discount tier.
Add that and also include the interaction with WS-CUSTOMER-TYPE.
```

### ❌ Ignoring Tool-Specific Strengths

**Bad:**
Using ChatGPT for rapid inline code edits (use Cursor instead)
Using Cursor for architectural research (use ChatGPT instead)
Using any tool for everything without considering strengths

**Why it fails:** Each tool has unique strengths. Mismatched tools reduce quality and efficiency.

**Tool Selection Guide:**

| Task                                      | Best Tool       |
| ----------------------------------------- | --------------- |
| Architecture analysis, research, planning | ChatGPT Pro 5.2 |
| Code generation, batch operations         | Codex 5.2       |
| Cross-file understanding, IDE workflows   | Augment AI      |
| Complex planning, multi-file refactoring  | Claude Code     |
| Quick edits, inline completions           | Cursor          |

### ❌ Not Providing Business Context

**Bad:**

```
What does line 450 do?
```

**Why it fails:** Without context, you get literal code explanation without understanding.

**Better:**

```
Line 450 in ORDPROC.cob modifies WS-DISCOUNT-AMOUNT. This is part of our
order pricing logic for wholesale customers.

Explain how this line fits into the overall discount calculation flow
and whether it correctly handles the case where WS-QUANTITY is zero.
```

### ❌ Assuming AI Knows Your Project

**Bad:**

```
Use the standard pattern
```

```
Follow our conventions
```

**Why it fails:** AI doesn't know your standards unless you tell it.

**Better:**

```
@PROJECT_MEMORY.md

Use our standard error handling pattern documented in PROJECT_MEMORY.md.
Specifically, apply the pattern from the "Error Handling" section that uses
ERR-xxxx codes and AUDIT-FILE logging.
```

### ❌ Not Validating AI Output

**Bad:**

```
[AI generates code]

[Developer commits directly without review]
```

**Why it fails:** AI makes mistakes. Always validate, especially for:

- Business logic correctness
- Edge case handling
- Security implications
- Performance impact

**Better:**

```
[AI generates code]

[Developer reviews code manually]
[Developer runs existing tests]
[Developer adds new tests for the change]
[Developer validates in test environment]
[Developer commits with proper documentation]
```

---

## Next Steps

- Apply these principles to your next prompting session
- Create a personal library of effective prompts for your common tasks
- Share successful prompts with your team via PROJECT_MEMORY.md
- Review the [Prompt Library](./prompt-library.md) for additional templates
- Set up your preferred tools using our tool-specific guides:
  - [ChatGPT Pro 5.2](./chatgpt.md)
  - [Codex 5.2](./codex.md)
  - [Augment AI](./augment-ai.md)
  - [Claude Code](./claude-code.md)
  - [Cursor](./cursor.md)
