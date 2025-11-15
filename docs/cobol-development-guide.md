---
sidebar_position: 9
tags: [cobol, fujitsu, netcobol, development, workflows]
---

# COBOL Development with AI

Practical guide for AI-assisted COBOL development using Fujitsu NetCOBOL, Visual Studio 2015, and our established tooling (Cursor, ChatGPT, Issue Maintenance Tool).

## Overview

This guide adapts AI-assisted development specifically for our COBOL environment:

- **Fujitsu NetCOBOL** - Primary COBOL compiler with .NET integration
- **Visual Studio 2015** - IDE with Fujitsu licenses
- **SP2 Panels** - UI framework for COBOL programs
- **Copybooks** - Shared data structures (FD/SL/NM/LK/WS)
- **Tortoise SVN** - Version control
- **Issue Maintenance Tool** - Work tracking

## Repository Structure Quick Reference

Understanding where things live is critical for effective AI prompts.

### Source Code Locations

| Directory      | Purpose                                         | AI Usage                                  |
| -------------- | ----------------------------------------------- | ----------------------------------------- |
| `fujsource/`   | Primary Fujitsu NetCOBOL library (~2K programs) | Main development target                   |
| `fujsource64/` | 64-bit builds mirroring fujsource               | x64 runtime development                   |
| `source/`      | Legacy NetCOBOL mirror                          | Modernization comparisons                 |
| `anssource/`   | ANSI-era COBOL programs                         | Regression analysis, historical reference |

### Copybook Locations

| Directory    | Purpose                 | Files                              |
| ------------ | ----------------------- | ---------------------------------- |
| `fujcopy/`   | Modern copybook catalog | FD/SL/NM/LK/SP2/REP/EO/EIO/WS      |
| `fujcopy64/` | x64-specific copybooks  | 64-bit numeric storage definitions |
| `copy/`      | Legacy copybooks        | Historical record layouts          |

### Project & Build Files

| Directory      | Purpose                      | Key Files                             |
| -------------- | ---------------------------- | ------------------------------------- |
| `fujprojects/` | 300+ Visual Studio projects  | `.cobproj`, `.sln`, `listing.txt`     |
| `Projects/`    | Active conversion workspaces | Runnable projects with `bin/`, `obj/` |
| `fujitsu/`     | Compiler configurations      | `stddebug.cfg`, `stdcomp64.cfg`       |

### UI & Panels

| Directory    | Purpose                      | Files                  |
| ------------ | ---------------------------- | ---------------------- |
| `panels/`    | SP2 `.PNL` screen sources    | 1:1 with UI programs   |
| `sp2panels/` | Folderized panel definitions | Modern panel structure |

### Support Files

| Directory               | Purpose               | Usage                      |
| ----------------------- | --------------------- | -------------------------- |
| `nonfujprojects/`       | .NET helper libraries | P/Invoke wrappers, caching |
| `distributables/`       | Deployment payloads   | Seed data, runtime DLLs    |
| `distributables/files/` | Test datasets         | FUJPM, USSTE for QA        |

## Scale & Metrics

Knowing the codebase size helps set AI context window expectations:

- **8,267** `.CBL` programs across all source folders
- **3,470** `.FD` copybooks + **474** `.CPY` files
- **5,743** `.SP2` panel files
- **325** `.cobproj` files and **324** `.sln` files
- Hundreds of binary assets (icons, DLLs, datasets)

**AI Implication**: Always be selective about which files to include in prompts. Start with the specific program and its direct dependencies.

## COBOL-Specific AI Workflow

### Preparation Checklist

Before starting any COBOL task with AI, gather:

1. **Target Program** - From `fujsource/<PROGRAM>.CBL`
2. **Copybooks** - All `COPY` statements from the program
   - Search: `rg "COPY \"(.+)\"" fujsource/<PROGRAM>.CBL`
3. **Panel Definition** - If UI program:
   - `panels/<PROGRAM>.PNL`
   - `sp2panels/<Name>Panel/`
4. **Compiler Output** - Recent `listing.txt` from:
   - `Projects/<PROGRAM>/<PROGRAM>/listing.txt`
   - `fujprojects/<PROGRAM>/<PROGRAM>/listing.txt`
5. **Legacy Comparison** (if modernization):
   - `source/<PROGRAM>.CBL` or `anssource/<PROGRAM>.CBL`
6. **External Dependencies** - If program uses `CALL`:
   - Check `nonfujprojects/` for C# wrappers

### Why This Matters

**Copybook Churn**: COBOL programs share copybooks. Without explicit copybook context, AI will hallucinate field definitions.

**Compiler Specifics**: Fujitsu NetCOBOL has unique `@OPTIONS` and syntax. Including `listing.txt` grounds AI in actual compiler behavior.

**.NET Integration**: Many programs call .NET assemblies. Context from `nonfujprojects/` prevents invalid CALL syntax.

**UI Mapping**: SP2 panels exist as both `.PNL` and `.SP2`. Providing both lets AI map screen fields to WORKING-STORAGE.

## Common COBOL Workflows with AI

### Workflow 1: Writing Specifications

**Goal**: Create a spec document for a program before making changes.

**Steps**:

1. **Gather Context** (see Preparation Checklist above)

2. **Create Spec File** in your WIP folder:

   ```
   WIP/Issue32527/Specs/SPEC_AP0043GI.md
   ```

3. **Use This Prompt with ChatGPT**:

   ````markdown
   You are documenting Fujitsu NetCOBOL module AP0043GI for Global Shop Solutions.

   Context files:

   - Program: ```cobol
     [Paste fujsource/AP0043GI.CBL]
   ````

   - Copybooks: ```cobol
     [Paste OPHDR.FD, COMMONSN.CPY, etc.]

   ````

   - Panel: ```cobol
     [Paste panels/AP0043GI.PNL]

   ````

   - Compiler Output: ```
     [Paste relevant listing.txt warnings/notes]

   ```

   Tasks:

   1. Summarize the business goal and what triggers this program
   2. List every file READ/WRITE with copybook field names
   3. Describe UI fields and validation from SP2 panel
   4. Provide test data setup (reference datasets in distributables/files)
   5. Flag any missing copybooks or unclear dependencies

   Format as Markdown with sections:

   - Purpose
   - Data Sources
   - UI Notes
   - Acceptance Criteria
   - Open Questions
   ```

4. **Save AI Response** to `Specs/SPEC_AP0043GI.md`

5. **Review & Refine** - Check field names, copybook references, file paths

**Cursor Alternative**:

```
@explain Create a spec for AP0043GI using the open files (CBL, copybooks, panel,
listing.txt). Detail inputs/outputs, PERFORM blocks, SP2 interactions, and test data.
Return Markdown with: Purpose, Data Sources, UI Notes, Acceptance.
```

### Workflow 2: Bug Triage & Fixes

**Goal**: Diagnose and fix a reported bug in COBOL code.

**Steps**:

1. **Reproduce in Issue Maintenance Tool**
   - Open the issue
   - Note symptoms, error messages, transaction data

2. **Search for Root Cause**:

   ```bash
   # Find program
   rg "PROGRAM-ID. AP0043GI" fujsource/ -n

   # Find all programs using suspect copybook
   rg "COPY \"OPHDR\"" fujsource/ -n
   ```

3. **Compare Fujitsu vs Legacy** (if regression suspected):
   - Open `fujsource/AP0043GI.CBL`
   - Open `source/AP0043GI.CBL` or `anssource/AP0043GI.CBL`

4. **Use This Prompt**:

   ````markdown
   A defect was reported in AP0043GI. Symptoms:
   [Describe: wrong calculation, null value, screen freeze, etc.]

   Compiler output shows:

   ```
   [Paste listing.txt error or warning]
   ```

   Context files:

   - Fujitsu version: ```cobol
     [Paste fujsource excerpt with suspect PERFORM or SECTION]
   ````

   - Legacy version (for comparison): ```cobol
     [Paste source/ or anssource/ equivalent if relevant]

   ````

   - Copybooks: ```cobol
     [Paste affected FD/SL/NM definitions]

   ```

   Tasks:
   1. Identify the failing paragraph and field(s) involved
   2. Compare Fujitsu vs legacy implementations (what changed?)
   3. Recommend minimal code/copybook fix
   4. List other programs using same copybooks (impact analysis)
   5. Outline rebuild steps (which .cobproj to compile)
   ```
   ````

5. **Apply Fix in Cursor**:
   - Open program in Cursor
   - Use inline chat: `@fix Apply the suggested patch while keeping Fujitsu syntax`
   - Review diff before applying

6. **Rebuild & Verify**:

   ```bash
   msbuild Projects/AP0043GI/AP0043GI/AP0043GI.cobproj /t:Build
   ```

7. **Check New `listing.txt`** for warnings

8. **Update Issue Maintenance Tool** - Add modified files via drag & drop

**Common COBOL Bug Patterns**:

- **Null/Spaces in Numeric Fields** - Check `PIC 9` vs `PIC X` copybook definitions
- **Boundary Conditions** - `PERFORM UNTIL` vs `PERFORM VARYING` off-by-one
- **SP2 Field Validation** - Panel field not matching WORKING-STORAGE
- **Copybook Mismatch** - Program using old version, needs recompile

### Workflow 3: Custom Customer Requests

**Goal**: Implement new functionality requested by customer.

**Steps**:

1. **Capture Requirement** in Issue Maintenance Tool
   - Customer Facing Description
   - Internal Notes
   - Related copybooks/panels

2. **Map to Codebase**:

   ```bash
   # Find programs that touch OPHDR file
   rg "SELECT OPHDR" fujsource/ -n

   # Find UI programs
   rg "CALL.*SP2PANEL" fujsource/ -A 5
   ```

3. **Use This Prompt for Impact Analysis**:

   ````markdown
   Customer request: [Paste exact requirement]

   Priority: [High/Medium/Low]
   Target release: 2019.1

   Relevant assets:

   - Programs: ```cobol
     [Paste candidate program excerpts]
   ````

   - Copybooks: ```cobol
     [Paste OPHDR.FD, related definitions]

   ````

   - Panels: ```cobol
     [Paste AP0043GI.PNL or relevant UI]

   ````

   - .NET Libraries (if applicable): ```csharp
     [Paste nonfujprojects/ snippets if CALL exists]

   ```

   Produce:

   1. Impact analysis (data files, UI, external interfaces, reports)
   2. Ordered implementation steps (specific file changes)
   3. Risks/unknowns + questions for customer
   4. Validation plan using distributables/files datasets
   ```

4. **Create Implementation Plan** - Save to `Plans/PLAN_Issue32527.md`

5. **Implement in Cursor**:

   ```
   @plan Implement [feature] using pinned files (CBL, copybooks, panel, C# helper).
   Build multi-step plan: analyze current behavior, identify touchpoints, design
   data/UI changes, describe validation. Reference exact file paths.
   ```

6. **Validate Changes**:
   - Test with datasets from `distributables/files/`
   - Verify SP2 panel behavior in clinic
   - Run impacted programs to check ripple effects

### Workflow 4: Daily Throughput Accelerators

**Goal**: Speed up routine development tasks.

**Batch Documentation**:

```markdown
Create program summaries for the following critical modules:

- AP0043GI (Accounts Payable)
- SYS080 (System Configuration)
- ORD147 (Order Entry)

For each program:

1. Scan fujsource/<ID>.CBL and fujcopy/ copybooks
2. Summarize business purpose and data flow
3. List all COPY statements with file types (FD/SL/NM)
4. Note .NET CALLs if any
5. Export as Markdown in docs/cobol-programs/

Run this for all 3 programs and save outputs for team reference.
```

**Automated Checklists in Cursor**:

Create reusable Composer template:

```
@tasks Generate Composer template for COBOL work:
1. Gather program + copybooks from fujsource/fujcopy
2. Pull listing.txt from Projects/<id>/<id>/
3. Run msbuild and capture output
4. Summarize changes + required tests
Output the task definition to save in Cursor settings.
```

**Prompt Library Maintenance**:

Keep commonly used prompts in WIP folder:

```
WIP/Templates/
  ├── spec-template.md
  ├── bug-fix-template.md
  ├── feature-request-template.md
  └── daily-checklist-template.md
```

## COBOL-Specific AI Prompting Strategies

### Context Bundle Requirements

For **any** COBOL AI task, minimum context includes:

1. **Program Source** - The specific `.CBL` file
2. **Copybooks** - All `COPY` statements resolved
3. **Compiler Configuration** - `@OPTIONS` header from program
4. **Build Output** - Recent `listing.txt` for warnings

**Optional but helpful**:

- Panel definition (if UI program)
- Legacy version (if modernization task)
- External dependencies (if program uses `CALL`)
- Test datasets (from `distributables/files/`)

### Syntax Preservation

**Always remind AI**:

```
Keep Fujitsu NetCOBOL syntax including:
- @OPTIONS header directives
- Fujitsu-specific intrinsics
- SP2 panel CALL conventions
- Repository bindings format
```

**Example**:

```
WRONG (Generic COBOL):
CALL 'PANEL' USING WS-FIELD.

RIGHT (Fujitsu NetCOBOL):
CALL 'SP2PANEL' USING BY REFERENCE PANEL-BLOCK
                       BY VALUE PANEL-FUNCTION
```

### Copybook-First Approach

When asking about data structures:

```markdown
Before analyzing this program, first review these copybooks:

- OPHDR.FD (file definition)
- OPHDR.SL (select clause)
- OPHDR-REC.NM (record layout)

Then identify which fields in the program map to which copybook structures.
```

This prevents AI from inventing field names.

### Listing.txt as Ground Truth

When debugging compilation errors:

```markdown
The compiler reports:
[Paste listing.txt error]

Given this actual Fujitsu compiler output, explain:

1. What line is failing
2. What the copybook expects vs what the code provides
3. How to fix while staying COBOL-85 compliant
```

## Guardrails & Best Practices

### Data Hygiene

**Always**:

- Use curated datasets from `distributables/files/` in prompts
- Sanitize customer data before feeding to AI
- Keep AI-generated specs/code within the repo for review

**Never**:

- Paste production data with customer info
- Share proprietary algorithms without sanitization
- Commit AI changes without human review

### Version Control

**Which flavor are you editing?**

- Fujitsu (`fujsource/`) - Primary development
- Legacy (`source/`, `anssource/`) - Reference only, don't edit
- 64-bit (`fujsource64/`, `fujcopy64/`) - Separate build target

**Avoid cross-contamination**:

```bash
# WRONG - editing legacy by mistake
vim source/AP0043GI.CBL

# RIGHT - edit Fujitsu version
vim fujsource/AP0043GI.CBL
```

### Compiler Artifacts

Treat `listing.txt` as source code:

- Archive key logs with the program
- Reference in Issue Maintenance Tool notes
- Include in AI context for diagnostics

**File paths for reference**:

```bash
Projects/AP0043GI/AP0043GI/listing.txt
fujprojects/SYS080/SYS080/listing.txt
```

### AI Tool Selection

| Task                   | Use Cursor                      | Use ChatGPT                       |
| ---------------------- | ------------------------------- | --------------------------------- |
| Quick code edits       | ✅ Direct file integration      | ❌ Copy/paste heavy               |
| Copybook analysis      | ✅ Can reference multiple files | ✅ Good at explaining structures  |
| Spec writing           | ❌ Better for short docs        | ✅ Better for comprehensive specs |
| Debugging              | ✅ Can see full context         | ⚠️ Needs all context pasted       |
| Architecture decisions | ❌ Limited context window       | ✅ Better high-level thinking     |
| Build automation       | ✅ Terminal integration         | ❌ Can't run commands             |

## Integration with Issue Maintenance Tool

### Typical Workflow

1. **Create/Open Issue** in Issue Maintenance Tool
2. **Gather Context** using preparation checklist
3. **Create WIP Folder**:

   ```
   WIP/Issue32527/
     ├── Trunk/           # Checkout from SVN
     ├── Working/         # Your changes
     ├── Specs/          # Spec documents
     └── Plans/          # AI-generated plans
   ```

4. **Work with AI** (Cursor or ChatGPT)
5. **Drag & Drop Files** from `Working/` to Issue Maintenance Tool
6. **Send to Testing** - Tools > Send to Testing
7. **QA Approval** - Status changes to "Resolved"
8. **Commit to SVN** - Tools > Commit to SVN

### File Type Mapping

Issue Maintenance Tool auto-populates file **Type** based on history:

| Extension          | Typical Type      |
| ------------------ | ----------------- |
| `.CBL`             | Source            |
| `.FD`, `.CPY`      | Copybook          |
| `.PNL`, `.SP2`     | Panel             |
| `.cobproj`, `.sln` | Project           |
| `.dll`, `.exe`     | Bin or Bin\Client |

**Tip**: Check "Include in Bin Client" if DLL needs client deployment.

## Example: End-to-End COBOL Task

Let's walk through a complete example using AI.

### Scenario

**Issue #32527**: Customer reports AP payment calculation is off by \$0.01 in certain rounding scenarios.

**Program**: `AP0043GI` (Accounts Payable Entry)

### Step 1: Gather Context

```bash
# Find program
rg "PROGRAM-ID. AP0043GI" fujsource/ -n

# Find copybooks
rg "COPY \"" fujsource/AP0043GI.CBL

# Result:
# COPY "APHDR.FD"
# COPY "COMMONSN.CPY"
# COPY "APPYMT.NM"
```

**Files to collect**:

- `fujsource/AP0043GI.CBL`
- `fujcopy/APHDR.FD`
- `fujcopy/COMMONSN.CPY`
- `fujcopy/APPYMT.NM`
- `panels/AP0043GI.PNL`
- `Projects/AP0043GI/AP0043GI/listing.txt`

### Step 2: Create WIP Folder

```bash
mkdir -p WIP/Issue32527/{Trunk,Working,Specs,Plans,Review}

# Checkout from SVN
cd WIP/Issue32527/Trunk
# [Use TortoiseSVN to checkout needed files]

# Copy to Working
cp -r Trunk/* Working/
```

### Step 3: Write Spec with ChatGPT

**Prompt**:

````markdown
You are documenting Fujitsu NetCOBOL module AP0043GI for rounding bug investigation.

Context:

- Bug: Rounding error of $0.01 in certain payment calculations
- Program:

  ```cobol
  [Paste fujsource/AP0043GI.CBL - focus on calculation sections]
  ```

- Copybooks:

  ```cobol
  [Paste APHDR.FD, APPYMT.NM - payment and calculation fields]
  ```

- Panel:

  ```cobol
  [Paste AP0043GI.PNL - payment entry fields]
  ```

Tasks:

1. Identify all monetary calculation fields (PIC 9V99 COMP-3)
2. Find COMPUTE or ADD/SUBTRACT statements affecting payment totals
3. Check for potential rounding issues (division, multiplication)
4. Compare current rounding method vs expected (ROUNDED phrase usage)
5. Suggest test cases with specific dollar amounts to reproduce bug

Save as Markdown: Purpose, Calculation Fields, Suspect Code, Test Scenarios

```

```
````

Save response to `Specs/SPEC_AP0043GI_Rounding.md`

### Step 4: Implement Fix with Cursor

Open Cursor in `Working/` folder:

```bash
cursor WIP/Issue32527/Working
```

**Pin files in Cursor**:

- AP0043GI.CBL
- APHDR.FD
- APPYMT.NM
- AP0043GI.PNL

**Prompt in Cursor**:

```
@fix The COMPUTE statement on line 450 causes $0.01 rounding errors.
Spec indicates we should use ROUNDED phrase for all monetary calculations.

Apply fix to calculation section while:
1. Keeping Fujitsu NetCOBOL syntax
2. Adding ROUNDED to all PIC 9V99 COMP-3 calculations
3. Preserving existing business logic
4. Adding comment explaining the fix
```

Review diff and apply.

### Step 5: Rebuild & Test

```bash
cd WIP/Issue32527/Working

# Rebuild
msbuild Projects/AP0043GI/AP0043GI/AP0043GI.cobproj /t:Build

# Check listing.txt for warnings
cat Projects/AP0043GI/AP0043GI/listing.txt | grep -i warning
```

Test in clinic with scenarios from spec.

### Step 6: Code Review with Fresh AI

Open **new** ChatGPT session:

````markdown
Please review this COBOL fix for rounding bug.

Original Spec:
[Paste Specs/SPEC_AP0043GI_Rounding.md]

Changed Code:

```cobol
[Paste modified calculation section from AP0043GI.CBL]
```

Copybook context:

```cobol
[Paste APPYMT.NM field definitions]
```

Review for:

1. Correct use of ROUNDED phrase
2. Edge cases (negative amounts, zero)
3. Impact on other calculations
4. Fujitsu NetCOBOL compatibility
5. Missing error handling
````

Save review to `Review/REVIEW_Issue32527.md`

### Step 7: Update Issue Maintenance Tool

1. Open Issue #32527
2. Drag files from `Working/` to Files grid:
   - `AP0043GI.CBL` → Type: Source
   - `APPYMT.NM` → Type: Copybook (if modified)
3. Add Internal Notes: "Fixed rounding in payment calculation. Added ROUNDED phrase per spec."
4. Tools > Send to Testing

### Step 8: Commit After QA Approval

Once QA marks "Resolved":

1. Tools > Commit to SVN
2. Log message auto-populates: "32527 - Fix payment rounding error"
3. Status changes to "Released"

**Done!**

## Troubleshooting

### Issue: AI suggests non-Fujitsu syntax

**Solution**: Explicitly remind AI about Fujitsu specifics

```
This is Fujitsu NetCOBOL, not standard COBOL-85. Use:
- @OPTIONS directives
- SP2PANEL calling convention
- Fujitsu intrinsic functions
```

### Issue: AI invents copybook fields

**Solution**: Always include full copybook definitions in context

```
Before answering, review these copybook field definitions:
[Paste complete FD/NM/SL sections]

Only use fields that exist in these copybooks.
```

### Issue: Can't find the right program

**Solution**: Use Issue Maintenance Tool file history

1. Open any related issue
2. Check Files grid for similar programs
3. Note the Type (location) where files were found

### Issue: Build fails after AI changes

**Solution**: Check `listing.txt` and include in new prompt

```
Compiler errors after applying your suggestion:
[Paste listing.txt errors]

Please revise the fix to address these Fujitsu compiler messages.
```

## Next Steps

1. **Try on simple bug** - Pick an issue with clear symptoms
2. **Build your prompt library** - Save successful prompts to WIP/Templates/
3. **Document patterns** - When AI teaches you something, add it to Internal Notes
4. **Share with team** - Present workflow at standup or team meeting

---

**Remember**: AI is a pair programmer, not a replacement. Always review:

- ✅ Copybook field names match exactly
- ✅ Fujitsu syntax is preserved
- ✅ listing.txt shows no new warnings
- ✅ Test in clinic before sending to QA
- ✅ Impact analysis includes all programs using modified copybooks
