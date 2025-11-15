---
sidebar_position: 8
tags: [workflow, svn, issue-maintenance, best-practices]
---

# AI-Assisted Development with SVN

This guide provides a practical workflow for AI-assisted development using our actual environment: Visual Studio 2015, Tortoise SVN, and the Issue Maintenance Tool.

## Overview

Our AI-assisted workflow adapts the **Spec → Plan → Code → Review** methodology to work seamlessly with:

- **Issue Maintenance Tool** - Issue tracking and file management
- **Tortoise SVN** - Version control
- **WIP Folders** - Safe, isolated development environments
- **Cursor & ChatGPT** - AI pair programming

## The Workflow

### 1. Start with an Issue

Every development task begins in the **Issue Maintenance Tool**:

1. **Create or Select an Issue**
   - File > New Issue (Ctrl+I)
   - Fill in Issue Title and Customer Facing Description
   - Select appropriate Issue Category (Feature, Bug, NFR, etc.)
   - Set Release version

2. **Set Up Working Copy Path**
   - When ready to develop, open Tools > Send to Testing
   - Set your Working Copy Path (e.g., `C:\Dev\WIP\Issue32527`)
   - This will be your isolated development environment

### 2. Create a WIP Folder Structure

**Work-in-Progress (WIP)** folders provide version control safety and isolation.

#### Recommended Folder Structure

```
C:\Dev\WIP\
  └── Issue[IssueNumber]\        # e.g., Issue32527
      ├── Trunk\                 # Fresh checkout from trunk
      ├── Working\               # Your modified files
      ├── Specs\                 # Specification documents for AI
      ├── Plans\                 # AI-generated implementation plans
      └── Review\                # Code review notes
```

#### Setting Up with Tortoise SVN

1. **Create Base Folder**

   ```
   C:\Dev\WIP\Issue32527\
   ```

2. **Checkout from Trunk**
   - Right-click `Trunk` folder > TortoiseSVN > SVN Checkout
   - URL: `http://gss2k19rnd/svn/3rnet/trunk/` (or your specific path)
   - Use "Choose items..." to select only the files you need
   - Click OK to checkout

3. **Create Working Copy**
   - Copy files from `Trunk\` to `Working\`
   - The `Working\` folder is where you'll make changes
   - Keep `Trunk\` pristine for easy comparisons

### 3. Write the Spec (AI Context Document)

Create a specification file that AI can reference throughout development.

#### `Specs\SPEC_Issue32527.md`

```markdown
# Spec: [Issue Title]

## Issue Information

- Issue ID: 32527
- Category: Feature
- Release: 2019.1
- Menu Path: Inventory > Reports > Stock Status

## Problem Statement

[Clear description of what needs to be fixed or added]

## Requirements

1. [Specific requirement 1]
2. [Specific requirement 2]
3. [Specific requirement 3]

## Files Involved

- INVR0100.cbl (Main report program)
- INVR0100.CPY (Copybook)
- [Additional files from Issue Maintenance]

## Constraints

- Must maintain compatibility with existing database schema
- No changes to menu structure
- Follow existing coding standards

## Testing Criteria

- [ ] Criterion 1
- [ ] Criterion 2
- [ ] QA approval in Issue Maintenance Tool
```

**Tips for Writing Specs:**

- Reference the Issue Maintenance Tool fields (Customer Facing Description, Internal Notes)
- Include file paths and copybook dependencies
- List any SQL changes or conversions needed
- Mention specific business rules or logic

### 4. Generate an AI Plan

Use ChatGPT or Cursor to create an implementation plan based on your spec.

#### Prompt Template for Planning

```
I need to work on Issue #32527 in our COBOL/VB.NET application.

Context:
- Environment: Visual Studio 2015, Fujitsu COBOL
- Version Control: SVN via Tortoise SVN
- Files: [List from Issue Maintenance Tool]

Please read the spec in Specs\SPEC_Issue32527.md and create a detailed
implementation plan following these steps:

1. Analyze current code structure
2. Identify specific changes needed
3. Consider data layer impacts
4. Plan SQL changes or conversions
5. Outline testing approach

Format the plan with:
- Numbered steps
- File-by-file changes
- Potential risks or dependencies
- Rollback procedure if needed
```

#### Save the Plan

Save AI's response to `Plans\PLAN_Issue32527.md` for reference.

### 5. Code with AI Assistance

Now implement the plan using Cursor or ChatGPT.

#### Using Cursor (Recommended for Code Changes)

1. **Open Working Folder in Cursor**

   ```
   cursor C:\Dev\WIP\Issue32527\Working
   ```

2. **Add Context Files**
   - Add `Specs\SPEC_Issue32527.md` to chat
   - Add `Plans\PLAN_Issue32527.md` to chat
   - Add relevant source files

3. **Iterative Development**
   - Work through plan step-by-step
   - Test changes in your local clinic
   - Use Cursor's "Apply" feature to make edits

#### Using ChatGPT (Recommended for Architecture/Planning)

- Complex refactoring decisions
- Understanding business logic
- Researching best practices
- Generating comprehensive documentation

### 6. Version Control with SVN

As you make progress, use Tortoise SVN to track changes.

#### Comparing Changes

1. **Right-click** `Working\` folder
2. **TortoiseSVN > Check for Modifications**
3. **View differences** between your working copy and trunk

#### Creating an SVN Branch (Optional for Large Changes)

If working on a major feature, consider creating an SVN branch:

```bash
# From TortoiseSVN browser:
# Right-click trunk > Branch/tag
# To URL: branches/Issue32527
# Create from: HEAD revision
```

### 7. Add Files to Issue Maintenance Tool

As you modify files, update the Issue:

1. **Drag and Drop Files**
   - Open your Issue in Issue Maintenance Tool
   - Drag files from `Working\` folder into the Files section
   - Type auto-populates based on previous usage

2. **Set File Types**
   - Ensure each file has correct Type (location)
   - Check "Include in Bin Client" if needed
   - Check "Include In Plugins" if needed

### 8. Send to Testing

When your changes are ready for QA:

1. **Tools > Send to Testing**
   - Working Copy Path should be set to your WIP folder
   - Files will be copied to SVN testing location
   - Issue status changes to "In Testing"

2. **QA Process**
   - QA tests your changes
   - QA marks issue as "Resolved" or returns to "In Testing"

### 9. Code Review with Fresh AI

Use a **new AI chat** to review your changes (prevents bias):

#### Prompt Template for Review

```
Please review the code changes I made for Issue #32527.

Original Spec: [Paste or attach Specs\SPEC_Issue32527.md]

Changed Files:
[Paste or attach modified files]

Please check for:
1. Logic errors or edge cases
2. Code quality and standards
3. Potential performance issues
4. Missing error handling
5. SQL injection or security concerns
6. Compatibility with our COBOL/VB.NET stack
```

Save review notes to `Review\REVIEW_Issue32527.md`.

### 10. Commit to SVN

Once QA approves (status = "Resolved"):

1. **Tools > Commit to SVN**
   - Working Copy Path populated automatically
   - Log message defaults to "Issue ID - Description"
   - Commits to trunk
   - Issue status changes to "Released"

## WIP Folder Best Practices

### Naming Conventions

```
Issue[Number]_[ShortDescription]\     # Easy to identify
  Example: Issue32527_StockReport\
```

### Version Control Safety

**Always keep a pristine trunk copy:**

- `Trunk\` = untouched checkout
- `Working\` = your changes
- Easy to compare and rollback

**Before making changes:**

```bash
# Create baseline copy
xcopy /E /I Trunk Working
```

### Rollback Procedure

If something goes wrong:

1. **Compare Working vs Trunk**
   - Use TortoiseSVN Compare to see what changed

2. **Selective Rollback**
   - Copy specific files from `Trunk\` to `Working\`
   - Revert individual changes

3. **Full Rollback**
   - Delete `Working\` folder
   - Recreate from `Trunk\`

### Cleanup

After issue is committed and released:

```bash
# Archive or delete WIP folder
move C:\Dev\WIP\Issue32527 C:\Dev\Archive\Issue32527
```

## AI Prompting Strategies

### Tool Selection Guide

| Task                    | Recommended Tool | Why                                      |
| ----------------------- | ---------------- | ---------------------------------------- |
| Quick code edits        | Cursor           | Direct file integration, fast iterations |
| Architecture decisions  | ChatGPT          | Better at explaining tradeoffs, research |
| Understanding copybooks | ChatGPT          | Can explain complex COBOL structures     |
| Refactoring VB.NET      | Cursor           | Apply changes directly to code           |
| Writing SQL queries     | Either           | Both handle SQL well                     |
| Debugging logic errors  | Cursor           | Can see full file context                |
| Planning large changes  | ChatGPT          | Better at high-level thinking            |

### Effective Prompts for COBOL Development

**Understanding COBOL logic:**

```
This COBOL program uses the following copybook structure:
[Paste copybook]

Can you explain what happens in the PROCEDURE DIVISION when:
[Specific scenario or condition]
```

**Modernizing COBOL to VB.NET:**

```
I need to convert this COBOL paragraph to VB.NET:
[Paste COBOL code]

Context:
- We use Entity Framework for data access
- DevExpress for UI components
- Follow our DataLayer pattern

Please provide equivalent VB.NET code with error handling.
```

**Adding features to existing code:**

```
Current code: [Paste from Working\filename]
Copybook: [Paste copybook if relevant]

New requirement: [From spec]

Please modify the code to add this feature while:
1. Maintaining existing functionality
2. Following the same code pattern
3. Adding appropriate error handling
```

### Prompt Templates by Issue Category

#### Feature Issues

```markdown
Issue Category: Feature
Files: [From Issue Maintenance]

Current Behavior:
[What the code does now]

Required Enhancement:
[What needs to be added]

Please provide:

1. Implementation approach
2. Code changes needed
3. Potential side effects
4. Testing recommendations
```

#### Bug Fixes

```markdown
Issue Category: Bug
Bug Description: [From Customer Facing Description]

Steps to Reproduce:

1. [Step 1]
2. [Step 2]

Current Code: [Paste relevant section]

Please:

1. Identify the root cause
2. Suggest a fix
3. Explain why the bug occurs
4. Recommend prevention strategies
```

#### NFR (Non-Functional Requirement)

```markdown
Issue Category: NFR
Task: [e.g., "Convert CUSTOMER.CPY to use new DataLayer object"]

Current Implementation: [Paste old code]
Target Pattern: [Paste example of new pattern]

Please create the conversion following our standards.
```

## Integration with Issue Maintenance Tool

### Workflow States

```
New → In Testing → Resolved → Released
```

**Key Points:**

- Only QA can mark "Resolved"
- Committing changes status to "Released"
- "Resolved" issues cannot be edited (change to "In Testing" first)

### File Management

**Drag & Drop Best Practice:**

1. Make changes in `Working\` folder
2. Test locally in clinic
3. Drag files from `Working\` to Issue Maintenance Files grid
4. Type auto-populates if file was used before

### Dependencies

If your issue depends on another issue:

1. Add dependency in Issue Maintenance Tool
2. Reference in your spec document
3. Ensure dependent issue is resolved first

## Tips for Success

### Start Small

For your first AI-assisted issue:

- Choose a simple bug fix or small feature
- Practice the WIP folder workflow
- Get comfortable with the tools

### Keep Specs Updated

As requirements change:

- Update `Specs\SPEC_IssueXXX.md`
- Re-run planning prompts with ChatGPT
- Document decision changes

### Use Version Control Early

Don't wait until you're "done":

- Compare changes frequently (TortoiseSVN)
- Commit to local branch for big changes
- Keep trunk copy pristine

### Learn from Reviews

Save code review notes in `Review\` folder:

- Build a library of common issues
- Improve your prompts over time
- Share learnings with team

### Leverage ChatGPT for Research

When stuck:

- "Explain this COBOL pattern to me"
- "What's the VB.NET equivalent of this?"
- "How should I structure this DataLayer call?"

## Troubleshooting

### Issue: AI makes incorrect assumptions

**Solution:** Improve your spec document

- Add more context about business rules
- Include examples of correct behavior
- Reference similar existing code

### Issue: Changes break existing functionality

**Solution:** Use incremental testing

- Test after each small change
- Keep trunk copy for comparison
- Use TortoiseSVN to see exact diffs

### Issue: SVN conflicts when committing

**Solution:** Update before committing

1. Right-click `Trunk\` > SVN Update
2. Compare updated trunk with your `Working\`
3. Merge changes manually
4. Test again before commit

### Issue: Lost track of what changed

**Solution:** Use TortoiseSVN Compare

- Right-click `Working\` folder
- TortoiseSVN > Check for Modifications
- Shows all changed files with diffs

## Next Steps

1. **Try it on a small issue**
   - Pick a simple bug fix
   - Follow the WIP folder workflow
   - Send to testing and get QA feedback

2. **Build your template library**
   - Save successful prompts
   - Create reusable spec templates
   - Document common patterns

3. **Share with the team**
   - Present your workflow to colleagues
   - Collect feedback and iterate
   - Build team best practices

---

**Remember:** The key to successful AI-assisted development is good documentation (specs), safe environments (WIP folders), and iterative testing. Start small, learn the workflow, and gradually tackle larger issues.
