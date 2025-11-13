---
title: Cursor
description: AI-powered IDE setup and best practices
sidebar_position: 2
last_updated: 2025-10-10
tags: [cursor, ide, ai-editor, setup]
---

# Cursor - Your AI Pair Programmer

Cursor is an AI-powered IDE (VS Code-based) with native AI editing and inline commands. It serves as your pair programmer for fast, contextual editing and refactoring within your codebase.

## Quick Reference

### Project Setup

#### Rules

Define rules in `.cursor/rules/` to enforce guidelines such as coding style and terminology. Rules apply to every query automatically.

**Official Documentation:** [https://cursor.com/docs/context/rules](https://cursor.com/docs/context/rules)

Rules provide system-level instructions to Agent and Inline Edit. They are persistent context, preferences, or workflows for your projects. It's good practice to have rules files for all codebases.

#### Alternative: AGENTS.md

For more versatility when using other AI tools/agents, you can utilize `AGENTS.md` as an alternative to `.cursor/rules`.

**Key differences:**

- `AGENTS.md` is a simple markdown file for defining agent instructions
- Place it in your project root
- Unlike Project Rules, it's a plain markdown file without metadata or complex configurations
- Perfect for projects that need simple, readable instructions without the overhead of structured rules
- Cursor supports `AGENTS.md` in the project root and subdirectories, allowing each level to be specifically tailored to that section of your project

#### Memory Bank

Initialize with:

```bash
npx cursor-bank init
```

This creates a structured context file. Update these files as you work using the "update memory bank" command to keep AI's knowledge current.

#### Model Settings

- Use **GPT-4** or **Claude Sonnet 4.5** for complex tasks
- Enable **"Thinking" mode** for long reasoning tasks if needed

#### Indexing

- Cursor auto-indexes files for context
- Use **Resync Index** after large changes

<!-- TODO: add screenshot of Resync Index location -->

## Prompting Best Practices

### Be Explicit and Provide Context

- Attach code with `@Files` or snippets
- Reference specific documentation
- Visit [cursorpractice.com](https://cursorpractice.com) for more tips

### Use Plan/Act Workflow

1. **Plan:** `"Plan: <task>"` → get plan
2. **Act:** Execute the plan

Example:

```
Plan: Refactor the calculateTotals function to use modern COBOL syntax
```

Wait for the plan, review it, then:

```
Act
```

### Refer to Rules and Memory

When prompting, reference your project's rules and memory:

```
Following our coding standards in systemPatterns.md, update the error handling...
```

### Ask for Step-by-Step Reasoning

If you need transparency in the AI's thought process:

```
Explain step-by-step how you would approach fixing this bug
```

### Iterate on Prompts

If output is off, clarify or rephrase and try again. Small tweaks in wording can yield better results.

> **💡 Tip:** If Cursor seems confused, try rephrasing your request with more specific context or break it into smaller steps.

## Cursor Shortcuts & Features

### Context Inclusion

| Shortcut   | Description                                                                        |
| ---------- | ---------------------------------------------------------------------------------- |
| `@Files`   | Include specific files in your prompt                                              |
| `@Code`    | Reference code snippets                                                            |
| `@Folders` | Include entire folders                                                             |
| `@Docs`    | Include documentation (after indexing via Settings > Docs)                         |
| `#`        | Reference a specific tab you have open that might not be in your current workspace |

### Adding Code to Chat

- **Drag & drop** files into the chat window
- **Ctrl+Shift+L** to add selected code to chat
- **Highlight code** and click 'Add to Chat'

### Troubleshooting Commands

- **Ctrl+Shift+P** → "Reload Window" if Cursor misbehaves or AI seems confused
- Keep `.cursorrules` open in a tab to help AI keep context, especially during reloads
- Use **Add Context** window to pin memory files if needed (ensuring they're always sent)

## Giving Cursor Access to Your Codebase

Add directories to your workspace so Cursor can access and reference other programs outside of your WIP that might provide better context (e.g., copybooks, calling programs, etc.).

### How to Add Folders

1. Right-click in the Explorer panel
2. Select **"Add Folder to Workspace"**
3. Navigate to the folder (e.g., `trunk`)
4. Click **Select Folder**

Now you can reference what's in `trunk` when you chat with Cursor.

<!-- TODO: add screenshot of "Add Folder to Workspace" menu -->

> **💡 Tip:** Adding your copybook directories and commonly-used utility programs to your workspace helps Cursor understand dependencies and relationships in your code.

## Rules Files Best Practices

### Location

```
project-root/
├── .cursor/
│   └── rules/
│       ├── coding-standards.md
│       ├── terminology.md
│       └── workflow.md
└── AGENTS.md (alternative approach)
```

### Tailoring Rules to Our ERP Stack

- **By lane:** keep separate rule packs such as `.cursor/rules/legacy-core.md`, `.cursor/rules/devexpress-ui.md`, and `.cursor/rules/cloud-services.md`. This lets COBOL, DevExpress, and C# contributors load focused guidance without wading through unrelated noise.
- **Call out stack specifics:** include canonical command snippets (`scripts/cobol/run_harness.sh`, `dotnet test src/services/sp2/...`) and terminology (SP2, DataLayer, ShopFloor) so Cursor learns the vocabulary older programmers already use.
- **Module overrides:** place additional `rules.md` inside module folders (e.g., `src/services/datalayer/rules.md`) to remind the AI about copybook names, DevExpress bindings, or VB.NET interop decisions when working locally.
- **Program guardrails:** document fragile COBOL paragraphs, DevExpress designer files, or SP2 metadata limitations so Cursor refuses destructive edits without an explicit confirmation prompt.

### Project Context Bundles

- **Context packs:** create `.cursor/context/*.ctx.json` entries that pin the architecture diagram, DataLayer guide, and relevant source files. Toggle between `legacy-core` and `modern-cloud` packs depending on the task so both veteran and modern teams get the right prompt mix.
- **Prompt macros:** use Cursor's `snippets/` to store reusable modernization prompts (e.g., _“Rewrite this COBOL paragraph using DataLayer shim XYZ and include cursor-based acceptance tests”_). Reference `prompt-library.md` so the macro stays updated.
- **System prompts for audits:** when running large refactors, load `docs/testing-strategy.md` and `docs/legacy-modern-handbook.md` via `@Docs` so the AI keeps shared QA and hand-off rules in memory.

### Accessibility Tips for Legacy Teams

- Surface Cursor command palettes and macros via printable cheat sheets stored in `docs/handouts/`. Include keyboard-only workflows and guidance for low-bandwidth RDP sessions common on shop-floor machines.
- Encourage pairing sessions where modern engineers record walkthroughs of Cursor usage on COBOL/DataLayer projects, then archive them alongside the written rules for on-demand refreshers.

### What to Include in Rules

- **Coding standards** - Naming conventions, formatting, comment styles
- **Terminology** - Project-specific terms and their meanings
- **Workflow preferences** - How you want AI to approach tasks
- **Constraints** - What the AI should avoid or be cautious about
- **References** - Links to memory files, documentation, or resources

### Example Rule File Structure

```markdown
# Coding Standards

## Naming Conventions

- Use UPPER-CASE-WITH-HYPHENS for COBOL variables
- Prefix copybooks with CP-

## Error Handling

- Always include error handling for file operations
- Use standard error message format: ERR-XXXX

## Comments

- Include paragraph descriptions
- Document complex logic inline
```

## Integration with Memory System

To keep Cursor synchronized with your project memory and change history, add this to your `AGENTS.md` or rules:

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
- **PROJECT_MEMORY.md**: For stable, reusable knowledge, propose adding or refining entries.

### 4. Guardrails & integrity

- Never delete or rewrite prior entries in `CHANGE_HISTORY.md` or older memory.
- All updates should be **additive or corrective via append**.
- Always reference file paths, versions, and line ranges.

### 5. Workflow placement

- At session start: agent reads both docs before accepting further prompts.
- Before proposing changes: agent cross-checks history & memory.
- After producing a diff: agent outputs the new entry blocks for both docs.
```

## Version Control Best Practices

When working with AI, which can sometimes hallucinate bugs into code if the context window gets overflown, it's a good idea to version control every change that works.

> **⚠️ Note:** Consider setting up a separate WIP away from your Checkout repo with your own private SVN session to ensure you're incrementally moving in the right direction. This also allows you to revert when AI goes haywire.

## Common Issues and Solutions

### AI Hallucinating or Off-Track

- **Refresh context:** Open relevant files, reload Cursor
- Verify that memory files contain correct info (no "poisoned" old info)

### Cursor AI Unresponsive

- Possibly re-login or ensure API keys haven't expired
- Try a smaller prompt to see if it responds
- If needed, restart Cursor IDE

### Context Lost Mid-Conversation

- Summarize where you left off
- Start a new chat
- Feed summary back in

### Large Output Gets Cut Off

- The model hit token limit for response
- Prompt: "continue from where you left off"
- For code, ensure it didn't omit any parts

---

## Next Steps

- Configure [ChatGPT Enterprise](./chatgpt.md) for research and planning
- Set up [Codex CLI](./codex.md) for deep code analysis
- Learn the [Spec → Plan → Code → Review workflow](./workflow.md)
