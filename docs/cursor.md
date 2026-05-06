---
title: Cursor
description: AI-powered IDE setup, latest features, and best practices for the GSS stack
sidebar_position: 2
last_updated: 2026-05-06
tags: [cursor, ide, ai-editor, setup, composer-2, bugbot, cloud-agents, mcp]
---

# Cursor — Your AI Pair Programmer

Cursor is an AI-first IDE built on top of VS Code. Much of P&E uses it daily for coding, refactoring, and pair programming. It runs Cursor's own **Composer** family of models alongside frontier models from Anthropic and OpenAI, with deep integrations for Tab autocomplete, multi-file Agent edits, Background Cloud Agents, automated PR review, and MCP-based tool access.

> **💡 Don't have it yet?** Download from [cursor.com](https://cursor.com). Use your GSS-licensed seat (request from your manager if you don't have one). Sign in with the email tied to your seat.

## What's new in 2026

If you've been away from Cursor for a few months, here's what changed:

- **Composer 2** is the current flagship model. 200k context window, Agent + Thinking + Image capability. Composer 1 and 1.5 are still available but hidden by default.
- **Background / Cloud Agents** run async work on Cursor's infrastructure. You hand off a task, walk away, and review the PR or summary later. Cloud Agents always run in **Max Mode**.
- **Bugbot** is Cursor's automated PR review bot. Reads diffs, flags bugs / security / quality issues, and posts inline GitHub comments. $40/mo Pro tier, free tier available.
- **First-class MCP support.** Configure HTTP or stdio MCP servers per-agent (local and cloud) — Linear, Figma, GitHub, Notion, our internal MCPs, anything.
- **Memories** persist context across chat sessions automatically.
- **Cursor CLI** lets you drive Cursor agents from a terminal without opening the IDE.
- **Slack and Linear integrations** kick off agents from chat or tickets.
- **TypeScript SDK** (`@cursor/sdk`) for programmatic agent creation — useful for custom automation pipelines.

## Models

| Model | Use it for | Notes |
|---|---|---|
| **Auto** | Default for most tasks | Cursor picks the right model per request — usually a good middle ground |
| **Composer 2** | Cursor's flagship | 200k context, Agent + Thinking + Images. Best for multi-file refactors and long sessions |
| **Claude Opus 4.7** (1M context) | Hardest tasks, deep reasoning | Anthropic's top model — reach for it when Composer 2 spins out |
| **Claude Sonnet 4.6** | Balanced day-to-day | Fast, very strong on code |
| **GPT-5.2 / GPT-5.2-Codex** | Cross-checking, certain coding flavors | OpenAI's flagship coding variant |

Switch via the model picker (bottom-right of chat). For long autonomous runs, **Max Mode** raises the per-turn limits and enables Cloud Agents.

## Quick Reference

### Project Setup

#### Rules

Define rules in `.cursor/rules/` to enforce coding style, terminology, and workflow preferences. Rules apply to every Agent and Inline-Edit query automatically.

**Official Documentation:** [https://cursor.com/docs/context/rules](https://cursor.com/docs/context/rules)

> **⚠ Note:** The legacy single-file `.cursorrules` is deprecated. Use `.cursor/rules/` directories — multiple files, scoped, easier to maintain.

#### Alternative: AGENTS.md

For cross-tool portability, use `AGENTS.md` at the project root (and inside subfolders for scoped rules). It's a plain Markdown file Cursor and other agents (Codex, Claude Code) all respect.

#### Memory Bank (third-party)

```bash
npx cursor-bank init
```

Creates a structured context file. Update with the "update memory bank" command. Cursor's built-in **Memories** feature now overlaps with this — pick one approach per project.

#### Indexing

- Cursor auto-indexes files for context.
- Use **Resync Index** after large refactors or branch switches.
- The index now respects `.cursorignore` and Git-ignored files automatically.

## Core features

### Tab autocomplete

Multi-line code predictions as you type. Press **Tab** to accept. The predictions cross file boundaries — Cursor sees what's open and uses that context.

### Inline Edit (Cmd/Ctrl+K)

Highlight code, press `Cmd/Ctrl+K`, describe the edit. Cursor shows the diff inline; accept, reject, or iterate. Best for small, scoped changes.

### Chat (Cmd/Ctrl+L)

Side panel with full codebase context. Use `@Files`, `@Folders`, `@Code`, `@Docs`, `@Web`, `@Git`, `@LinkedIn`, `#tab` to scope context. Drop files in via drag-and-drop.

### Agent mode (Composer)

Give Cursor a multi-step task. It plans, reads files, edits them, runs the terminal, and pauses for your approval before destructive actions. Toggle Max Mode for long-running autonomous work.

### Background / Cloud Agents

Hand off a task to a remote agent that works on Cursor's infrastructure. You can keep coding while it runs.

- Always operate in **Max Mode** (mandatory; not configurable).
- Repository access via `cloud.repos[]` — checkout starting ref configurable.
- MCP servers can be HTTP or stdio.
- Returns a PR or a summary at completion.

We have a `cursor-cloud-agent-test` LaunchPad app for trying this out before adopting it in your team's workflow.

```typescript
// Programmatic example (TypeScript SDK)
import { Agent } from "@cursor/sdk";

const agent = await Agent.create({
  apiKey: process.env.CURSOR_API_KEY!,
  model: { id: "composer-2", params: [{ id: "thinking", value: "high" }] },
  cloud: {
    repos: [{ url: "https://github.com/GlobalShopSolutions-InternalTools/<repo>", startingRef: "main" }],
  },
  mcpServers: {
    github: {
      type: "stdio",
      command: "npx",
      args: ["-y", "@modelcontextprotocol/server-github"],
      env: { GITHUB_TOKEN: process.env.GITHUB_TOKEN! },
    },
  },
});

await agent.send("Refactor the inventory data layer to use the new SP2 shim.");
```

### Bugbot — automated PR review

Bugbot reads every PR diff and posts inline GitHub comments flagging bugs, security issues, and code-quality problems. Configurable to run automatically on every push or manually via PR comment.

- **Pro tier:** $40/mo for 200 PR reviews per month.
- **Team tier:** higher limits, analytics API.
- Analytics endpoint for usage tracking by repo and date range.

For our org, Bugbot is most useful on the larger modernization PRs — leave a `@bugbot review` comment on any PR.

### MCP server support

Cursor speaks MCP (Model Context Protocol) natively. Configure servers in `.cursor/mcp.json` (project-level) or globally. Supports both `http` and `stdio` server types, with auth helpers for Bearer tokens, OAuth, and static API keys.

#### Easy MCP setup at GSS

Don't wire each MCP server up by hand. Connect Cursor to **`mcp-intelligence`** — it acts as a proxy that exposes every downstream MCP we run (cobol-mcp, log-parser, queue-routing, svn-mcp, book-of-armaments, testarchitect-mcp, clinic-utilities, agents registry, GitHub MCP, M365 MCP, Notion, monday.com, and more).

Minimum viable `.cursor/mcp.json`:

```json
{
  "mcpServers": {
    "mcp-intelligence": {
      "type": "http",
      "url": "https://mcp-intelligence.globalshopsolutions.dev/mcp"
    }
  }
}
```

Once connected, Cursor calls any registered downstream tool via `call_proxy_tool(server="<name>", tool_name="<tool>", arguments=...)`. New MCP servers register themselves with mcp-intelligence as they come online — Cursor picks them up without you editing config.

> **💡 Even easier:** ask your Cursor agent to do it. With repo access and a one-line prompt — *"configure my Cursor MCP for the GSS internal toolkit"* — it'll write `.cursor/mcp.json` for you, drop in any auth env vars, and be ready.

The same pattern works in [Claude Code](./claude-code.md) and any other MCP-compatible AI agent. See the [AI Tools at GSS Catalog](./ai-tools-catalog/overview.md) for the full inventory of MCP servers behind the proxy.

### Memories

Persistent context that carries between chat sessions automatically. Cursor decides what's worth remembering (decisions, preferences, facts you confirmed) and surfaces it later. Edit/delete in Settings → Memories.

### Cursor CLI

Drive agents from the terminal without opening the IDE:

```bash
cursor agent run "Add a unit test for the new SP2 helper" --model composer-2
cursor agent list
cursor agent logs <agent-id>
```

Useful for scripting, CI integration, and headless workflows.

### Slack / Linear integrations

- **Slack:** mention `@Cursor` in a thread to spawn a cloud agent on the linked repo.
- **Linear:** assign tickets to Cursor; it picks them up, runs an agent, and opens a PR linked back to the ticket.

## Prompting Best Practices

### Be Explicit and Provide Context

- Attach code with `@Files`, `@Folders`, `@Code` — don't make Cursor guess.
- Reference specific documentation via `@Docs` (after indexing in Settings → Docs).
- Visit [cursorpractice.com](https://cursorpractice.com) for additional patterns.

### Plan / Act Workflow

For non-trivial work, plan first:

```
Plan: Refactor the calculateTotals function to use the new DataLayer shim
```

Review the plan, then:

```
Act
```

This forces an explicit review checkpoint before any code change.

### Refer to Rules and Memory

```
Following our coding standards in coding-standards.md, update the error handling…
```

### Ask for Step-by-Step Reasoning

```
Explain step-by-step how you would approach fixing this bug
```

### Iterate on Prompts

If output drifts, rephrase rather than fight. Small wording changes yield outsized improvements.

> **💡 Tip:** When Cursor seems confused, try restating the request with concrete file paths and the exact symptom you're seeing.

## Cursor Shortcuts & Features

### Context Inclusion

| Shortcut | Description |
|---|---|
| `@Files` | Include specific files in your prompt |
| `@Code` | Reference code snippets |
| `@Folders` | Include entire folders |
| `@Docs` | Include indexed documentation (Settings → Docs) |
| `@Web` | Live web search |
| `@Git` | Reference recent commits / branches |
| `#<tab-name>` | Reference an open tab not in the current workspace |

### Adding Code to Chat

- **Drag & drop** files into the chat window.
- **`Ctrl+Shift+L`** to add selected code to chat.
- **Highlight code** and click "Add to Chat".

### Troubleshooting Commands

- **`Ctrl+Shift+P` → "Reload Window"** if Cursor misbehaves or AI drifts.
- Keep a key rule file open in a tab so the AI keeps its conventions in mind during reloads.
- Use **Add Context** to pin memory files (always sent with prompts).

## Giving Cursor Access to Your Codebase

Add directories to your workspace so Cursor can reference programs outside your WIP — copybooks, calling programs, shared utilities.

### How to Add Folders

1. Right-click in the Explorer panel.
2. Select **Add Folder to Workspace**.
3. Navigate to the folder (e.g., `trunk`).
4. Click **Select Folder**.

> **💡 Tip:** Add your copybook directories and commonly-used utility programs to your workspace so Cursor can reason about cross-program dependencies.

## Rules Files Best Practices

### Layout

```
project-root/
├── .cursor/
│   ├── rules/
│   │   ├── coding-standards.md
│   │   ├── terminology.md
│   │   └── workflow.md
│   └── mcp.json                # MCP server config (per-project)
└── AGENTS.md                   # alternative / cross-tool
```

### Tailoring Rules to Our ERP Stack

- **By lane.** Keep separate rule packs such as `.cursor/rules/legacy-core.md`, `.cursor/rules/devexpress-ui.md`, and `.cursor/rules/cloud-services.md`. COBOL, DevExpress, and C# contributors load focused guidance without unrelated noise.
- **Stack specifics.** Include canonical command snippets (`scripts/cobol/run_harness.sh`, `dotnet test src/services/sp2/...`) and terminology (SP2, DataLayer, ShopFloor) so Cursor learns the vocabulary.
- **Module overrides.** Place additional `rules.md` inside module folders (e.g., `src/services/datalayer/rules.md`) for copybook names, DevExpress bindings, or VB.NET interop decisions.
- **Program guardrails.** Document fragile COBOL paragraphs, DevExpress designer files, or SP2 metadata limitations so Cursor refuses destructive edits without explicit confirmation.

### Project Context Bundles

- **Context packs.** Pin architecture diagrams, the DataLayer guide, and key source files. Toggle between `legacy-core` and `modern-cloud` packs depending on the task.
- **Prompt macros.** Use Cursor's `snippets/` for reusable modernization prompts. Reference `prompt-library.md` so the macro stays current.
- **System prompts for audits.** Load `docs/testing-strategy.md` and `docs/legacy-modern-handbook.md` via `@Docs` for big refactors.

### Accessibility Tips for Legacy Teams

- Surface Cursor command palettes and macros via printable cheat sheets in `docs/handouts/`. Include keyboard-only workflows for low-bandwidth RDP sessions on shop-floor machines.
- Run pairing sessions where modern engineers record walkthroughs of Cursor on COBOL/DataLayer projects, then archive them alongside the written rules for refresher viewing.

### What to Include in Rules

- **Coding standards** — Naming conventions, formatting, comment styles.
- **Terminology** — Project-specific terms and meanings.
- **Workflow preferences** — How you want AI to approach tasks.
- **Constraints** — What the AI should avoid or be cautious about.
- **References** — Links to memory files, docs, or resources.

### Example Rule File

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

Add this to your `AGENTS.md` or rules to keep Cursor synced with project memory:

```markdown
## Agent Behavior: Synchronizing with Change History & Project Memory

### 1. Monitor existing docs
- Before starting new work, the agent **must read** `CHANGE_HISTORY.md` and `PROJECT_MEMORY.md`.
- Use them as **reference** to avoid repeating ideas or conflicting with past decisions.

### 2. Use history & memory in reasoning
- Check if a similar change was already made (per `CHANGE_HISTORY.md`).
- Use `PROJECT_MEMORY.md` to recall conventions, patterns, naming, constraints.
- If new insight is discovered, propose an update to memory.

### 3. How to update docs
- **CHANGE_HISTORY.md**: Append a new entry for every proposed or accepted change.
- **PROJECT_MEMORY.md**: Add or refine entries for stable, reusable knowledge.

### 4. Guardrails & integrity
- Never delete or rewrite prior entries.
- All updates should be **additive or corrective via append**.
- Always reference file paths, versions, and line ranges.

### 5. Workflow placement
- At session start: agent reads both docs before accepting prompts.
- Before proposing changes: agent cross-checks history & memory.
- After producing a diff: agent outputs new entry blocks for both docs.
```

## Pricing (May 2026)

| Tier | Monthly | Includes |
|---|---|---|
| **Free** | $0 | Limited Tab requests, limited slow Agent requests, Bugbot Free PR allowance |
| **Pro (Individual)** | ~$20/mo | 500 fast Agent requests, unlimited slow, Bugbot up to 200 PRs at $40/mo add-on |
| **Pro+** | higher | More fast requests + premium model access |
| **Business / Team** | ~$40/mo per seat | SSO, admin controls, analytics API, centralized billing |
| **Enterprise** | custom | Privacy mode mandatory, audit logs, custom contracts |

Confirm exact pricing on Cursor's pricing page — values shift. GSS pays at the company tier; you don't budget per-task.

## Version Control Best Practices

When working with AI — which can sometimes hallucinate bugs into code if context overflows — version-control every change that works.

> **⚠ Note:** Consider a separate WIP repo with your own private SVN session so you can incrementally check progress and revert when AI goes haywire.

## Common Issues and Solutions

### AI Hallucinating or Off-Track

- **Refresh context:** open relevant files, reload Cursor.
- Verify memory files contain correct info (no "poisoned" old info).
- Drop to a more capable model (Composer 2 → Claude Opus 4.7) for the trickiest cases.

### Cursor Unresponsive

- Re-login if your seat expired.
- Try a smaller prompt to confirm the model is responding.
- Restart the IDE if needed.

### Context Lost Mid-Conversation

- Summarize where you left off.
- Start a new chat; feed the summary back in.
- Memories should pick up the persistent facts automatically.

### Large Output Cut Off

- Hit token limit on response.
- Prompt: "continue from where you left off."
- For code, verify nothing was silently omitted.

### Cloud Agent Stalls

- Check Max Mode is on (it's mandatory for cloud agents).
- Check the agent's logs via the dashboard or `cursor agent logs <id>`.
- If the repo is private, confirm the cloud agent has access (token / SSH key configured).

## Privacy Mode at GSS

- **Default:** Cursor's standard mode sends prompts and selected file context to the configured model provider (Anthropic / OpenAI / Cursor).
- **Privacy Mode:** stricter retention controls — recommended when working on customer-confidential code.
- **Enterprise tier:** Privacy Mode is mandatory.

Treat Cursor like any external AI: don't paste PII, customer-confidential data, or production credentials into prompts unless cleared per GSS data-handling rules.

---

## Next Steps

- Configure [ChatGPT Enterprise](./chatgpt.md) for research and planning.
- Set up [Codex CLI](./codex.md) for OpenAI-side coding tasks.
- Learn the [Spec → Plan → Code → Review workflow](./workflow.md).
- See the [AI Tools at GSS Catalog](./ai-tools-catalog/overview.md) for our internal MCPs you can wire into Cursor.
