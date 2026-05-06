---
title: "Cursor at GSS"
description: "Cursor is the AI-first IDE most of P&E uses for daily coding. Same MCP servers as Claude Code, same internal tools, different shape: it lives in your ed..."
sidebar_position: 3
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, cursor, umbrella]
---

> **TL;DR** — Cursor is the AI-first IDE most of P&E uses for daily coding. Same MCP servers as Claude Code, same internal tools, different shape: it lives in your editor instead of the terminal. This article covers how Cursor fits into the GSS setup. For depth on Cursor's general feature set (Composer, Agents, Bugbot, etc.), see the [main Cursor doc](../../cursor.md).

## Overview

Cursor is what most engineers reach for first. It's a desktop application that looks and feels like VS Code — same extensions, same keybindings, same UI — but with AI features integrated at the editor level: Tab autocomplete, inline edits, multi-file Composer/Agent runs, and asynchronous Cloud Agents.

What makes our setup different from "vanilla" Cursor:

- **Internal MCP servers wired in.** The same MCPs Claude Code uses (`cobol-mcp`, `mcp-intelligence`, `log-parser`, `queue-routing`, `svn-ops`, `book-of-armaments`, etc.) work in Cursor too — Cursor has first-class HTTP/stdio MCP support.
- **`AGENTS.md` in our repos.** Most repos have an `AGENTS.md` at the root that Cursor (and Codex, and Claude Code) all read for project-specific rules.
- **Bugbot on PRs.** Pull requests get an automated review pass from Cursor's Bugbot, with inline comments on bugs / security / quality issues.

## Why use it

- **It's an IDE.** If you live in a code editor all day, Cursor's edits-in-place flow is faster than switching to a CLI agent.
- **Tab autocomplete.** Multi-line, cross-file predictions while you type. Press Tab. Keep typing.
- **Inline Edit (`Cmd/Ctrl+K`).** Highlight code, describe an edit, accept the diff. The fastest way to make scoped changes.
- **Composer (Agent mode).** Multi-step tasks across files, with the editor showing each change as it happens.
- **Cloud Agents.** Hand off work that would take 30 minutes; review the resulting PR while you do other things.
- **Codebase search.** Cursor indexes the repo so chat answers cite real files, not training data.

## When to use it (vs. Claude Code)

| Scenario | Cursor | Claude Code |
|---|---|---|
| Editing while you read code | ✅ | |
| Tab-autocomplete-driven coding | ✅ | |
| Quick scoped edit at the cursor (`Cmd+K`) | ✅ | |
| Async/background agents that PR back | ✅ Cloud Agents | (Claude Code background mode) |
| Automated PR review | ✅ Bugbot | (use a code-review subagent) |
| Long terminal automation, scripts, hooks | | ✅ |
| Heavy MCP-driven workflows | both work; pick by ergonomics | both work |
| Working from a tablet/SSH session | | ✅ |

Many engineers use both, picking per-task.

## How to access it

- **Download:** [cursor.com](https://cursor.com)
- **Plan:** GSS-licensed seats (request from your manager). Free tier exists but is rate-limited.
- **Sign in:** Use the email tied to your GSS Cursor seat.
- **Setup details:** see the [main Cursor doc](../../cursor.md) for keybindings, model selection, rule files, and pricing tiers.

## Easy MCP setup at GSS (the recommended path)

You don't need to wire each MCP server up by hand. Give your Cursor agent access to your repository, then **connect to `mcp-intelligence`** — it acts as a proxy that exposes every downstream MCP we run (cobol-mcp, log-parser, queue-routing, svn-mcp, book-of-armaments, testarchitect-mcp, clinic-utilities, agents registry, and more).

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

Once connected, Cursor calls any registered downstream tool through `call_proxy_tool(server="<name>", tool_name="<tool>", arguments=...)`. New MCP servers register themselves with mcp-intelligence as they come online — your Cursor agent picks them up without you editing config.

> **💡 Even easier:** ask your Cursor agent to do it. With repo access and a one-line prompt — "configure my Cursor MCP for the GSS internal toolkit" — it'll write the `.cursor/mcp.json` for you, drop in any auth env vars, and you're done.

For the full inventory of MCP servers exposed via mcp-intelligence, see the **MCP servers** section of this catalog.

## How to use it (the basics)

1. Open your project folder.
2. Wait for the codebase to index (one-time per repo).
3. Tab autocomplete kicks in as you type — press Tab to accept.
4. `Cmd/Ctrl+K` for inline edits at the cursor.
5. `Cmd/Ctrl+L` for chat with full codebase context. Use `@Files`, `@Folders`, `@Docs`, `@Web` to scope.
6. Switch to Agent mode (Composer) for multi-step, multi-file tasks.
7. For longer async work, kick off a Cloud Agent — let it run in the background and PR back.

For more depth on each of those, the [main Cursor doc](../../cursor.md) is the source of truth.

## Common questions

**Does it use the same models as Claude Code?**
It can — Cursor lets you pick Claude Opus 4.7, Sonnet 4.6, GPT-5.2, GPT-5.2-Codex, or Cursor's own Composer 2 (200k context, Agent + Thinking + Images). Many engineers leave it on **Auto** and let Cursor pick.

**Will it see customer-confidential data?**
Treat Cursor like any cloud AI: don't paste PII, customer-confidential data, or production credentials. Privacy Mode tightens retention; Enterprise tier makes it mandatory. Confirm your tier in Settings.

**How do rules work?**
Cursor reads `.cursor/rules/<topic>.md` files automatically. `AGENTS.md` at the project root is also picked up and is portable across Cursor / Codex / Claude Code.

**What's Bugbot's deal?**
Automated PR review. Posts inline comments on bugs / security / quality. ~$40/mo at the Pro tier covers 200 PR reviews. Trigger via `@bugbot review` on a PR or configure auto-run.

## How it works

Cursor is built on VS Code's open-source Code OSS. It adds an AI layer that talks to multiple model providers (Anthropic, OpenAI, Cursor Composer, others). The codebase index is local. MCP servers are configured per-project via `.cursor/mcp.json` (HTTP or stdio, both supported). Cloud Agents run on Cursor's infrastructure with isolated repo checkouts and return PRs or summaries on completion.

## Owner & support

- **Vendor:** Anysphere (Cursor)
- **Docs:** [docs.cursor.com](https://docs.cursor.com)
- **Main internal doc:** [Cursor (full feature reference)](../../cursor.md)
- **GSS account/access:** Request a seat from your manager.
- **Last reviewed:** 2026-05-06
