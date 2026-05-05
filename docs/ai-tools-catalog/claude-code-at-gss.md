---
title: "Claude Code at GSS"
description: "Claude Code is an AI coding assistant from Anthropic that we run in the terminal and IDE. Engineers at GSS use it to write code, generate tests, refacto..."
sidebar_position: 2
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, claude-code, umbrella]
---

> **TL;DR** — Claude Code is an AI coding assistant from Anthropic that we run in the terminal and IDE. Engineers at GSS use it to write code, generate tests, refactor, debug, draft docs, and automate routine work. It's been extended with GSS-specific "skills" and connected to our internal systems via MCP servers.

## Overview

Claude Code is a command-line tool (with VS Code and JetBrains plugins) that lets you have an AI pair-programmer sitting next to you. You ask it questions, give it tasks, and it reads your code, runs commands, and edits files alongside you. Under the hood it talks to Anthropic's Claude models — currently **Claude Opus 4.7 (1M context)**, **Sonnet 4.6**, and **Haiku 4.5** — the same family that powers [claude.ai](https://claude.ai).

The Claude Code feature set as of mid-2026 includes:

- **Slash commands** — typed shortcuts (`/plan`, `/review`, etc.) that kick off a workflow.
- **Skills** — reusable prompt packs that teach Claude how to handle a specific kind of task. Skills can specify which model (`opus` / `sonnet` / `haiku`) they run on.
- **Subagents** — spawn focused sub-Claudes for parallel work (research, drafting, review) inside one session.
- **Hooks** — shell commands the harness runs in response to events (e.g., log every Bash invocation, block writes to `prod/`).
- **Plugins** — bundles of slash commands + skills + agents + hooks + MCP server definitions, installed from a marketplace.
- **MCP servers** — Model Context Protocol gateways that give Claude extra tools (read/write to external systems).
- **Plan Mode** — generate and review a plan file before any code changes happen.
- **Background agents** — long-running async work that Claude executes while you do other things.
- **Computer Use** — Claude can drive a virtual desktop (click, type, scroll) to operate apps that don't have an API.

What makes our setup different from "vanilla" Claude Code:

- **Internal MCP servers.** Bridges that let Claude read from internal systems — our COBOL source code, internal-tools docs, Outlook/SharePoint, the LaunchPad app catalog, and more.
- **Shared institutional knowledge.** The `mcp-intelligence` MCP server logs significant work (decisions, deploys, bug fixes) so the next person — or the next AI session — can find context.

## Why use it

Manual code work has a high "tax" — boilerplate, looking up syntax, formatting, finding the right file to edit, writing tests, drafting docs. Claude Code absorbs most of that tax. Things that take a senior dev hours often take minutes; things that take days often take hours.

It also lowers the barrier for non-coders to get useful work done. QA can ask it to generate test data. PMs can ask it to summarize a dense technical PR. Support can ask it to draft a customer-facing explanation of a bug fix.

## When to use it

Reach for Claude Code when:

- You're starting a new feature and want a draft you can edit, not a blank file.
- You're debugging something unfamiliar and want an extra pair of eyes that has read the whole repo.
- You need to write a script for a one-off task (rename a bunch of files, build a CSV, summarize a log file).
- You're about to write tests, docs, or commit messages — let Claude take the first pass.
- You're touching a part of the codebase you've never seen before and need an orientation.

Don't use it for:

- Anything that touches customer data without first checking the data-handling rules — confidential customer or PII data shouldn't be pasted into AI tools.
- "Approve and ship without reading" — Claude is fast, not omniscient. Always review the diff before committing.

## How to access it

- **Install:** Available through the GSS [appcart](https://launchpad.globalshopsolutions.dev/apps/appcart) (search "Claude Code"), or follow the [Anthropic install guide](https://docs.claude.com/claude-code).
- **Sign in:** Sign in with your GSS Anthropic account (request a seat from your engineering manager if you don't have one yet).
- **IDE plugin:** VS Code and JetBrains both have a Claude Code extension. Install it and sign in with the same account.

## How to use it

The basic loop:

1. Open a terminal in your project folder.
2. Run `claude` to start a session.
3. Type what you want in plain English. Examples:
   - "What does this script do?"
   - "Add a unit test for the function in `lib/foo.ts`."
   - "Find every place we call the deprecated `getUser()` API."
4. Claude responds, sometimes asking permission to run a command or edit a file. Approve, deny, or redirect.
5. When you're done, the session ends — your project is left as Claude edited it.

### Common patterns

- **"Read this and explain it like I'm new here."** Great for orienting yourself in unfamiliar code.
- **"Make the change, then run the tests, then summarize what changed."** Lets Claude verify its own work before handing back.
- **"Plan this first, then I'll approve."** Use plan mode (`/plan` or the planning toggle) when you want a roadmap before any code changes.

## Common questions

**Does my code get sent to Anthropic?**
Yes — Claude Code sends the parts of your code/files it needs to in order to answer. Anthropic does not train on data sent through the API. See Anthropic's [privacy and data handling docs](https://www.anthropic.com/legal/privacy) for specifics.

**Can I use it on customer data or production secrets?**
Treat it like you would treat any cloud service. Don't paste PII, customer-confidential data, production credentials, or `.env` files into Claude.

**Does it cost me anything?**
GSS pays for Claude API/seat usage at the company level. Heavy usage is fine; you don't have a per-task budget.

**Where do I report a bug or weird behavior?**
For Claude Code itself, file an issue on Anthropic's [GitHub](https://github.com/anthropics/claude-code/issues). For our internal MCPs, ping the engineering team.

**What's the difference between this and ChatGPT or Cursor?**
- **ChatGPT** is a chat web UI (GPT-5.2) — great for general questions, brainstorming, writing assistance. Not deeply integrated with your code.
- **Cursor** is an AI-first IDE — runs alongside your editor and edits inline.
- **Claude Code** is a terminal/CLI agent (Claude Opus 4.7 / Sonnet 4.6) that can read files, run commands, edit code, spawn subagents, and call internal MCPs. It's the most "agentic" of the three: you can give it a task and let it work.

We use all of them at GSS for different things. See the External AI Services section of the catalog.

## How it works

Under the hood, Claude Code is a CLI app that talks to Anthropic's Claude models (Opus 4.7, Sonnet 4.6, Haiku 4.5). When you ask it something:

1. It reads the relevant files in your project (it picks which to read based on what you're asking).
2. It sends those, plus your question, to Claude.
3. Claude responds with either text or a "tool call" — an instruction to read a file, run a shell command, edit something, spawn a subagent, or invoke an MCP tool.
4. Claude Code executes the tool call (with your permission for risky ones), feeds the result back, and the loop continues until your task is done.
5. **Hooks** run automatically in response to events — for example, a `PostToolUse` hook can log every Bash call, or a `PreToolUse` hook can block writes outside the project root.

MCP servers are small services that expose extra tools — like "search internal-tools docs" or "read the COBOL repo" — to Claude. Plugins bundle slash commands, skills, agents, hooks, and MCP server definitions into one installable unit.

## Owner & support

- **Tooling owners (GSS):** Engineering platform team
- **Vendor:** Anthropic
- **Where to file issues:**
  - Claude Code product issues → [Anthropic GitHub](https://github.com/anthropics/claude-code/issues)
  - GSS MCP issues → engineering platform team
- **Last reviewed:** 2026-05-05
