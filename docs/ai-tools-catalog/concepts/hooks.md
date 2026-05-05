---
title: "Concept: Hooks"
description: "Hooks are shell commands that the Claude Code harness runs automatically in response to events — before/after tool use, when a session starts, on user p..."
sidebar_position: 1
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, concept, claude-code]
---

> **TL;DR** — Hooks are shell commands that the Claude Code harness runs automatically in response to events — before/after tool use, when a session starts, on user prompts. They're how you enforce policy, add logging, or block risky actions without having to trust the model alone.

## Overview

Claude is a model, not a bouncer. If you want a guarantee that something runs before every Bash call, or that nothing ever writes to a protected directory, you can't rely on instructions in CLAUDE.md alone — you need a deterministic gate. That's what hooks are.

A hook is configured in `~/.claude/settings.json` (user-level) or `.claude/settings.json` (project-level). It binds a shell command to an event matcher:

- **`PreToolUse`** — runs before a tool is called. Can block the call by exiting non-zero.
- **`PostToolUse`** — runs after a tool returns. Useful for logging.
- **`UserPromptSubmit`** — runs when the user submits a prompt.
- **`SessionStart`** / **`SessionEnd`** — runs at session boundaries.
- **`Stop`** — runs when Claude wants to end the turn.
- **`PreCompact`** — runs before context compression.

Each hook can match by tool name (`Bash`, `Edit`, etc.) so you can target specific behaviors.

## Why use hooks

- **Hard guarantees.** "Every Bash call gets logged to telemetry" — model can't forget.
- **Policy enforcement.** Block writes to `prod/`, refuse risky commands, require approval for certain actions.
- **Audit trail.** Append every tool call to a file you can review later.
- **Side effects.** Send a Slack notification when Claude finishes a long-running task.

## Examples in use

From the catalog owner's setup (`~/.claude/settings.json`):

```json
"hooks": {
  "PostToolUse": [
    {
      "matcher": "Bash",
      "hooks": [
        { "type": "command", "command": "bash ~/.claude/hooks/log-to-intelligence.sh" }
      ]
    }
  ]
}
```

After every Bash command Claude runs, the harness pipes the result through `log-to-intelligence.sh`, which records it to the [mcp-intelligence](../mcp-servers/mcp-intelligence.md) store. Result: a complete log of every shell action across sessions, without Claude having to remember.

## When to write a hook

- You need a behavior to be **guaranteed**, not requested.
- You want to integrate Claude with an external system (logging, notifications, audit).
- You want to enforce a security policy.

When **not** to:
- The behavior is preference, not policy — use CLAUDE.md or a skill instead.
- You can solve it with a slash command — easier to reason about.

## How hooks differ from skills and plugins

- A **skill** tells Claude how to do something. Claude can ignore or deviate.
- A **plugin** bundles skills/commands/hooks together for distribution.
- A **hook** is the harness running a shell command on its own — Claude isn't even consulted.

If you're thinking "I need this to happen no matter what," reach for a hook.

## Common questions

**Can a hook block Claude's action?**
Yes — a `PreToolUse` hook that exits non-zero will block the tool call.

**Can hooks see the prompt content?**
Yes — they're piped relevant event data via stdin. See the Anthropic docs for the per-event schema.

**What's a good hook to start with?**
A `PostToolUse` matcher on `Bash` that appends every command + result to a local log file. Cheap, useful, and a good way to learn the shape.

**Last reviewed:** 2026-05-05
