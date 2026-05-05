---
title: "Concept: Plugins"
description: "A Claude Code \"plugin\" is an installable bundle of slash commands, skills, agents, hooks, and MCP server definitions. Install one and Claude gains a coo..."
sidebar_position: 2
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, concept, claude-code]
---

> **TL;DR** — A Claude Code "plugin" is an installable bundle of slash commands, skills, agents, hooks, and MCP server definitions. Install one and Claude gains a coordinated set of new capabilities. Plugins are how teams package and share AI behavior at scale.

## Overview

Claude Code on its own is a smart but generic CLI agent. Plugins are how it becomes opinionated for a specific workflow or team. A plugin is a folder (or marketplace download) containing:

- **`commands/`** — slash commands (e.g., `/plan`, `/review`).
- **`skills/`** — reusable task playbooks Claude can follow.
- **`agents/`** — specialized subagent definitions (e.g., a code-reviewer persona).
- **`hooks/`** — shell commands that run automatically on events.
- **`.mcp.json`** — MCP server definitions to expose extra tools.
- **`plugin.json`** — manifest with name, version, description.

Install a plugin and all of those become available in your Claude Code sessions.

## Why use plugins

- **One install, full toolkit.** Instead of configuring 5 things manually, install one plugin.
- **Versioned and updatable.** Plugins are released by their authors; you pull updates the way you'd update an npm package.
- **Shareable.** A team builds a plugin once, anyone can use it.

## How they're installed

Plugins come from a **marketplace** — usually a Git repo that lists available plugins. Common marketplaces:

- **`claude-plugins-official`** — Anthropic's official marketplace (superpowers, playwright, code-review, frontend-design, etc.).
- **GSS internal marketplace** *(if applicable — confirm with platform team).*
- **Custom marketplaces** — any public Git repo can host one.

Install via `/plugin install <name>` in Claude Code, or by configuring `~/.claude/settings.json` directly.

## Examples already in use at GSS

From the catalog owner's setup:

| Plugin | Source | What it adds |
|---|---|---|
| `superpowers` | `claude-plugins-official` | Brainstorming, plan-writing, parallel agents, TDD, verification |
| `playwright` | `claude-plugins-official` | Browser automation MCP server |
| `claude-md-management` | `claude-plugins-official` | Tools for editing `CLAUDE.md` files |
| `claude-code-setup` | `claude-plugins-official` | Recommendations for Claude Code automation |
| `skill-creator` | `claude-plugins-official` | Helper for authoring new skills |
| `frontend-design` | `claude-plugins-official` | Production-grade UI generation |
| `code-review` | `claude-plugins-official` | PR review skill |
| `supabase` | `claude-plugins-official` | Supabase patterns + MCP |
| `context7` | `claude-plugins-official` | Live library documentation lookup |
| `github` | `claude-plugins-official` | GitHub MCP server |
| `team` | `claude-agent-kit` (third-party) | Multi-agent team coordination |

## When to install a new plugin

- A repeating workflow has a published plugin (don't reinvent it).
- A team you collaborate with has standardized on one.
- You're building a new internal workflow and want it shareable.

When **not** to:
- One-off behavior — just write a quick CLAUDE.md instruction or skill instead.
- Anything that brings code from an unknown source — review the plugin manifest first.

## How it differs from a skill

- A **skill** is one task playbook.
- A **plugin** can contain multiple skills, plus commands, agents, hooks, and MCPs — a coordinated bundle.

Think: skill = recipe; plugin = cookbook + kitchen tools.

## Common questions

**Where do installed plugins live?**
`~/.claude/plugins/cache/<marketplace>/<plugin>/<version>/`. The list of enabled plugins is in `~/.claude/settings.json` under `enabledPlugins`.

**Can plugins read my files or run commands?**
Yes — that's the point of hooks and MCP servers. Always review what a plugin does before installing one from an unfamiliar source.

**Last reviewed:** 2026-05-05
