---
title: "mcp-artifacts"
description: "A team-organized library of AI skills, rules, and agent definitions. The artifacts in this library are read at runtime by the mcp-intelligence MCP serve..."
sidebar_position: 27
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — A team-organized library of AI skills, rules, and agent definitions. The artifacts in this library are read at runtime by the [mcp-intelligence MCP server](../mcp-servers/mcp-intelligence.md) — so the things you save here actually shape how Claude Code behaves across the company.

## Overview

mcp-artifacts is the central library where teams contribute the AI assets that should be available to everyone:

- **Skills** — reusable Claude Code prompts that automate specific workflows.
- **Rules** — shared guidance Claude should follow ("always run tests before committing in repo X," "never paste customer data into prompts").
- **Agent definitions** — pre-configured agent personalities for specific roles (frontend-design, code-reviewer, etc.).

The contents are organized by team so each group can manage their own artifacts.

## Why use it

- **Single source of truth.** Don't reinvent the same skill in three different repos.
- **Discoverability.** Other teams can find and reuse artifacts your team has built.
- **Versioned.** Skills and rules change over time; the library keeps the history.

## When to use it

- You built a useful Claude Code skill or agent and want others to use it.
- You're starting a new project and want to see what skills/rules already exist.
- A team is establishing standards they want Claude to enforce.

Don't use it for:
- Personal-only configurations (those go in your `~/.claude/` folder).
- Customer-facing artifacts.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/mcp-artifacts](https://launchpad.globalshopsolutions.dev/apps/mcp-artifacts) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.
- **Access:** All staff (read). Write/contribute access depends on team membership.

## How to use it

1. Launch the app.
2. Browse by team, kind (skill / rule / agent), or search.
3. Open an artifact to view details, version history, and usage.
4. To contribute: follow the team-specific contribution flow on the app page (typically a PR to a Git repo).

## How it works

mcp-artifacts is a UI on top of a Git-backed library. The [mcp-intelligence MCP server](../mcp-servers/mcp-intelligence.md) reads from the same library at runtime, exposing the artifacts to AI clients (Claude Code) when relevant. So adding a skill or rule here actually changes how Claude behaves in sessions where it's relevant.

## Common questions

**If I add a skill here, will Claude pick it up automatically?**
For shared skills/rules used by mcp-intelligence, yes — typically with a refresh interval. Confirm with the maintainers for cache behavior.

**Can I edit someone else's artifact?**
Generally no — each team owns their artifacts. Discuss with the owning team or contribute to your own team's space.

## Owner & support

- **Owner:** *See app page (no individual owner listed on the tile).*
- **App page:** [mcp-artifacts on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/mcp-artifacts)
- **Related MCP:** [mcp-intelligence](../mcp-servers/mcp-intelligence.md)
- **Last reviewed:** 2026-05-04
