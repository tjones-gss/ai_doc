---
title: "agents"
description: "A LaunchPad-hosted MCP server for sharing, distributing, and managing agent files across teams. Think of it as the \"agent registry\" for GSS — somewhere ..."
sidebar_position: 7
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, launchpad]
---

> **TL;DR** — A LaunchPad-hosted MCP server for sharing, distributing, and managing agent files across teams. Think of it as the "agent registry" for GSS — somewhere agents live so multiple Claude Code setups can pull from one canonical source.

## Overview

A Claude Code "agent" is a packaged AI personality (system prompt + tools + model + behavior). Engineers build them, but distribution is awkward — people end up emailing files or copy-pasting from Slack. `agents` solves that: a central registry where agents are uploaded, versioned, and pulled into Claude Code automatically.

## Why use it

- **One canonical version.** No more "which copy of the reviewer agent is current?".
- **Cross-team reuse.** A useful agent built by one team can be discovered by another.
- **Versioned.** Updates to an agent flow to everyone who's using it.

## When to use it

- You built a Claude Code agent worth sharing.
- You want to use an agent someone else built without copy-pasting it.

## How to access it

- **URL:** [https://launchpad.globalshopsolutions.dev/apps/agents](https://launchpad.globalshopsolutions.dev/apps/agents) → **Launch**.
- **Login:** Global Shop Office 365 SSO.
- **Status:** Live.
- **Kind:** MCP server.

## How to use it

1. Launch the app.
2. Browse the registry by team / kind / search.
3. To contribute: follow the upload flow (typically a Git push to the underlying repo).
4. To consume from Claude Code: the registry is exposed via MCP, so agents can be referenced directly.

## Owner & support

- **Owner:** hfutrell@gssmail.com
- **App page:** [agents on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/agents)
- **Related apps:** [mcp-artifacts](mcp-artifacts.md), [mcp-intelligence](../mcp-servers/mcp-intelligence.md)
- **Last reviewed:** 2026-05-05
