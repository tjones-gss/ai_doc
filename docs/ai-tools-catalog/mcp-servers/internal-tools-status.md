---
title: "MCP Server: internal-tools-status"
description: "Lets AI assistants check the live health/status of internal tools (LaunchPad apps, MCP servers, infra) without leaving the Claude session."
sidebar_position: 6
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, mcp-server, claude-code]
---

> **TL;DR** — Lets AI assistants check the live health/status of internal tools (LaunchPad apps, MCP servers, infra) without leaving the Claude session.

## Overview

The companion to [internal-tools-docs](internal-tools-docs.md). Where `docs` answers "how do I use X?", `status` answers "is X up right now?". When you're debugging "why doesn't this work" and the answer might be "the upstream service is down," status checks give Claude the data to tell you that immediately.

## Why use it

Lets Claude rule out the obvious "is it me, or is the service down?" question before sinking time into a non-existent bug.

## When to use it

Auto-triggers when you describe a symptom Claude thinks could be an outage. You can also ask explicitly:

> "Check the status of the artifact-mirroring service."

## How to use it

You don't invoke directly. Claude calls it when relevant.

## Common questions

**Is this the same as a public status page?**
This is internal — covers the LaunchPad-hosted apps and supporting MCP servers, not Anthropic/OpenAI/etc.

**What about external services?**
For those, ask Claude to check the relevant public status page (status.anthropic.com, status.openai.com, etc.). The `internal-tools-status` MCP only covers GSS internals.

## How it works

A backend that polls health endpoints for our internal tools and exposes the current state over MCP.

## Owner & support

- **Owner:** GSS engineering platform team
- **Authentication:** GSS SSO.
- **Last reviewed:** 2026-05-04
