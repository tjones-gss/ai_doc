---
title: "MCP Server: GitHub"
description: "Lets Claude Code search code, manage pull requests, read/file issues, and inspect repos across GitHub — all without leaving the chat. Provided by GitHub..."
sidebar_position: 4
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, mcp-server, claude-code]
---

> **TL;DR** — Lets Claude Code search code, manage pull requests, read/file issues, and inspect repos across GitHub — all without leaving the chat. Provided by GitHub's official MCP server, connected to your GSS GitHub account.

## Overview

The GitHub MCP server is the bridge between Claude Code and GitHub. Once connected, Claude can:

- **Search code** across repos in the GSS org.
- **Read pull requests** — files, comments, reviews, status checks.
- **Create branches, PRs, and reviews** on your behalf.
- **File issues, comment, and update them.**
- **Inspect commits and history.**
- **Push files** (via API) when the workflow calls for it.

It's a third-party MCP (maintained by GitHub) but central to most engineering workflows here.

## Why use it

- **One-step PR review.** "Summarize what changed in PR #123" pulls the diff, reads it, and answers — no tab-switching.
- **Cross-repo search.** Find every reference to a function or table across the entire org.
- **Issue triage.** "List all open issues mentioning the artifact mirror" becomes one Claude turn.
- **Automation.** Claude can open a PR for a small change end-to-end (branch, commit, PR body, request review).

## When to use it

Anytime your task involves GitHub. Auto-triggers based on context.

Don't use it for:
- Anything that involves a customer-confidential repo not yet cleared for AI access.

## How to access it

- **MCP:** Connected by default in the GSS Claude Code setup.
- **Auth:** Uses a GitHub Personal Access Token tied to your account. First use prompts an auth flow.
- **Vendor docs:** [github.com/github/github-mcp-server](https://github.com/github/github-mcp-server)

## Common questions

**Will it push or merge without my permission?**
By default, write actions (open PR, push files, merge) require your confirmation in the Claude Code session. Read actions are unrestricted within your token's scope.

**What scope does the token need?**
Typical scopes: `repo`, `read:org`, `workflow`. Don't grant more than needed.

**Does Anthropic see my code through this?**
The GitHub MCP returns content directly to your Claude Code session. As with any prompt, the parts Claude reads are sent to Anthropic's API to formulate a response. Don't ask Claude to read repos containing customer-confidential code without clearance.

## How it works

A small Go service (or hosted equivalent) running between Claude Code and the GitHub REST/GraphQL APIs. Claude calls MCP tools (`get_pull_request`, `search_code`, etc.); the MCP forwards to GitHub using your token; results come back as structured data.

## Owner & support

- **Vendor:** GitHub (official MCP server)
- **Repo:** [github.com/github/github-mcp-server](https://github.com/github/github-mcp-server)
- **GSS account/access:** Use your existing GSS GitHub account; mint a PAT via GitHub Settings → Developer settings.
- **Last reviewed:** 2026-05-05
