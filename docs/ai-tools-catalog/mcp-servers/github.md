---
title: "MCP Server: GitHub"
description: "Lets your AI agent search code, manage pull requests, read/file issues, and inspect repos across GitHub — all without leaving the chat. Provided by GitH..."
sidebar_position: 4
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, mcp-server, claude-code]
---

# MCP Server: GitHub

> **TL;DR** — Lets your AI agent search code, manage pull requests, read/file issues, and inspect repos across GitHub — all without leaving the chat. Provided by GitHub's official MCP server, connected to your GSS GitHub account.

## Overview

The GitHub MCP server bridges your AI agent and GitHub. Once connected, your AI agent can:

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
- **Issue triage.** "List all open issues mentioning the artifact mirror" becomes one agent turn.
- **Automation.** Your AI agent can open a PR for a small change end-to-end (branch, commit, PR body, request review).

## When to use it

Anytime your task involves GitHub. Auto-triggers based on context.

Don't use it for:
- Anything that involves a customer-confidential repo not yet cleared for AI access.

## How to access it

- **MCP:** Connected by default in the GSS AI tooling setup.
- **Auth:** Uses a GitHub Personal Access Token tied to your account. First use prompts an auth flow.
- **Vendor docs:** [github.com/github/github-mcp-server](https://github.com/github/github-mcp-server)

## Common questions

**Will it push or merge without my permission?**
By default, write actions (open PR, push files, merge) require your confirmation in the AI agent session. Read actions are unrestricted within your token's scope.

**What scope does the token need?**
Typical scopes: `repo`, `read:org`, `workflow`. Don't grant more than needed.

**Does Anthropic see my code through this?**
The GitHub MCP returns content directly to your AI agent session. As with any prompt, the parts your AI agent reads are sent to Anthropic's API to formulate a response. Don't ask your AI agent to read repos containing customer-confidential code without clearance.

## How it works

A small Go service (or hosted equivalent) running between your AI agent and the GitHub REST/GraphQL APIs. Your AI agent calls MCP tools (`get_pull_request`, `search_code`, etc.); the MCP forwards to GitHub using your token; results come back as structured data.

## Owner & support

- **Vendor:** GitHub (official MCP server)
- **Repo:** [github.com/github/github-mcp-server](https://github.com/github/github-mcp-server)
- **GSS account/access:** Use your existing GSS GitHub account; mint a PAT via GitHub Settings → Developer settings.
- **Last reviewed:** 2026-05-05
