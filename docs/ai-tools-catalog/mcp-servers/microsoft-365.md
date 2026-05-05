---
title: "MCP Server: microsoft-365"
description: "Lets Claude search your Outlook email, Outlook calendar, Teams chat, SharePoint, and find meeting availability — all through your normal Microsoft 365 s..."
sidebar_position: 8
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, mcp-server, claude-code]
---

> **TL;DR** — Lets Claude search your Outlook email, Outlook calendar, Teams chat, SharePoint, and find meeting availability — all through your normal Microsoft 365 sign-in. Reaches what's already in your inbox/calendar without you leaving a Claude session.

## Overview

A connected MCP server that exposes your Microsoft 365 data (your own — it uses your auth, scoped to what you can see) to Claude Code. Claude can:

- **Search Outlook email** — find threads by subject, sender, content.
- **Search the Outlook calendar** — find events by attendee, topic, date range.
- **Find meeting availability** — answer "when can these 4 people meet next week?".
- **Search Teams chat messages** — find a quote you remember from a Teams thread.
- **Search SharePoint** — find docs and the folders that hold them.
- **Read M365 resources** — pull a specific doc Claude found.

## Why use it

The "where did I see that?" tax is huge. Combining Outlook+Teams+SharePoint search into one natural-language query saves tabs and clicks. Especially valuable for:

- Finding the email thread that contained the customer's original requirement.
- Looking up which SharePoint folder had the architecture diagram.
- Pulling Teams chat context into a current Claude task.

## When to use it

You're asking Claude something and the answer probably lives in your Microsoft 365 data. Examples:

- "Find me last week's email from `<colleague>` about the artifact mirror outage."
- "What's on my calendar Thursday?"
- "Search SharePoint for the GSS Stripe Connect onboarding doc."

## How to access it

Connected by default in the GSS Claude Code setup. First use prompts for Microsoft auth — sign in with your normal `@gssmail.com` account. After that, it just works.

## Common questions

**Does it see other people's email?**
Only what your account already has access to — same as if you searched in Outlook yourself.

**Does my email content go to Anthropic?**
Only the parts Claude needs to answer your question (a search result, a thread Claude opened). It does not bulk-export your inbox. Treat any AI tool as you'd treat an external service: don't ask it to read or summarize confidential customer/PII content unless cleared per data-handling rules.

**Can it send email or accept invites on my behalf?**
The current set of operations is search/read-only. If you need send/write, ask the platform team.

## How it works

OAuth flow with Microsoft to get tokens scoped to your account. The MCP wraps the M365 Graph APIs (mail search, calendar search, Teams search, SharePoint search) and exposes them as MCP tools.

## Owner & support

- **Owner:** GSS engineering platform team
- **Authentication:** Microsoft 365 OAuth (your `@gssmail.com` account).
- **Last reviewed:** 2026-05-04
