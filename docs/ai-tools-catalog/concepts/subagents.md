---
title: "Concept: Subagents"
description: "A subagent is a focused, throwaway sub-Claude that the main Claude session can spawn to handle one specific job. They run in parallel, return a summary,..."
sidebar_position: 4
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, concept, claude-code]
---

> **TL;DR** — A subagent is a focused, throwaway sub-Claude that the main Claude session can spawn to handle one specific job. They run in parallel, return a summary, and don't pollute the main conversation. Use them for research, parallelism, and protecting context.

## Overview

A typical Claude Code session is one model talking to one user. Subagents add a second level: from inside that conversation, Claude can spawn a sub-Claude with its own context, system prompt, and tool access — give it a task — and read back just the result.

Each subagent has:

- A **type** (e.g., `Explore` for fast read-only search, `Plan` for design, `general-purpose` for anything, plus custom ones from plugins).
- A **prompt** — the briefing for the sub-Claude.
- An **isolated context** — what it reads doesn't bloat the main session.

The lead Claude sees only the subagent's final summary, not the 50 file reads it took to produce that summary.

## Why use subagents

- **Parallelism.** Spawn 5 subagents on independent tasks in one message; they run concurrently.
- **Context hygiene.** A research task that reads 20 files would otherwise consume the main session's context. Delegating keeps the main thread lean.
- **Specialization.** Give one subagent a code-reviewer system prompt, another a designer prompt — each behaves like a domain expert.
- **Independent verification.** A reviewer subagent that doesn't see the lead's reasoning gives a second opinion that isn't anchored.

## Common subagent types

From the catalog owner's setup:

| Type | Purpose |
|---|---|
| `Explore` | Fast read-only code search and file lookup |
| `Plan` | Software architect designing implementation plans |
| `general-purpose` | Multi-step tasks where shape isn't predetermined |
| `code-reviewer` | Independent review of a finished piece of work |
| `team:full-stack-developer` | Hands-on implementation of features |
| `team:reviewer` | Pragmatic, non-blocking PR reviews |
| `team:database-admin` | Schema, migrations, RLS, query work |
| `team:shipper` | Owns the commit/push/release pipeline |
| `team:documentor` | Maintains markdown docs |

## When to spawn a subagent

- The task spans 2+ independent things that can run in parallel.
- You need a fresh perspective (review, second opinion).
- A research task would otherwise read so many files it'd blow the main context.
- A specialized agent type is a better fit than the lead.

When **not** to:
- The task is small and sequential — just do it inline.
- You need ongoing back-and-forth with the user — subagents don't have that.

## Examples

**Parallel research:** "Search the codebase for every place we call the Stripe API, every test that mocks Stripe, and every doc page that mentions Stripe." → spawn three `Explore` subagents in one message; merge their findings.

**Independent review:** Lead Claude finishes a plan; spawns a `code-reviewer` subagent with no prior context to review the plan independently. The reviewer has no anchoring on the lead's reasoning.

**Implementation team:** A plan with 5 independent tasks; spawn 5 `team:full-stack-developer` subagents in parallel, each owning one task. They claim from a shared task list and mark complete when done.

## Common questions

**Can subagents spawn their own subagents?**
Generally no — subagents are leaf-level by design to keep things sane.

**Do subagents share memory or files with the lead?**
They share the working directory (so they can read the same files) but not conversation context. Each starts fresh with only the briefing prompt the lead gave them.

**How do they communicate back?**
The subagent returns a single message. The lead reads that message; the rest of the subagent's chain-of-thought is invisible.

**Last reviewed:** 2026-05-05
