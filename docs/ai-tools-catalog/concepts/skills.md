---
title: "Concept: Skills"
description: "A Claude Code \"skill\" is a reusable, structured prompt that teaches Claude how to handle a specific kind of task — e.g., \"review a PR,\" \"estimate codeba..."
sidebar_position: 3
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, concept, claude-code]
---

> **TL;DR** — A Claude Code "skill" is a reusable, structured prompt that teaches Claude how to handle a specific kind of task — e.g., "review a PR," "estimate codebase cost," "convert a COBOL program to a grid." Trigger it with a slash command or by mentioning the task. Skills are the building blocks of repeatable AI workflows.

## Overview

Most AI prompting starts with someone typing a long, careful request from scratch. The next person doing the same task does it slightly differently, and quality varies wildly. A **skill** captures the "right way" to ask, once, so anyone can invoke it consistently.

A skill is a Markdown file (`SKILL.md`) with:

- **Frontmatter** — name, description, optional model (`opus`/`sonnet`/`haiku`), allowed tools.
- **Body** — the prompt itself: the steps Claude should take, the rules to follow, the output format.

When a skill is installed and a matching trigger fires, Claude loads the skill body into its context and executes the workflow.

## Why use skills

- **Consistency.** Same task → same approach, every time.
- **Quality.** A skill encodes lessons learned (good and bad) so future runs benefit.
- **Speed.** "Run this skill" is a sentence, not a careful 10-paragraph prompt.
- **Composable.** Skills can call subagents, MCPs, and other skills.

## How they're invoked

Three ways:

1. **Slash command.** A skill can be wired up as `/skill-name`. Type it; Claude runs it.
2. **Auto-trigger.** Skills can declare triggers in their description (e.g., "use this whenever the user asks about X"). Claude loads them automatically when the trigger matches.
3. **Explicit reference.** "Use the code-review skill on this PR."

## Examples in use at GSS

Many of the skills enabled via plugins:

| Skill | From | What it does |
|---|---|---|
| `brainstorming` | superpowers | Explores user intent before any creative work |
| `executing-plans` | superpowers | Runs an approved plan with review checkpoints |
| `dispatching-parallel-agents` | superpowers | Spawns 2+ independent agents for parallel work |
| `code-review` | code-review plugin | Reviews a PR with structured findings |
| `frontend-design` | frontend-design plugin | Builds production-grade UI with high design quality |
| `claude-api` | Anthropic SDK skill | Migrates Anthropic SDK code between Claude versions |
| `update-config` | Anthropic config skill | Configures Claude Code's settings.json safely |

## When to write a new skill

- You catch yourself prompting Claude the same way three times → make it a skill.
- A team wants a workflow standardized → make it a skill, ship it via a plugin.
- A complex task has a known sequence of steps → encoded once, reusable forever.

When **not** to:
- One-off questions or quick fixes.
- Workflows where each instance is meaningfully different.

## How skills relate to plugins

A skill on its own can live in `~/.claude/skills/<name>/SKILL.md`. A skill bundled inside a plugin lives in `<plugin>/skills/<name>/SKILL.md` and ships with the plugin.

Plugins are how skills get distributed; skills are what plugins mostly contain.

## Common questions

**Can a skill use a different model than the rest of my session?**
Yes — set `model: haiku` (or `sonnet` / `opus`) in the frontmatter. Useful for fast, simple skills that shouldn't burn Opus tokens.

**Where can I see what skills are loaded right now?**
Claude Code lists active skills in its system context. You can also browse `~/.claude/plugins/cache/` for plugin-bundled skills.

**Last reviewed:** 2026-05-05
