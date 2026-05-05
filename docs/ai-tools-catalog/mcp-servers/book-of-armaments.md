---
title: "MCP Server: book-of-armaments"
description: "Semantic search MCP server for the Helpjuice knowledge base. Lets AI assistants search GSS Helpjuice articles by meaning (not just keywords) and surface..."
sidebar_position: 1
last_updated: 2026-05-05
tags: [ai-tools, gss-internal, mcp-server, claude-code]
---

> **TL;DR** — Semantic search MCP server for the Helpjuice knowledge base. Lets AI assistants search GSS Helpjuice articles by meaning (not just keywords) and surface tribal knowledge from past tickets and write-ups. Built with .NET 10, pgvector, and Ollama local embeddings.

## Overview

Helpjuice has hundreds of internal articles, runbooks, and tribal-knowledge write-ups. Plain keyword search misses anything phrased differently than the article. `book-of-armaments` adds a semantic layer: it embeds every article into a vector store (pgvector) using a local Ollama model, then exposes search over MCP. Claude Code can ask it questions in natural language and get back the most relevant articles regardless of exact wording.

The MCP also exposes tools for tribal-knowledge management — adding new entries, syncing in the background, and surfacing patterns across articles.

## Why use it

- **Find docs by meaning.** "How do we onboard a new Stripe customer?" matches an article titled "Connect Account Setup Flow," even though the words don't overlap.
- **Tribal knowledge capture.** When a tricky problem gets solved, the resolution can be written up and immediately searchable for the next person.
- **Context for Claude.** Instead of guessing or pulling from training data, Claude can cite a real internal article when answering a question.

## When to use it

Auto-triggers when you ask Claude something that probably lives in Helpjuice. Examples:

- "What's the procedure for handling a stuck batch job?"
- "Where's the doc on the new build pipeline?"
- "Has anyone solved an issue where..."

## How to access it

- **LaunchPad page:** [https://launchpad.globalshopsolutions.dev/apps/book-of-armaments](https://launchpad.globalshopsolutions.dev/apps/book-of-armaments)
- **As an MCP:** Connected by default in the GSS Claude Code setup.
- **Status:** Live.

## Common questions

**What language model does the embedding use?**
A local Ollama model (no data sent to external embedding APIs). The articles never leave GSS infrastructure during indexing.

**How fresh is the index?**
The MCP runs a background sync process. Confirm cadence with the owner if you need real-time freshness.

**Can it write to Helpjuice?**
The current scope is search + tribal-knowledge management within the MCP's own store. Helpjuice itself isn't directly modified.

## How it works

- **Ingestion:** Helpjuice articles are pulled, chunked, and embedded with Ollama.
- **Storage:** Vectors live in PostgreSQL with the pgvector extension.
- **Search:** When Claude asks a question, the MCP embeds the query, finds the nearest vectors, and returns the matching articles.
- **Stack:** .NET 10 service + Postgres + Ollama, hosted on LaunchPad.

## Owner & support

- **Owner:** *Listed on the LaunchPad app page.*
- **App page:** [book-of-armaments on LaunchPad](https://launchpad.globalshopsolutions.dev/apps/book-of-armaments)
- **Last reviewed:** 2026-05-05
