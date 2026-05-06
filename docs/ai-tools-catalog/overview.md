---
title: "AI Tools at GSS"
description: "Plain-English directory of AI tools, services, and dev infrastructure used at Global Shop Solutions. Written for non-technical readers."
sidebar_position: 1
last_updated: 2026-05-06
tags: [ai-tools, gss-internal, catalog, index]
---

A plain-English directory of the AI tools, services, and related dev infrastructure used at Global Shop Solutions. Each entry below links to a dedicated page that explains what the tool is, why you'd use it, and how to get started — written for a non-technical reader.

> **New here?** Skim the categories below, click into anything that sounds useful, and come back if you need help finding something. If a tool you've heard of isn't listed, [tell us](#missing-something) and we'll add it.

## How to read each article

Every article in this collection follows the same shape, so you can scan any page and find what you need in the same place:

1. **TL;DR** — what it is, who it's for (1–2 sentences).
2. **Overview** — plain-English description.
3. **Why use it** / **When to use it** — the problem it solves and concrete scenarios.
4. **How to access / How to use** — URL, login, the golden path.
5. **Common questions** — short FAQ.
6. **How it works** *(optional)* — the AI/automation behind it, in non-technical terms.
7. **Owner & support** — who to ask if something breaks.

---

## Internal apps on LaunchPad

These are tools built in-house at GSS and hosted on [LaunchPad](https://launchpad.globalshopsolutions.dev/). They use your normal Global Shop Office 365 / SAML login.

### AI & MCP servers (LaunchPad-hosted)

- [agents](launchpad/agents.md) — Registry for sharing and managing agent files across teams.
- [book-of-armaments](mcp-servers/book-of-armaments.md) — Semantic search MCP for the Helpjuice knowledge base. *(LaunchPad app + MCP server.)*
- [bug-fixer](launchpad/bug-fixer.md) — *Stub — owner to fill in.*
- [bug-triage-pack](launchpad/bug-triage-pack.md) — MCP gateway aggregating COBOL, GitHub, Service Web, and test-case tools for bug triage.
- [clinic-utilities](launchpad/clinic-utilities.md) — Returns Clinic company-code info to AI assistants.
- [cobol-mcp](launchpad/cobol-mcp.md) — Lets AI assistants read our COBOL source and SVN history.
- [gab-codeeditor-mcp](launchpad/gab-codeeditor-mcp.md) — *Stub — owner to fill in.*
- [gabmcpserver](launchpad/gabmcpserver.md) — *Stub — owner to fill in.*
- [issue-manager-mcp](launchpad/issue-manager-mcp.md) — *Stub — owner to fill in.*
- [log-parser](launchpad/log-parser.md) — Parses GSS SP2 log files (CoreLog/GSSEO/OCTSRS/ACU traces) into noise-filtered records.
- [mcp-artifacts](launchpad/mcp-artifacts.md) — Team-organized library of AI skills, rules, and agent definitions.
- [mcp-intelligence](mcp-servers/mcp-intelligence.md) — Development intelligence engine. *(LaunchPad app + MCP server.)*
- [pervasiveschema](launchpad/pervasiveschema.md) — Pervasive table/column schema + EO Object mappings exposed over MCP.
- [queue-routing](launchpad/queue-routing.md) — Triages the P&E Bugs queue with AI-recommended team routing.
- [svn-ops](launchpad/svn-ops.md) — General-purpose SVN MCP server (checkout, log, diff, blame, commit, branch).
- [testarchitect-mcp](launchpad/testarchitect-mcp.md) — Exposes TestArchitect test management data to AI assistants.

### QA & testing

- [TABugTracking](launchpad/TABugTracking.md) — Tracks bugs caught by Test Architect tests.
- [TAD-Weekly-Stats](launchpad/TAD-Weekly-Stats.md) — Weekly TestArchitect testing statistics dashboard.
- [QA-daily-standup-rotation](launchpad/QA-daily-standup-rotation.md) — Picks who runs QA standup each day.
- [QA-resolved-issues](launchpad/QA-resolved-issues.md) — Dashboard of issues resolved by QA.
- [bug-traffic-dashboard](launchpad/bug-traffic-dashboard.md) — Real-time view of how service calls flow through P&E Bugs.
- [CausingIssueDashboard](launchpad/CausingIssueDashboard.md) — Issues with their Causing-Issue relationships.
- [program-issue-tracker](launchpad/program-issue-tracker.md) — Search every issue tied to a Program.
- [zen-data-builder](launchpad/zen-data-builder.md) — Generate and manage test data in Actian Zen / Pervasive PSQL.

### Developer tools

- [arc-scanner](launchpad/arc-scanner.md) — Visualizes SQL table usage across our GitHub org.
- [designerconsolidation](launchpad/designerconsolidation.md) — Smart WinForms `.Designer.vb` merge tool.
- [zen-log-parser](launchpad/zen-log-parser.md) — Reads Zen / Pervasive database logs into a tabbed UI.
- [bom-compare-generator](launchpad/bom-compare-generator.md) — Generates BOM .xlsx files for BOM Compare testing.
- [hookmaintenance](launchpad/hookmaintenance.md) — Defines hook ID ranges and manages script hooks.
- [featuretoggles](launchpad/featuretoggles.md) — Controls which features are available in the system.
- [GSPLinkDecoder](launchpad/GSPLinkDecoder.md) — Quickly decodes `gsp://` links.

### Product & docs

- [globe-tv](launchpad/globe-tv.md) — Internal YouTube-like home for help videos.
- [gsshelp-letterpress](launchpad/gsshelp-letterpress.md) — Export Helpjuice articles to Word/PDF on GSS letterhead.
- [career-path-tracker](launchpad/career-path-tracker.md) — Log skills and track your career path quarter by quarter.
- [qchat](launchpad/qchat.md) — Schedule quarterly check-ins.

### Customer-facing & ops

- [gss-stripe-connect](launchpad/gss-stripe-connect.md) — Onboards customers as GSS Stripe Connect Accounts.
- [weekly-performance-dashboard](launchpad/weekly-performance-dashboard.md) — Maintenance & PPT-Bugs team weekly performance.
- [mobile-crm-status](launchpad/mobile-crm-status.md) — Monitors MobileCRM endpoints and pings Teams when one goes down.
- [ihop-rapid-relay](launchpad/ihop-rapid-relay.md) — *Stub — owner to fill in.*
- [rapid-ihop-relay](launchpad/rapid-ihop-relay.md) — *Stub — owner to fill in.*
- [quick-option-import-export-web](launchpad/quick-option-import-export-web.md) — Web tool for importing, editing, and exporting Option JSON files.

---

## Internal MCP servers and concepts

Everything below works with **any MCP-compatible AI agent** — Cursor, Claude Code, Codex, Augment AI, or anything else that speaks Model Context Protocol. Each agent has its own page in the **AI Tools** sidebar (Cursor, Claude Code, Codex, ChatGPT, Augment AI); this section covers the GSS-specific tools your agent can plug into.

> **💡 Easiest MCP setup.** Don't wire each MCP up by hand. Connect your AI agent to **`mcp-intelligence`** — it proxies every other downstream MCP we run, so you get the whole toolkit through one connection. Even easier: ask the agent to do it for you. "Configure my MCP for the GSS internal toolkit" with repo access is enough; the agent writes the config file. See [Cursor](../cursor.md#easy-mcp-setup-at-gss) or [Claude Code](../claude-code.md#easy-mcp-setup-at-gss) for the one-line config block.

### MCP servers (used by any MCP-compatible AI agent)

GSS-internal:

- [mcp-intelligence](mcp-servers/mcp-intelligence.md) — The shared knowledge layer for AI tools at GSS.
- [internal-tools-docs](mcp-servers/internal-tools-docs.md) — Searches the internal-tools docs.
- [internal-tools-status](mcp-servers/internal-tools-status.md) — Health/status checks for internal tools.
- [book-of-armaments](mcp-servers/book-of-armaments.md) — Semantic search over Helpjuice articles.
- [cobol-codebase](mcp-servers/cobol-codebase.md) — Reads our COBOL source repo (used by `cobol-mcp` on LaunchPad).
- [microsoft-365](mcp-servers/microsoft-365.md) — Searches Outlook, SharePoint, and Teams content.
- [devexpress](mcp-servers/devexpress.md) — Searches the DevExpress documentation.

Third-party:

- [GitHub](mcp-servers/github.md) — Search code, manage PRs/issues across the GSS GitHub org.
- [Notion](mcp-servers/notion.md) — Search and read Notion content.
- [monday.com](mcp-servers/monday.md) — Read and update monday.com boards.

### AI agent concepts

Background for anyone configuring or extending an AI coding agent. The framing is Claude Code's terminology; Cursor has direct analogues for each (`.cursor/rules/` for rules, `.cursor/mcp.json` for MCPs, Composer/Cloud Agents for agentic runs).

- [Plugins](concepts/plugins.md) — Bundles of slash commands, skills, agents, hooks, and MCPs.
- [Skills](concepts/skills.md) — Reusable task playbooks (the building blocks of plugins).
- [Hooks](concepts/hooks.md) — Shell commands that fire on events (logging, policy, side effects).
- [Subagents](concepts/subagents.md) — Spawning focused sub-agents for parallel or specialized work.

---

## External AI services

Third-party AI tools the company uses or licenses.

- [Claude Code](../claude-code.md) — Anthropic's CLI/IDE assistant (Opus 4.7, Sonnet 4.6, Haiku 4.5).
- [ChatGPT](../chatgpt.md) — OpenAI's chat assistant (GPT-5.2).
- [Cursor](../cursor.md) — AI-first code editor used for in-IDE pair programming.
- [Codex](../codex.md) — OpenAI's coding agent (GPT-5.2-Codex).
- [Augment AI](../augment-ai.md) — Context-aware coding partner with deep codebase indexing.

> The articles linked above live in the broader **AI Tools** sidebar. They cover external/vendor products in depth. The catalog you're reading focuses on **how those tools fit into GSS** plus our internal apps and MCP servers.

---

## Missing something?

If a tool isn't listed and you think it should be, ping the catalog owner (Travis Jones — tjones@gssmail.com) with the tool name, link if any, and a one-liner about what it does. We add new entries on a rolling basis.

> **Catalog last updated:** 2026-05-06
