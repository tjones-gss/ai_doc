---
title: Legacy + Modern Handbook
description: Shared playbook for COBOL, DataLayer, DevExpress, SP2, and modern C# teams
tags: [handbook, legacy, modernization]
sidebar_position: 11
last_updated: 2025-02-20
---

# Legacy + Modern Handbook

> **Goal:** Keep veteran COBOL teams and modern C# squads aligned on process, tooling, and expectations while modernizing a manufacturing ERP.

## Track Overview

| Track        | Primary Languages                    | Focus                                 | Hand-off Notes                          |
| ------------ | ------------------------------------ | ------------------------------------- | --------------------------------------- |
| Legacy Core  | Acu/Fujitsu COBOL, DataLayer, VB.NET | Batch stability, regulatory logic     | Share copybook diffs + regression notes |
| Transitional | VB.NET, DevExpress, SP2              | UI/Service alignment, defect triage   | Provide DTO contracts + replay scripts  |
| Modern Cloud | C#, ASP.NET Core, TypeScript         | New features, telemetry, integrations | Publish REST schemas + feature flags    |

## Collaboration Rituals

- **Spec → Plan → Code → Review** applies to every track; document which lane owns each step.
- **Design pitches:** use `docs/templates.md#modernization-proposal` and attach both COBOL call graphs and C# diagrams.
- **Change ambassadors:** list a legacy and modern contact in each PR description; they co-review cross-tier changes.
- **Shared vocabulary:** keep the glossary in `docs/overview.md` updated so DevExpress, DataLayer, and C# teams describe modules consistently.

## Coding Guidelines Snapshot

| Area           | Legacy Expectation                           | Modern Expectation                    | Bridge Tip                                         |
| -------------- | -------------------------------------------- | ------------------------------------- | -------------------------------------------------- |
| Error handling | `PERFORM ERR-HANDLER` with DataLayer logging | Fluent `ILogger` with correlation IDs | Map COBOL `ERRCODE` to HTTP/problem+json fields    |
| Data models    | Copybooks + VB.NET wrapper classes           | C# records / minimal DTOs             | Generate VB.NET + C# types from shared JSON schema |
| Testing        | COBOL harness scripts, nightly regression    | `dotnet test`, Playwright suites      | Add smoke tests to both harness + CI job           |

## Knowledge Sharing

- **Legacy-friendly briefs:** Every new modern feature needs a one-page PDF summary (screenshots + job impacts) stored in `docs/handouts/` for bug teams.
- **Modern primers:** Record quick Looms showing how to navigate AcuBench, DataLayer, and SP2 logs so new hires can debug end-to-end.
- **Brown bags:** Alternate topics monthly (COBOL deep dive → DevExpress theming → Azure reliability) to keep empathy high.

## Decision Checklist

1. Did you confirm whether the change touches COBOL copybooks, SP2 services, or DevExpress bindings?
2. Are AI tools (Cursor, Codex) updated with the latest rules and sample prompts from `prompt-library.md`?
3. Have both a legacy and modern reviewer signed off on the plan, not just the code?
4. Did you document rollback steps for each tier involved?
5. Are telemetry and support docs updated so Support/Bug teams can trace incidents across tiers?
