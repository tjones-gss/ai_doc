---
title: Testing Strategy
description: Unified test workflow for COBOL, VB.NET, DevExpress UI, and modern C# services
tags: [testing, ci, qa]
sidebar_position: 12
last_updated: 2025-02-20
---

# Testing Strategy

> **TL;DR:** Run language-specific suites locally, then trigger `npm run test:all` plus `.github` workflows; every PR must show COBOL regression, .NET unit, and UI automation evidence.

## Local Test Matrix

| Stack               | Command                                                                                               | Notes                                                                                                               |
| ------------------- | ----------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------- |
| COBOL (Acu/Fujitsu) | `scripts/cobol/run_harness.sh program-id`                                                             | Uses Acu runtime + DataLayer mocks; outputs to `test-results/cobol/`.                                               |
| DataLayer + VB.NET  | `dotnet test src/services/datalayer/Datalayer.Tests.csproj`                                           | Verifies OOP helpers and COM-visible APIs.                                                                          |
| SP2 Services        | `dotnet test src/services/sp2/Sp2.Services.Tests.csproj`                                              | Includes contract tests ensuring DTO backward compatibility.                                                        |
| DevExpress UI       | `npm run test:e2e`                                                                                    | Playwright suites open packaged WinForms/WPF via WinAppDriver bridge; record new baselines in `playwright-report/`. |
| Modern APIs         | `dotnet test src/services/dotnet/*Tests.csproj` and `npm run test:links` for docs that describe them. |

## CI Guardrails

1. **Build**: `npm run test:build` + `dotnet build` for SP2/modern services.
2. **Static checks**: `npm run lint`, `npm run test:markdown`, `npm run test:frontmatter`, `npm run validate:structure`, and `dotnet format --verify-no-changes`.
3. **Regression**: COBOL harness triggered via GitHub Actions self-hosted runners; attach key logs to the PR.
4. **UI automation**: Playwright artifacts uploaded as build attachments; compare diffs when DevExpress skins change.

## Coverage Expectations

- COBOL/DataLayer critical paths ≥ 80% branch coverage via harness metrics.
- C#/VB.NET unit suites ≥ 85% line coverage; document gaps in PR description.
- Playwright suites must touch every DevExpress module affected by the change; add tags (`@shopfloor`, `@inventory`) for targeted reruns.
- For analytics/telemetry code, include contract tests ensuring schema stability.

## Naming & Structure

- Tests mirror source paths. Example: `src/clients/devexpress/ShopFloor/ShopFloorForm.cs` → `tests/devexpress/ShopFloor/ShopFloorForm.spec.ts` (UI) and `src/clients/devexpress/ShopFloor/ShopFloorForm.Tests.cs` (unit).
- Harness scenarios use `PROGRAMID_scenario.cbl` naming and store fixtures under `tests/cobol/fixtures/`.
- Shared mocks (database, SP2 endpoints) live in `tests/shared/` to avoid drift between languages.

## When to Add New Tests

- Any change to DataLayer contracts or DevExpress bindings.
- Whenever COBOL copybooks gain or remove fields.
- When toggling feature flags or revising SP2 metadata.
- When Cursor/Codex assisted refactors modify more than 30 lines across tiers—capture before/after behavior with regression scripts.
