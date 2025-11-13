---
title: DevExpress UI Guide
description: Standards for building and modernizing DevExpress WinForms/WPF clients in our ERP
last_updated: 2025-02-20
tags: [devexpress, ui, accessibility]
sidebar_position: 14
---

# DevExpress UI Guide

> **Audience:** Legacy WinForms teams, modernization squads, and QA working on manufacturing screens.

## Project Layout

- WinForms: `src/clients/devexpress/winforms/` (one project per module: ShopFloor, Inventory, QA).
- WPF: `src/clients/devexpress/wpf/` with MVVM helpers in `Common/`.
- Shared assets: `src/clients/devexpress/themes/`, `src/clients/devexpress/resources/`, and localization files under `static/i18n/`.

## Component Standards

- Use **DevExpress 23.x LTS** controls; avoid legacy grids unless a COBOL dependency requires them.
- Naming: `ModuleActionView` for forms, `ModuleActionViewModel` for bindings, `ModuleActionController` for SP2 orchestration.
- Configure `UseDirectXPaint = true` for high-density screens; disable when targeting thin clients.

## Accessibility & Ergonomics

- Minimum font size 12pt; contrast ratio ≥ 4.5:1.
- Provide keyboard paths: set `TabIndex` in logical order and expose `Alt` accelerators for power users.
- Bundle printable cheat-sheets as PDFs in `docs/handouts/` for bug teams; link within tooltips for quick help.

## Data Binding & SP2

- DTOs from SP2 live in `src/services/sp2/Contracts/`; generate binding models via `dotnet run --project scripts/GenerateDtoBindings.csproj`.
- When adding columns bound to COBOL fields, align names with copybook fields to keep Cursor/Codex suggestions accurate.

## Testing Guidance

- UI smoke tests: `npm run test:e2e -- ShopFloor` exercises Playwright + WinAppDriver scenarios.
- Visual regression: capture baselines per theme in `tests/devexpress/baselines/`. Update baselines only when UX approves.
- Manual scenarios: use the printable checklists in `docs/templates.md#manual-ui-checklist` for QA/beta customers.

## AI Pairing Tips

- Create per-module Cursor rules (`.cursor/rules/devexpress-shopfloor.md`) describing SP2 contract names, common grid columns, and validation wording.
- When asking for refactors, include both the `.Designer.cs` and `.cs` files via `@Files` so the AI respects designer-managed code.
- For VB.NET-to-C# migrations, paste representative DataLayer shims and DevExpress handlers so Cursor can preserve event semantics.

## Deployment

- Prefer ClickOnce for shop-floor kiosks; coordinate with Device Mgmt for high-trust machines.
- Tag MSI builds with the same semantic version used by SP2 packages to simplify rollback.
