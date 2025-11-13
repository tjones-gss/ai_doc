---
title: Architecture Overview
description: Layered view of the Global Shop Solutions ERP stack across COBOL, VB.NET, DevExpress, and modern C# services
sidebar_position: 20
last_updated: 2025-02-20
tags: [architecture, erp, stack]
---

# Architecture Overview

> **TL;DR:** Three primary lanes keep the platform moving: Legacy Core (Acu/Fujitsu COBOL + DataLayer), Service & Integration (SP2 service bus, DevExpress front ends), and Modern Cloud (C#/ASP.NET Core). Every contribution should note which lane it touches and how it affects the others.

## Layered Model

1. **Legacy Core (Manufacturing Brain)**
   - Source: `src/cobol/`, `legacy/acu`, and shared copybooks.
   - Languages: Acu/Fujitsu COBOL plus proprietary **DataLayer** modules that expose VB.NET methods for OOP patterns.
   - Responsibilities: Scheduling, materials, shop floor control, and nightly batch jobs.
2. **Service & Integration Plane**
   - SP2 exposes COBOL programs as services for UI/reporting layers; it also hosts integration bridges (EDI, MES hooks).
   - VB.NET service wrappers live under `src/services/sp2/` and surface strongly-typed DTOs to DevExpress screens.
3. **Experience Layer (DevExpress + Web)**
   - DevExpress WinForms/WPF projects live in `src/clients/devexpress/`; theming and localization assets sit in `src/clients/devexpress/themes`.
   - Web or kiosk experiences use ASP.NET Core and TypeScript widgets stored in `src/clients/web/`.
4. **Modern Cloud Extensions**
   - Net-new services (`src/services/dotnet/`) use C# 12, ASP.NET Core, Azure Service Bus, and SQL modern patterns.
   - Domain APIs wrap legacy calls behind a stable interface and emit telemetry to `observability/`.

## Data Flow

- **Transactional loop:** DevExpress UI → SP2 DTO → DataLayer bridge → COBOL program → DB2/iSeries.
- **Event loop:** COBOL batch commits → message dropped on SP2 queue → C# subscriber enriches → downstream analytics.
- **Modernization loop:** C# service calls DataLayer shim first; when a COBOL feature is retired, flip the feature flag in `config/feature-toggles.json` and reroute traffic directly to the managed service.

## Deployment Targets

| Layer                  | Environment                    | Tooling                              |
| ---------------------- | ------------------------------ | ------------------------------------ |
| Legacy COBOL/DataLayer | IBM i / Windows services       | AcuBench, Fujitsu NetCOBOL pipelines |
| SP2 / VB.NET services  | Windows Server IIS             | Octopus Deploy, MSBuild              |
| DevExpress clients     | Windows endpoints              | ClickOnce, MSI, Device Mgmt          |
| Modern C# APIs         | Azure App Service + Containers | GitHub Actions, Bicep                |

## Cross-Cutting Concerns

- **Security:** Use centralized credential vaults; never embed credentials in COBOL copybooks or DevExpress designers.
- **Observability:** All tiers push traces/events to `observability/`; legacy programs emit structured messages via DataLayer logging helpers.
- **Feature Flags:** Maintain per-tier toggles so legacy teams can opt-in gradually; document defaults in `docs/templates.md` when adding new flags.
- **Compatibility Windows:** Publish end-of-support dates for DevExpress/SP2/COBOL compiler versions so planning teams can coordinate upgrades.
