# Codex + Cursor Workflow Playbook

## Tooling highlights (late 2025)

**Codex CLI**

- Plan-first sessions now let you declare intent, gather evidence (via `rg`, `Get-ChildItem`, etc.), and then have Codex orchestrate the edit or analysis without losing track of subtasks.
- Multi-command runs show exact shell output, so you can chain file discovery, copybook extraction, and documentation generation while keeping the entire transcript for audits.
- Fine-grained permission requests (`with_escalated_permissions`) make it safe to touch protected assets such as `cobol/cobol/fujcopy/` while keeping history of every write.
- Built-in `apply_patch` editing plus Markdown-aware summaries mean you can capture specs, diffs, and findings without jumping between tools.

**Cursor AI IDE**

- Composer tasks can now run multi-stage plans (gather context -> propose change -> apply diff) with workspace pins so you never lose `fujsource`/`fujcopy` snippets mid-conversation.
- The Context Panel supports permanent pins (`@workspace`, `@open`, `@git`, `@issues`) so SP2 panels, copybooks, and `listing.txt` files stay visible while you iterate.
- Inline `Cmd+I` autocompletions use the same model as chat, keeping Fujitsu-specific syntax and `@OPTIONS` headers intact.
- Terminal Tasks let Cursor kick off `msbuild <project>.cobproj /t:Build` or custom scripts, capture the resulting `listing.txt`, and feed the log directly back into chat for diagnosis.

## Shared preparation patterns

- Start in `cobol/cobol/` and confirm the target program/copybook with `rg --files -g "<id>.CBL"` to avoid editing legacy paths (`source/`, `anssource/`) by mistake.
- Always pull the governing copybooks (`fujcopy/*.FD`, `.SL`, `.NM`, `.LK`) plus any `.REP/.EO` repository bindings before asking either AI tool to reason about data flow.
- When UI work is involved, grab both the `.PNL` version under `panels/` and the `.SP2`/panel directory inside `sp2panels/` so the assistant can map screen fields to storage.
- For build-aware tasks, open the matching project folder under `fujprojects/` or `Projects/` and keep `listing.txt` in the context stack; it anchors warnings to real compiler output.
- Note external dependencies living in `nonfujprojects/` (CacheDLLs, FujitsuHelper, GSSERP.Shared) whenever a COBOL program issues `CALL` statements; include those snippets to keep AI guidance accurate.

## Scenario 1 - Develop specs for assigned conversions

**Codex CLI**

1. Use `rg "PROGRAM-ID. <name>" cobol/cobol/fujsource -n` plus `rg "COPY "OPHDR"" cobol/cobol/fujcopy -n` (or similar) to collect the driving paragraphs and copybooks.
2. Ask Codex to summarize the program structure, highlighting inputs, outputs, and dependencies; include excerpts from `cobol/cobol/source/` or `anssource/` when you want legacy comparisons.
3. Transform that summary into a spec template (Purpose -> Files touched -> Copybooks -> UI -> Acceptance) and write it into `docs/` or `cobol-findings/` so the artifact is diffable.
4. Attach any relevant `Projects/<id>/<id>/listing.txt` snippets that prove the current compile baseline.

**Cursor IDE**

1. Pin the target `.CBL`, the relevant copybooks, and associated `.PNL/.SP2` files in the Context Panel.
2. Use Composer to run a "spec draft" task: instruct it to describe the program, identify every file READ/WRITE, and outline side effects; export the result to Markdown.
3. Iterate inside Cursor chat to refine acceptance criteria or data mappings, referencing repository bindings (e.g., `.REP` -> .NET classes) when needed.
4. Store the spec next to the program folder (or sync it back to Codex) so both tools share a single source of truth.

**Deliverable checklist**

- Scope statement referencing data stores (`OPHDR`, `COMMONSN`, etc.).
- Copybook and panel inventory with version notes (Fujitsu vs legacy).
- Test hooks (where to capture inputs/outputs, which dataset from `distributables/files` to use).
- Pointers to `listing.txt` or other diagnostics.

## Scenario 2 - Bug triage and fixes

**Codex CLI**

1. Reproduce the symptom by searching `fujsource/` and `fujcopy/` for the reported identifiers; lean on plan mode to log every search request.
2. Compare Fujitsu vs legacy implementations when helpful by diffing `fujsource/<id>.CBL` against `source/<id>.CBL` or `anssource/<id>.CBL` inside chat.
3. Draft the fix using `apply_patch`, cite the copybooks affected, and let Codex explain ripple impacts (other programs that `COPY` the modified books).
4. Capture post-change instructions (which `fujprojects/<proj>/<proj>.cobproj` to rebuild, which `listing.txt` lines to inspect).

**Cursor IDE**

1. Create a Cursor task like "Investigate <ticket>" that opens the program, pins the relevant copybooks, and pulls the latest `listing.txt`.
2. Use inline chat to ask for root-cause analysis; share the failing diagnostics or transaction data.
3. Apply the suggested patch via Cursor's diff view, then run the matching `msbuild` command from the integrated terminal; the Task runner can keep retrying until the compile succeeds.
4. Paste the new `listing.txt` warnings back into chat for validation, and export the conversation summary to share with Codex or the team.

## Scenario 3 - Custom customer requests

**Codex CLI**

1. Start with customer verbatim requirements and tag the target dataset (e.g., `distributables/files/FUJPM`), UI panels, and copybooks.
2. Have Codex map the requirement to candidate programs by scanning `Projects/` folders, repository `.REP` bindings, or search hits inside `fujsource/`.
3. Let Codex produce a gap analysis: current behavior vs requested behavior, file impacts, UI impacts, and downstream interfaces (EDI, reports, etc.).
4. Use Codex to draft communication-ready specs or implementation plans that the COBOL team can hand directly to Cursor for coding.

**Cursor IDE**

1. Encode the requirement into Cursor Rules so every chat reminder references the customer context.
2. Use `@workspace` commands to pull every file touched by the plan, then rely on Composer to stage iterative updates (data-layer -> UI -> repositories).
3. When external assemblies (`nonfujprojects/`) are involved, open the corresponding C# files so Cursor can reason about P/Invoke signatures.
4. Capture validation evidence (screenshots, SP2 panel diffs, updated datasets) and link them back to Codex deliverables.

## Scenario 4 - Accelerating daily throughput

- Batch documentation: ask Codex to loop through a list of critical programs and auto-summarize copybook usage, storing the output in `cobol-findings/`.
- Automated checklists: in Cursor, create reusable Composer templates that remind developers to grab copybooks, run listings, and diff `fujcopy` before submitting code.
- Prompt libraries: keep commonly used Codex/Cursor prompts (for specs, bugs, UI tweaks) in version control so the team does not rewrite them each time.
- Build pipelines: use Cursor Terminal Tasks to queue nightly `msbuild` runs on high-traffic `fujprojects/` folders and harvest the resulting logs for Codex analysis.

## Guardrails and data hygiene

- Always note which flavor you are editing (Fujitsu vs legacy vs 64-bit) to avoid cross-contaminating copybooks.
- Sanitize customer data before feeding it to either tool; rely on the curated datasets under `distributables/files/` for reproducible prompts.
- Keep AI-generated specs and changes within the repo so they inherit code review, lint, and audit history.
- Treat `listing.txt` as an artifact just like source code; archive key logs in the same folder as the program so Codex and Cursor can cite them later.
