# Codex + Cursor Prompt Library

Use these scaffolds as copy/paste starting points. Each template calls out the minimal context bundle so that both Codex and Cursor stay grounded in the right Fujitsu assets.

## 1. Specification drafting

**Context package**

- Program body from `cobol/cobol/fujsource/<PROGRAM>.CBL` (include `ENVIRONMENT DIVISION` and all PERFORM targets).
- All referenced copybooks from `cobol/cobol/fujcopy/` (`*.FD`, `.SL`, `.NM`, `.LK`, `.WS`).
- Matching panel (`panels/<PROGRAM>.PNL` and/or `sp2panels/<Name>Panel/*`).
- Recent compiler output `Projects/<PROGRAM>/<PROGRAM>/listing.txt` so the AI can quote warnings.

**Codex CLI prompt**

````text
You are documenting Fujitsu NetCOBOL module <PROGRAM>. Use the snippet below plus the copybooks
and panel definitions to produce a customer-readable spec.

Context:
- Program: ```<paste fujsource code>```
- Copybooks: ```<paste ASSGN.FD / COMMONSN.CPY excerpts>```
- Panel: ```<paste panel snippet>```
- Listing: ```<paste relevant listing.txt warnings>```

Tasks:
1. Summarize the business goal and triggers.
2. Enumerate every file read/written, keyed by copybook field names.
3. Describe UI fields and validation using SP2 panel labels.
4. Provide acceptance criteria and data setup instructions (reference datasets in `distributables/files`).
5. Flag open questions or missing copybooks.
````

**Cursor IDE prompt**

```text
@explain Create a spec for <PROGRAM> using the open files (CBL, copybooks, panel, listing.txt).
Detail inputs/outputs, PERFORM blocks, SP2 interactions, and test data drawn from FUJPM.
Return Markdown sections: Purpose, Data Sources, UI Notes, Acceptance.
```

## 2. Bug triage / fix

**Context package**

- Failing transaction data or log excerpt.
- Target program from `fujsource/` plus legacy analog from `source/` or `anssource/` if regression suspected.
- Copybooks touched by the suspected RECORD/FILE.
- Snippet of `listing.txt` that shows the compiler/runtime error.

**Codex CLI prompt**

````text
A defect was reported in <PROGRAM>. Symptoms:
<describe data + listing snippet>

Context files:
```<fujsource excerpt>```
```<source/legacy excerpt if relevant>```
```<copybook snippets>```

Tasks for Codex plan mode:
1. Identify the failing paragraph and the field(s) involved.
2. Compare Fujitsu vs legacy implementations and describe the delta.
3. Recommend the minimal code or copybook change, citing impacts on other programs using the same copybooks.
4. Outline rebuild + validation steps (which <PROJECT>.cobproj to compile, what dataset to run).
````

**Cursor IDE prompt**

```text
@explain Help debug <PROGRAM> with the open files. Listing.txt shows <error>. Walk the CALL stack
and copybooks to pinpoint the null value. Suggest a patch but keep Fujitsu syntax.
After proposing the diff, list which copybooks/programs must be retested.
```

## 3. Custom customer requests

**Context package**

- Customer requirement verbatim plus priority/severity.
- Target records/panels (`fujcopy/*.FD`, `panels/*.PNL`) and any repository bindings (`*.REP`).
- Non-Fujitsu dependencies from `nonfujprojects/` if the change crosses into .NET helpers.

**Codex CLI prompt**

````text
Customer request: <paste requirement>.
Relevant assets:
- Program(s): ```<fujsource excerpts>```
- Copybooks: ```<fujcopy snippets>```
- Panels: ```<panel snippet>```
- External libs: ```<nonfujprojects snippet>```

Produce:
1. Impact analysis (data, UI, interfaces, reports).
2. Proposed implementation plan (ordered steps referencing specific files).
3. Risks/unknowns plus questions for the customer.
4. Suggested validation plan referencing datasets from distributables.
````

**Cursor IDE prompt**

```text
@plan We need to implement <customer feature>. Use the pinned files (CBL, copybooks, panel, external
C# helper) to build a multi-step plan: analyze current behavior, identify touchpoints, design data/UI
changes, describe validation. Keep references to exact file paths.
```

## 4. Daily throughput accelerators

**Context package**

- List of program IDs or copybooks scheduled for touch.
- Any scripts (rg queries, build commands) you want Codex/Cursor to reuse.

**Codex CLI prompt**

```text
Create a checklist for today's COBOL work:
Targets: <list of program IDs/copybooks>.
Include for each item: files to open, copybooks to diff, rg searches to run, build commands, and
links to datasets. Return as Markdown so we can paste into standup notes.
```

**Cursor IDE prompt**

```text
@tasks Generate a reusable Composer template that enforces our workflow:
1. Gather program + copybooks from fujsource/fujcopy.
2. Pull listing.txt from Projects/<id>/<id>/.
3. Run msbuild and capture output.
4. Summarize changes + tests.
Output the task definition I can save inside Cursor.
```
