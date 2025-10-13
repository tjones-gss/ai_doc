---
title: Codex
description: Developer-focused AI for deep code understanding
sidebar_position: 3
last_updated: 2025-10-10
tags: [codex, cli, code-analysis]
---

# Codex - Your Junior Dev Team

Codex is a developer-focused AI model optimized for code reasoning, refactoring, and automation. Think of it as your junior dev team handling delegated work with deep code understanding across files, tests, and architecture.

## What Is Codex?

Codex is OpenAI's specialized model for code generation and understanding. It excels at:

- Deep code analysis across multiple files
- Understanding complex architectures
- Refactoring and optimization
- Test generation
- Code documentation
- Automated code tasks

## Access Methods

### CLI (Command Line Interface)

The Codex CLI provides a terminal-based interface for interacting with Codex directly from your command line.

### Web Interface

Access Codex through your browser for a more visual experience.

### Cursor Extension

Integrate Codex directly into your Cursor IDE for seamless code assistance.

## CLI Setup

### Prerequisites

Download and install NodeJS if you don't have it already:

**Download:** [https://nodejs.org/en/download](https://nodejs.org/en/download)

### Installation

Open a terminal or PowerShell and run:

```bash
npm install -g @openai/codex
```

### Running Codex CLI

1. Navigate to the directory you want to work in:

```bash
cd C:\path\to\your\project
```

2. Start Codex:

```bash
codex
```

3. Authenticate with your GSS ChatGPT account when prompted

## Using Codex CLI

### Natural Language Commands

In Codex CLI, type natural language commands just like you would speak to a colleague:

```
Analyze the error handling in module CUST-MAINT
```

```
Refactor the file I/O operations to use modern COBOL syntax
```

```
Generate test cases for the calculateDiscount function
```

### Approval Modes

Codex has different approval modes for safety and automation:

#### Read-Only Mode (Planning)

```bash
/approvals read-only
```

Use this mode to plan changes without executing them. Codex will analyze and suggest but not modify files.

#### Auto Mode (Default)

```bash
/approvals auto
```

Codex will make changes automatically but will ask for confirmation on destructive operations.

### Model Selection

Use the advanced Codex model for complex tasks:

```bash
/model gpt-5-codex
```

> **💡 Tip:** Switch to the advanced model when working on complex refactoring or architectural analysis.

### Interrupting Codex

If Codex is going in the wrong direction:

- Press **Ctrl+C** to interrupt
- Refine your instruction
- Try again with more specific context

### Shell Commands

Codex CLI can run shell commands for you. For example:

```
Compile and run program INVMAINT
```

Codex will attempt to execute the necessary commands. It will ask for confirmation before running potentially dangerous operations.

> **⚠️ Note:** Always review Codex's planned actions before confirming. The CLI includes safety checks, but you should verify commands before execution.

## Web Interface

### Accessing Codex Web

**URL:** [https://chatgpt.com/codex](https://chatgpt.com/codex)

**OR**

Access through ChatGPT itself:

1. Log in to ChatGPT Enterprise
2. Select "Codex" from the model dropdown
3. Start coding

### When to Use Web vs CLI

| Use Case                        | Recommended Interface |
| ------------------------------- | --------------------- |
| Quick code analysis             | Web                   |
| Multi-file refactoring          | CLI                   |
| Planning architecture changes   | Web                   |
| Automated batch operations      | CLI                   |
| Code review                     | Web                   |
| Continuous development workflow | CLI                   |

## Cursor Extension

<!-- TODO: add installation instructions for Cursor extension -->

Integrate Codex directly into your Cursor IDE for seamless access to Codex capabilities without leaving your editor.

**Benefits:**

- Context from your current file
- Inline suggestions
- Quick access to Codex analysis
- No context switching

## Comparison with Other Tools

| Tool                   | What It Is                                                                                            | Best Use                                                                                                                     |
| ---------------------- | ----------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| **Codex**              | Developer-focused model optimized for code reasoning, refactoring, and automation (CLI / Web)         | Deep code understanding across files, tests, and architecture                                                                |
| **Cursor**             | AI-powered IDE (VS Code-based) with native AI editing and inline commands                             | Fast, contextual editing and refactoring within your codebase                                                                |
| **ChatGPT Enterprise** | Full-featured conversational environment with advanced reasoning, larger context, and document upload | Research, planning, documentation, architectural ideation, and integration with Company Documentation (SharePoint connector) |

## Best Practices

### 1. Provide Context

Give Codex enough context about your project:

```
We're working on a COBOL inventory management system. The main program is INVMAINT.cob, and it uses copybooks CP-INVHDR and CP-INVDTL. Analyze the data validation logic.
```

### 2. Break Down Complex Tasks

Instead of asking for everything at once, break it down:

```
Step 1: Analyze the current error handling pattern
Step 2: Suggest improvements based on modern COBOL standards
Step 3: Show me a refactored example for one function
```

### 3. Review Before Executing

Always review Codex's proposed changes before accepting them. Use read-only mode for planning, then switch to auto mode for execution.

### 4. Use Version Control

Before running Codex on your codebase:

- Commit your current working state
- Create a backup or branch
- This allows easy rollback if Codex makes unexpected changes

### 5. Iterate and Refine

If Codex's output isn't quite right:

- Interrupt with Ctrl+C
- Provide more specific guidance
- Reference specific files or patterns
- Try again

## Common Use Cases

### Code Analysis

```
Analyze the data flow in ORDPROC.cob and identify potential bottlenecks
```

### Refactoring

```
Refactor the nested IF statements in paragraph VALIDATE-INPUT to use EVALUATE
```

### Documentation Generation

```
Generate detailed comments for all paragraphs in CUST-REPORT.cob
```

### Test Case Generation

```
Create test cases for the calculateTax function, including edge cases
```

### Error Handling Improvement

```
Review error handling in FILE-OPERATIONS section and suggest improvements
```

### Code Migration

```
Identify all instances of deprecated syntax in this program and suggest modern alternatives
```

## Troubleshooting

### Codex Not Responding

- Check your internet connection
- Verify authentication (you may need to re-login)
- Try restarting the CLI

### Unexpected Changes

- Always work in a version-controlled environment
- Use `/approvals read-only` mode first to preview changes
- Revert using your version control system if needed

### Rate Limiting

If you encounter rate limiting:

- Space out your requests
- Enterprise accounts have high limits, but intensive usage may hit caps
- Contact your admin about quota if needed

### Context Confusion

If Codex seems confused about your project:

- Restart the CLI in the correct directory
- Provide more explicit context in your prompt
- Reference specific files by name
- Point to your AGENTS.md or PROJECT_MEMORY.md for context

---

## Next Steps

- Learn about [ChatGPT Enterprise](./chatgpt.md) features
- Explore the [Spec → Plan → Code → Review workflow](./workflow.md)
- See [Templates](./templates.md) for example prompts and workflows
