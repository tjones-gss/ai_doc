---
title: Codex 5.2
description: Developer-focused AI for deep code understanding and agentic coding
sidebar_position: 3
last_updated: 2026-01-16
tags: [codex, cli, code-analysis, gpt-5.2-codex, agentic-coding, mcp]
---

# Codex 5.2 - Your Junior Dev Team

Codex is a developer-focused AI model optimized for code reasoning, refactoring, and automation. Think of it as your junior dev team handling delegated work with deep code understanding across files, tests, and architecture.

**Current Version:** CLI v0.87.0 (January 16, 2026) with GPT-5.2-Codex model

## What Is Codex?

Codex is OpenAI's specialized model for code generation and understanding. The latest GPT-5.2-Codex model (released December 18, 2025) is specifically optimized for agentic coding workflows. It excels at:

- Deep code analysis across multiple files
- Understanding complex architectures
- Refactoring and optimization
- Test generation
- Code documentation
- Automated code tasks
- **NEW:** Multi-thread coordination and collaboration
- **NEW:** Skills-based task automation
- **NEW:** MCP (Model Context Protocol) support

## Access Methods

### CLI (Command Line Interface)

The Codex CLI (v0.87.0) provides a terminal-based interface for interacting with Codex directly from your command line.

### Web Interface

Access Codex through your browser for a more visual experience with cloud exec support.

### IDE Extension

Integrate Codex directly into VS Code, Cursor, or Windsurf for seamless code assistance.

### Integrations

Connect Codex with Slack, Linear, and GitHub for team collaboration.

## CLI Setup

### Prerequisites

Download and install NodeJS if you don't have it already:

**Download:** [https://nodejs.org/en/download](https://nodejs.org/en/download)

### Installation

Open a terminal or PowerShell and run:

```bash
npm install -g @openai/codex@0.87.0
```

> **Note:** On Windows, CLI v0.87.0 enables PowerShell UTF-8 by default for improved character encoding support.

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

- `Analyze the error handling in module CUST-MAINT`
- `Refactor the file I/O operations to use modern COBOL syntax`
- `Generate test cases for the calculateDiscount function`

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

Use the GPT-5.2-Codex model for agentic coding tasks:

```bash
/model gpt-5.2-codex
```

> **💡 Tip:** GPT-5.2-Codex is optimized for agentic coding workflows including multi-file refactoring, autonomous task execution, and complex architectural analysis.

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

In CLI v0.87.0, user shell commands run under user snapshot for improved security and isolation.

### Execpolicy (Command Whitelisting)

Configure allowed commands using execpolicy for enterprise security:

```bash
codex --execpolicy allow:git,npm,dotnet
```

### Context Compaction

For long coding sessions, Codex automatically compacts context to maintain performance. You can also manually trigger compaction:

```bash
/compact
```

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
| Cloud exec with branch support  | Web                   |

### Cloud Exec

Run Codex tasks in the cloud with branch support:

```bash
codex cloud exec --branch feature/my-task "Refactor the authentication module"
```

## IDE Extension

The Codex IDE Extension integrates directly into VS Code, Cursor, and Windsurf for seamless access to Codex capabilities without leaving your editor.

### Installation

1. Open your IDE's extension marketplace
2. Search for "OpenAI Codex"
3. Install the official extension
4. Authenticate with your GSS ChatGPT account

**Supported IDEs:**

- Visual Studio Code
- Cursor
- Windsurf

**Benefits:**

- Context from your current file
- Inline suggestions
- Quick access to Codex analysis
- No context switching
- TUI approval requests from spawned threads (v0.87.0)

## Skills System

Skills allow Codex to perform specialized tasks using reusable configurations defined in `SKILL.toml` files.

### Creating a Skill

Create a `SKILL.toml` file in your project:

```toml
[skill]
name = "cobol-modernize"
description = "Modernize COBOL programs with updated syntax"
version = "1.0.0"

[inputs]
source_file = { type = "file", required = true }
target_standard = { type = "string", default = "COBOL-2014" }

[steps]
analyze = "Analyze {source_file} for deprecated syntax"
refactor = "Apply {target_standard} patterns"
validate = "Run validation tests"
```

### Using Skills

Invoke a skill from the CLI:

```bash
codex skill run cobol-modernize --source_file INVMAINT.cob
```

## Integrations

### Slack Integration

Mention @Codex in Slack to trigger coding tasks:

```
@Codex analyze the error handling in the inventory module and suggest improvements
```

Codex will respond in-thread with analysis and can create follow-up tasks.

### Linear Integration

Connect Codex to Linear for issue tracking:

1. Configure Linear integration in Codex settings
2. Reference Linear issues in prompts: `Fix issue LIN-1234`
3. Codex will update issue status as work progresses

### GitHub Integration

Mention @codex on GitHub PRs and issues:

```
@codex review this PR and check for security vulnerabilities
```

Codex can:

- Review pull requests
- Suggest code changes
- Create follow-up issues
- Run automated checks

### Codex SDK

Automate Codex tasks programmatically:

```javascript
import { Codex } from '@openai/codex-sdk';

const codex = new Codex({ apiKey: process.env.CODEX_API_KEY });

const result = await codex.analyze({
  files: ['src/inventory/*.cob'],
  prompt: 'Identify performance bottlenecks',
});
```

## MCP (Model Context Protocol) Support

Codex v0.87.0 supports MCP for enhanced context management and tool integration:

```bash
codex --mcp-server http://localhost:8080
```

MCP enables:

- Custom tool definitions
- External context providers
- Integration with enterprise systems

## Comparison with Other Tools

| Tool                   | What It Is                                                                                            | Best Use                                                                                                                     |
| ---------------------- | ----------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| **Codex 5.2**          | Agentic coding AI with GPT-5.2-Codex model, CLI/Web/IDE integration, Skills, and MCP support          | Deep code understanding, multi-file refactoring, autonomous task execution, and enterprise workflows                         |
| **Cursor**             | AI-powered IDE (VS Code-based) with native AI editing and inline commands                             | Fast, contextual editing and refactoring within your codebase                                                                |
| **ChatGPT Enterprise** | Full-featured conversational environment with advanced reasoning, larger context, and document upload | Research, planning, documentation, architectural ideation, and integration with Company Documentation (SharePoint connector) |

## Best Practices

1. **Provide Context** - Give Codex enough context about your project, including file names and relevant copybooks
2. **Break Down Complex Tasks** - Instead of asking for everything at once, break work into steps
3. **Review Before Executing** - Use read-only mode for planning, then switch to auto mode for execution
4. **Use Version Control** - Commit your current state before running Codex; create a backup or branch
5. **Iterate and Refine** - Interrupt with Ctrl+C, provide more specific guidance, and try again

## Common Use Cases

| Use Case        | Example Prompt                                                                                |
| --------------- | --------------------------------------------------------------------------------------------- |
| Code Analysis   | `Analyze the data flow in ORDPROC.cob and identify potential bottlenecks`                     |
| Refactoring     | `Refactor the nested IF statements in paragraph VALIDATE-INPUT to use EVALUATE`               |
| Documentation   | `Generate detailed comments for all paragraphs in CUST-REPORT.cob`                            |
| Test Generation | `Create test cases for the calculateTax function, including edge cases`                       |
| Error Handling  | `Review error handling in FILE-OPERATIONS section and suggest improvements`                   |
| Code Migration  | `Identify all instances of deprecated syntax in this program and suggest modern alternatives` |

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
- Use `/compact` to trigger context compaction for long sessions

### Multi-Thread Coordination Issues

If using collaboration wait calls for multi-thread coordination:

- Ensure all threads are using the same Codex version (v0.87.0+)
- Check TUI approval requests from spawned threads
- Verify user message metadata is properly configured

---

## Next Steps

- Learn about [ChatGPT Enterprise](./chatgpt.md) features
- Explore the [Spec → Plan → Code → Review workflow](./workflow.md)
- See [Templates](./templates.md) for example prompts and workflows
