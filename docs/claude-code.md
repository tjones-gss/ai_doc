---
title: Claude Code
description: Comprehensive guide to using Claude Code CLI for AI-assisted COBOL modernization
sidebar_position: 5
last_updated: 2026-01-16
tags: [claude, anthropic, ai-tools, cli, agentic-coding]
---

# Claude Code - Agentic Coding CLI

Claude Code is Anthropic's command-line interface for agentic coding, powered by Claude Opus 4.5—the state-of-the-art model for coding, agents, and computer use. It enables long-horizon autonomous coding sessions directly in your terminal.

## Quick Start

### Installation

```bash
npm install -g @anthropic-ai/claude-code
```

### Authentication

```bash
claude login
```

Follow the prompts to authenticate with your Anthropic account or API key.

### Basic Usage

```bash
# Start an interactive session
claude

# Start with a specific task
claude "Analyze this COBOL program's data flow"

# Resume a previous session
claude --resume
```

## Key Features

### Claude Opus 4.5 Model

Released November 24, 2025, Claude Opus 4.5 delivers state-of-the-art performance for coding, agents, and computer use. Key specifications:

- **Pricing:** $5/$25 per million tokens (input/output)
- **API Identifier:** `claude-opus-4-5-20251101`
- **Availability:** Claude apps, API, Amazon Bedrock, Google Cloud Vertex AI

### Agentic Coding

Claude Code operates as an autonomous agent capable of:

- Reading and navigating your entire codebase
- Making multi-file edits across sessions
- Running terminal commands and tests
- Managing long-horizon refactoring tasks

### Plan Mode

Build precise execution plans before making changes:

```bash
claude --plan "Refactor inventory management module"
```

This generates an editable `plan.md` file you can review and modify before execution.

### Desktop App Integration

Run multiple local and remote sessions in parallel through the Claude Desktop application. Ideal for managing several workstreams simultaneously.

### GitHub Integration

Install the Claude GitHub app for seamless PR workflows:

- Automated code review
- PR description generation
- Issue triage and response

## Deep Dive: Claude Opus 4.5 Capabilities

### Effort Parameter

Control reasoning depth with the `effort` parameter:

| Level    | Use Case                             |
| -------- | ------------------------------------ |
| `low`    | Quick tasks, simple queries          |
| `medium` | Daily development work (recommended) |
| `xhigh`  | Complex refactoring, legacy analysis |

```bash
claude --effort xhigh "Analyze cross-program dependencies in this COBOL system"
```

### Context Compaction

For long conversations, Claude Code automatically compacts context to maintain efficiency while preserving critical information. You can manually trigger compaction:

```bash
/compact
```

### Token Efficiency

Claude Opus 4.5 includes improvements for reduced token consumption during extended sessions, making it more cost-effective for enterprise workloads.

## Integration Workflows

### Spec → Plan → Code → Review Methodology

Claude Code fits naturally into the structured workflow:

1. **Spec:** Use ChatGPT Enterprise to research and draft specifications
2. **Plan:** Generate plans with `claude --plan` and refine the `plan.md`
3. **Code:** Execute the plan with Claude Code's agentic capabilities
4. **Review:** Leverage GitHub integration for automated code review

### Working with COBOL Codebases

Claude Code excels at legacy code analysis:

```bash
# Analyze a COBOL program's structure
claude "Map the paragraph flow and PERFORM statements in INVMAINT.cob"

# Identify copybook dependencies
claude "List all COPY statements and their relationships in this program"

# Trace data transformations
claude "Track how WS-INVENTORY-RECORD flows through the program"
```

### GitHub PR Integration

```bash
# Create a PR with AI-generated description
claude pr create --title "Refactor inventory calculations"

# Request AI code review on current branch
claude pr review
```

### Team Collaboration Patterns

- Share `plan.md` files for team review before execution
- Use session exports for knowledge transfer
- Integrate with existing CI/CD pipelines via GitHub app

## Best Practices

### Effective Prompting for Legacy Code

- **Be specific about scope:** "Analyze only the CALCULATE-TOTALS paragraph"
- **Reference copybooks explicitly:** "Include INVENTORY-CPY in your analysis"
- **Request structured output:** "Provide findings in a table format"

### Using Plan Mode for Complex Refactoring

1. Start with `claude --plan` to generate initial plan
2. Review and edit the `plan.md` file
3. Add constraints or clarifications
4. Execute with `claude --execute plan.md`

### Managing Long Coding Sessions

- Use `/compact` periodically to optimize context
- Save session state with `/save`
- Resume interrupted work with `claude --resume`

### Parallel Session Strategies

- Run separate sessions for independent modules
- Use Desktop App to monitor multiple refactoring efforts
- Coordinate via shared `plan.md` files in version control

## Troubleshooting

### Common CLI Issues

| Issue                   | Solution                                       |
| ----------------------- | ---------------------------------------------- |
| Command not found       | Run `npm install -g @anthropic-ai/claude-code` |
| Slow response           | Check network; try `--effort low` for testing  |
| Unexpected token errors | Update to latest version: `npm update -g`      |

### Authentication Problems

```bash
# Clear cached credentials
claude logout

# Re-authenticate
claude login

# Verify authentication status
claude auth status
```

### Session Management Issues

- **Lost session:** Check `~/.claude/sessions/` for recovery
- **Context overflow:** Use `/compact` or start fresh session
- **Stale context:** Reload with `/refresh`

---

## Next Steps

- Configure [Cursor](./cursor.md) for interactive editing
- Set up [Codex CLI](./codex.md) for deep code analysis
- Learn the [Spec → Plan → Code → Review workflow](./workflow.md)
