---
title: Augment AI (Auggie)
description: Comprehensive guide to using Augment AI and Auggie CLI for AI-assisted COBOL modernization
sidebar_position: 4
last_updated: 2026-01-16
tags: [augment, auggie, ai-tools, context-engine, code-review]
---

# Augment AI - Enterprise Code Intelligence

Augment AI is an enterprise-grade AI coding assistant with an industry-leading context engine that maintains a live understanding of your entire codebase. It integrates seamlessly into VS Code, JetBrains IDEs, and the terminal via the Auggie CLI.

## Quick Start (2-3 Minutes)

### Installing Auggie CLI

```bash
# Install via npm
npm install -g @anthropic/auggie

# Verify installation
auggie --version

# Authenticate
auggie auth login
```

### VS Code Extension

1. Open VS Code Extensions (Ctrl+Shift+X)
2. Search for "Augment Code"
3. Click **Install**
4. Sign in when prompted

### JetBrains Extension

1. Open Settings → Plugins
2. Search for "Augment Code"
3. Click **Install** and restart IDE
4. Authenticate via the Augment panel

### Initial Configuration

After installation, Augment automatically indexes your workspace. For large codebases, allow 5-10 minutes for initial indexing.

## Key Features

### Context Engine

Augment's proprietary context engine provides industry-leading codebase understanding:

- **Live Indexing** - Maintains real-time understanding of code, dependencies, architecture, and history
- **Cross-Language Support** - Works across COBOL, copybooks, JCL, and modern languages
- **Semantic Search** - Find code by describing what it does, not just text matching
- **Dependency Mapping** - Understands relationships between programs, copybooks, and data files

> **💡 Tip:** For COBOL modernization, the context engine excels at tracing data flows through copybooks and understanding program call hierarchies.

### IDE Agents

Full-featured AI agents embedded in your IDE:

| Feature              | Description                                        |
| -------------------- | -------------------------------------------------- |
| **Task Lists**       | Break complex work into tracked, manageable steps  |
| **Memories**         | Automatic session persistence across conversations |
| **Multi-File Edits** | Coordinated changes across related files           |
| **Inline Commands**  | Quick edits without leaving your code              |

### Augment CLI (Auggie)

Terminal-based AI coding with full shell integration:

```bash
# Start interactive session
auggie chat

# Ask about specific files
auggie ask "What does CALC-ORDER-TOTAL do?" --file ORDER-PROC.cbl

# Generate code review
auggie review ./src/

# Search codebase semantically
auggie search "error handling for file I/O"
```

### Code Review

AI-powered GitHub PR reviews with actionable feedback:

- **Inline Comments** - Specific, contextual feedback on changed lines
- **One-Click Fixes** - Apply suggested changes directly in IDE
- **COBOL-Aware** - Understands legacy patterns and modernization opportunities
- **Team Standards** - Learns your coding conventions over time

## Deep Dive: Context Engine

### How It Works

The context engine builds a semantic graph of your codebase:

1. **Parsing** - Analyzes source files for structure and symbols
2. **Embedding** - Creates vector representations of code semantics
3. **Graphing** - Maps relationships between components
4. **Indexing** - Enables fast retrieval across the entire codebase

### Optimizing for COBOL

For legacy COBOL codebases:

```markdown
# Recommended workspace structure

project-root/
├── src/ # COBOL source programs
├── copybooks/ # Shared copybooks (add to workspace)
├── jcl/ # Job control language
└── docs/ # Documentation
```

> **⚠️ Note:** Add copybook directories to your workspace to ensure the context engine understands data structures and shared definitions.

## Deep Dive: IDE Agent Workflows

### Task Management

For complex modernization work, use task lists:

```
User: Refactor the ORDER-ENTRY program to use structured exception handling

Agent: I'll break this into tasks:
1. [ ] Analyze current error handling patterns
2. [ ] Design new exception handling structure
3. [ ] Update paragraph EXIT points
4. [ ] Modify copybook error definitions
5. [ ] Test error scenarios
```

### Memory System

Augment automatically maintains memories across sessions:

- **Project Context** - Architecture decisions, coding standards
- **Work History** - Recent changes and their rationale
- **User Preferences** - Your coding style and workflow preferences

Access memories via the Augment panel or:

```bash
auggie memory list
auggie memory add "Use EVALUATE instead of nested IF for multi-condition logic"
```

## Deep Dive: CLI Commands

### Essential Commands

| Command                   | Purpose                       |
| ------------------------- | ----------------------------- |
| `auggie chat`             | Interactive conversation mode |
| `auggie ask "<question>"` | Quick one-off questions       |
| `auggie review <path>`    | Code review with suggestions  |
| `auggie search "<query>"` | Semantic codebase search      |
| `auggie explain <file>`   | Detailed code explanation     |
| `auggie diff`             | Explain staged changes        |

### Shell Integration

Enable rich shell integration:

```bash
# Add to your shell profile (.bashrc, .zshrc, etc.)
eval "$(auggie shell-init)"

# Now use natural language in terminal
$ auggie: find all programs that call CALC-INVENTORY
```

## Deep Dive: Code Review Setup

### GitHub Integration

1. Install the Augment GitHub App from the GitHub Marketplace
2. Configure repository access in your organization settings
3. Enable PR reviews in Augment settings

### Review Configuration

Create `.augment/review.yaml` in your repository:

```yaml
review:
  enabled: true
  auto_review: true # Automatically review new PRs
  focus_areas:
    - error_handling
    - performance
    - cobol_standards
  ignore_patterns:
    - '*.jcl'
    - 'test/**'
```

## Integration Workflows

### Spec → Plan → Code → Review Methodology

Augment integrates into each phase of the development workflow:

| Phase      | Augment Usage                                                             |
| ---------- | ------------------------------------------------------------------------- |
| **Spec**   | Use context engine to analyze existing code and document current behavior |
| **Plan**   | Create task lists, break down complex changes, identify affected files    |
| **Code**   | IDE agents for implementation, Auggie CLI for terminal workflows          |
| **Review** | Automated PR reviews, inline suggestions, one-click fixes                 |

### COBOL Modernization Workflow

```markdown
1. **Analysis Phase**
   - Use `auggie search` to find related programs and copybooks
   - Generate documentation with `auggie explain`

2. **Planning Phase**
   - Create task list in IDE agent
   - Identify all affected copybooks and calling programs

3. **Implementation Phase**
   - Use multi-file editing for coordinated changes
   - Leverage context engine for consistent naming

4. **Review Phase**
   - Run `auggie review` before creating PR
   - Use one-click fixes for suggested improvements
```

### Team Collaboration

- **Shared Memories** - Team-level knowledge persists across developers
- **Consistent Standards** - Context engine learns and enforces conventions
- **Review Insights** - Aggregate learnings from all code reviews

## Best Practices

### Effective Prompting for Legacy Code

When working with COBOL:

```markdown
✅ Good: "Explain the business logic in CALC-ORDER-TOTAL, focusing on the
discount calculations and how WS-DISCOUNT-PCT is used"

❌ Poor: "What does this code do?"
```

> **💡 Tip:** Reference specific paragraph names, copybooks, and data items to get precise answers.

### Using Context Engine for Large Codebases

- **Index Strategically** - Add frequently-used copybook directories to workspace
- **Use Semantic Search** - Describe behavior rather than searching for variable names
- **Leverage Dependencies** - Ask about call hierarchies and data flow

### Code Review Optimization

- Run local reviews before pushing: `auggie review --staged`
- Configure focus areas relevant to your modernization goals
- Use team-level standards to ensure consistent feedback

### Memory Management

```bash
# View current memories
auggie memory list

# Add project-specific knowledge
auggie memory add "CUSTOMER-MASTER file uses key CUST-ID with alternate key CUST-NAME"

# Clear outdated memories
auggie memory remove <memory-id>
```

## Troubleshooting

### Common Setup Issues

| Issue                    | Solution                                               |
| ------------------------ | ------------------------------------------------------ |
| Extension not activating | Ensure you're signed in; check Output panel for errors |
| CLI not recognized       | Verify npm global path is in your system PATH          |
| Authentication failing   | Run `auggie auth logout` then `auggie auth login`      |

### Context Indexing Problems

- **Slow Indexing** - Large codebases take time; check status in Augment panel
- **Missing Files** - Verify files are in workspace and not in `.gitignore`
- **Stale Context** - Use "Reindex Workspace" from command palette

### Extension Conflicts

If Augment conflicts with other extensions:

1. Disable other AI coding assistants temporarily
2. Check for duplicate keybinding conflicts
3. Review extension host logs for errors

### CLI Issues

```bash
# Reset CLI configuration
auggie config reset

# Verbose mode for debugging
auggie --verbose chat

# Check connection status
auggie status
```

---

## Next Steps

- Configure [Cursor](./cursor.md) for complementary AI editing
- Set up [Codex CLI](./codex.md) for deep code analysis
- Learn the [Spec → Plan → Code → Review workflow](./workflow.md)
- Review [ChatGPT Pro 5.2](./chatgpt.md) for research tasks
