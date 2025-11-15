---
title: Overview
description: Software Modernization AI Guide for Global Shop Solutions
sidebar_position: 1
last_updated: 2025-10-10
tags: [overview, getting-started, ai-tools]
---

# Software Modernization AI Guide

Welcome to the Software Modernization AI Guide for Global Shop Solutions. This living documentation serves as the evolving source of truth for our AI-assisted development workflow.

## Our Vision

We're not replacing what already works. Our goal is to use AI tools to:

- **Speed up development work**
- **Improve code clarity and documentation**
- **Enhance our existing workflow** without disruption

## Our Environment

We build and maintain COBOL programs using:

- **Visual Studio 2015** with Fujitsu licenses
- **Tortoise SVN** for version control
- **Internal Clinics** for testing and running programs

## The AI Toolkit

Think of our AI tools this way:

| Tool                   | Role                                         | Best For                                                      |
| ---------------------- | -------------------------------------------- | ------------------------------------------------------------- |
| **Cursor**             | Your pair programmer                         | Fast, contextual editing and refactoring within your codebase |
| **ChatGPT Enterprise** | Your senior architect / research assistant   | Research, planning, documentation, architectural ideation     |
| **Codex**              | Your junior dev team handling delegated work | Deep code understanding across files, tests, and architecture |

## Core Workflow: Spec → Plan → Code → Review

Our recommended approach for AI-assisted development:

1. **Spec** - Document what you're building in a file for the agent to reference
2. **Plan** - Have AI generate a thorough gameplan and build a todo list
3. **Code** - Execute the implementation with AI assistance
4. **Review** - Use a fresh AI instance to code review the changes

> **💡 Tip:** Opening a new chat instance for code review helps prevent the previous AI from "gaslighting" you or defending its decisions.

## Documentation Structure

This guide is organized into the following sections:

### AI Tools

- **[Cursor](./cursor.md)** - IDE setup, rules, and best practices
- **[ChatGPT](./chatgpt.md)** - Enterprise features and personalization
- **[Codex](./codex.md)** - CLI and web interface usage

### Methodology

- **[Workflow](./workflow.md)** - Spec → Plan → Code → Review methodology
- **[Memory & Change History](./memory-and-change-history.md)** - Context tracking system

### Stack Guides

- **[COBOL Development Guide](./cobol-development-guide.md)** - Fujitsu NetCOBOL workflows, repository structure, and AI-assisted COBOL development

### Resources

- **[Templates](./templates.md)** - Starter files and examples
- **[Troubleshooting](./troubleshooting.md)** - Common issues and solutions
- **[Prompt Library](./prompt-library.md)** - Reusable prompts for common tasks

## Getting Started

1. **Set up your tools** - Install Cursor, configure ChatGPT Enterprise, and install Codex CLI
2. **Configure your project** - Add rules files and initialize memory banks
3. **Start with a small task** - Test the workflow on a simple change
4. **Iterate and improve** - Update memory and change history as you learn

---

> **⚠️ Note:** This is a living document. As we discover new patterns, best practices, and workflows, this guide will evolve. Always check the last updated date and refer to CHANGE_HISTORY.md for recent modifications.

## Next Steps

Continue to [Cursor Setup](./cursor.md) to configure your AI-powered IDE.
