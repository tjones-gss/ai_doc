---
title: ChatGPT Enterprise
description: Your senior architect and research assistant
sidebar_position: 4
last_updated: 2025-10-10
tags: [chatgpt, enterprise, planning, research]
---

# ChatGPT Enterprise - Your Senior Architect

ChatGPT Enterprise is a full-featured conversational AI environment with advanced reasoning, larger context windows, and document upload capabilities. Think of it as your senior architect and research assistant.

## What Is ChatGPT Enterprise?

ChatGPT Enterprise provides:

- **Advanced reasoning capabilities** - Deep analysis and strategic thinking
- **Larger context windows** - Handle more information at once
- **Document upload** - Analyze Word docs, PDFs, spreadsheets
- **Data privacy** - Your data is NOT used for training
- **SharePoint integration** - Connect to company documentation
- **Code interpreter** - Run Python code for data analysis
- **Custom GPTs** - Create specialized AI assistants for specific tasks

## Key Features

### Data Privacy

> **🔒 Security:** Data is private by default and not used for training. You can safely discuss internal code, business logic, and proprietary information.

This makes ChatGPT Enterprise ideal for:

- Discussing sensitive code implementations
- Analyzing business requirements
- Planning architecture for proprietary systems
- Reviewing confidential documentation

### Advanced Reasoning

ChatGPT Enterprise excels at:

- **Strategic planning** - "How should we approach modernizing this legacy system?"
- **Architectural decisions** - "What are the tradeoffs between these two approaches?"
- **Research** - "What are the best practices for error handling in COBOL?"
- **Problem-solving** - "Why might this approach cause issues at scale?"

### Document Upload and Analysis

Upload and analyze various document types:

- **Word documents** - Requirements, specifications, documentation
- **PDFs** - Technical manuals, standards, reports
- **Spreadsheets** - Data analysis, test case matrices
- **Code files** - For review and analysis

Example use cases:

```
Upload: Business_Requirements_v3.docx
Prompt: "Extract all data validation rules and create a summary table"
```

```
Upload: Legacy_System_Architecture.pdf
Prompt: "Identify all database interactions and create a data flow diagram"
```

### Code Interpreter

Enable code interpreter for tasks like:

- Analyzing log files
- Converting data formats
- Generating test data
- Parsing complex file structures
- Creating visualizations of code metrics

> **💡 Tip:** While not directly needed for COBOL development, code interpreter is great for auxiliary tasks like analyzing SVN logs, parsing clinic output, or converting data formats.

## Personalization

### Custom Instructions

Custom Instructions is a feature in the Settings menu where you provide information about yourself and specify desired response styles, tones, and formats.

**Location:** Settings > Personalization > Custom Instructions

This allows ChatGPT to act as a more tailored and effective assistant by reflecting your needs, work style, and preferences without needing to repeat them in every prompt.

#### Example Custom Instructions

**About you:**

```
I'm a COBOL developer at Global Shop Solutions working on legacy system modernization.
I work with Visual Studio 2015, Fujitsu COBOL, and Tortoise SVN.
Our focus is on improving code clarity and documentation while maintaining existing functionality.
I prefer practical, actionable advice with code examples.
```

**How ChatGPT should respond:**

```
Act as my expert assistant with access to all your reasoning and knowledge.

Always provide:
1. A clear, direct answer to my request
2. A step-by-step explanation of how you got there
3. Alternative perspectives or solutions I might not have thought of
4. A practical summary or action plan I can apply immediately

Never give vague answers. If the question is broad, break it into parts.
If I ask for help, act like a professional in that domain (teacher, coach, engineer, etc.).
Push your reasoning to 100% of your capacity.

Ask me questions until you're 95% confident you understand what I'm looking for.
```

### Projects

Projects are dedicated "smart workspaces" or organizational folders for long-running tasks.

**What Projects Do:**

- Group relevant chats together
- Store uploaded reference files
- Apply custom instructions specific to the project
- Maintain context across multiple conversations

**Example Project Structure:**

**Project: COBOL Modernization - Inventory Module**

- Reference files:
  - INVMAINT.cob
  - CP-INVHDR.cpy
  - Business_Requirements.docx
  - Data_Dictionary.xlsx
- Custom instructions: "Focus on maintaining backward compatibility. Reference the data dictionary for all field definitions."
- Chats:
  - "Error Handling Review"
  - "Performance Optimization Ideas"
  - "Test Case Generation"

> **💡 Tip:** Create a separate project for each major module or system you're working on. This keeps context organized and relevant.

## Common Use Cases

### Q&A and General Guidance

```
What's the difference between INSPECT and UNSTRING in COBOL?
```

```
How should I handle file locking in a multi-user COBOL environment?
```

### Code Review and Analysis

```
[Upload CUSTMAINT.cob]
Review this code for potential issues with data validation and error handling.
```

### Generating Auxiliary Scripts

```
Generate a PowerShell script to batch compile all .cob files in a directory
```

```
Create a Python script to parse SVN log and extract all changes to INVMAINT.cob
```

### SQL and Data Tasks

```
Write a SQL query to extract all customer records modified in the last 30 days for testing
```

```
Generate test data for the CUSTOMER table with realistic values for 100 records
```

### Regular Expressions

```
Create a regex to validate customer ID format: XXX-NNNN where X is letter, N is number
```

### Documentation Generation

```
[Upload multiple .cob files]
Create a markdown documentation page describing the relationship between these programs
```

### Planning and Architecture

```
We need to add a new audit trail feature to track all changes to customer records.
What approach would you recommend, and what are the tradeoffs?
```

## SharePoint Integration

ChatGPT Enterprise can connect to your company's SharePoint to access:

- Internal documentation
- Standards and guidelines
- Architecture diagrams
- Historical knowledge

<!-- TODO: add SharePoint connector setup instructions -->

> **💡 Tip:** Reference company documentation directly in your prompts for more contextual answers aligned with your organization's standards.

## Best Practices

### 1. Start Projects for Major Initiatives

Create a dedicated project for each significant modernization effort. Upload reference documentation and set custom instructions.

### 2. Use for Planning Before Coding

Before jumping into Cursor or Codex:

1. Discuss the approach in ChatGPT
2. Analyze requirements
3. Identify potential issues
4. Create a plan

Then execute in Cursor/Codex with the plan in hand.

### 3. Leverage Document Upload

Upload requirements, specs, or existing code for analysis rather than pasting large amounts of text into the chat.

### 4. Ask for Alternatives

Don't settle for the first answer:

```
What are 3 different approaches to solving this problem? List pros and cons of each.
```

### 5. Use for Research

When you encounter unfamiliar concepts or need to understand best practices:

```
Explain the COBOL SORT verb with practical examples for our inventory system
```

### 6. Create Specialized GPTs

Build custom GPTs for repeated tasks:

- **COBOL Code Reviewer** - Trained on your coding standards
- **Test Case Generator** - Creates test scenarios based on your templates
- **Documentation Assistant** - Formats documentation in your style

## Integration with Other Tools

### ChatGPT → Cursor Workflow

1. **Plan in ChatGPT:**
   - Analyze requirements
   - Design approach
   - Identify risks

2. **Export plan to Cursor:**
   - Copy the plan
   - Add to PROJECT_MEMORY.md
   - Reference in Cursor prompts

3. **Execute in Cursor:**
   - Use the plan as context
   - Reference it with @Files

### ChatGPT → Codex Workflow

1. **Research in ChatGPT:**
   - Understand the problem space
   - Identify patterns

2. **Execute in Codex:**
   - Apply learnings to actual codebase
   - Use Codex for implementation

### Full Workflow Example

```
ChatGPT: "Analyze this requirement and create an implementation plan"
↓
[Export plan to PROJECT_MEMORY.md]
↓
Cursor: "@PROJECT_MEMORY.md Implement the first step of the plan"
↓
Codex: "Analyze the changes for potential issues"
↓
ChatGPT: "Review this diff and suggest improvements"
```

## Troubleshooting

### Response Too Generic

- Provide more context in your prompt
- Upload relevant files
- Use Custom Instructions to set your domain
- Ask follow-up questions to narrow down

### Lost Context

- Use Projects to group related conversations
- Reference previous conversations: "In our earlier discussion about..."
- Upload reference files to the project

### Code Interpreter Not Available

- Check your Enterprise account settings
- Ensure you've enabled Advanced Data Analysis
- Contact your admin if unavailable

---

## Next Steps

- Set up your [Custom Instructions](https://chatgpt.com/settings)
- Create your first [Project](https://chatgpt.com/projects)
- Learn the [Spec → Plan → Code → Review workflow](./workflow.md)
- Explore [Templates](./templates.md) for example prompts
