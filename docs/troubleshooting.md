---
title: Troubleshooting
description: Common issues and solutions
sidebar_position: 8
last_updated: 2025-10-10
tags: [troubleshooting, faq, common-issues]
---

# Troubleshooting

This page covers common issues you may encounter when using AI tools for development and how to resolve them.

## Cursor Issues

### AI Hallucinating or Off-Track

**Symptoms:**

- Cursor generating incorrect or nonsensical code
- Suggestions don't match your project patterns
- AI seems confused about file structure or relationships

**Solutions:**

1. **Refresh context:**
   - Open relevant files in editor
   - Close unrelated tabs
   - Use `Ctrl+Shift+P` → "Reload Window"

2. **Verify memory files:**
   - Check `PROJECT_MEMORY.md` for accuracy
   - Look for "poisoned" or outdated information
   - Update or correct stale entries

3. **Provide explicit context:**

   ```
   @PROJECT_MEMORY.md
   @CUSTMAINT.cob
   @CP-CUSTHDR.cpy

   [Your specific request with clear context]
   ```

4. **Break down the request:**
   - Instead of asking for everything at once
   - Request one small change at a time
   - Build incrementally

### Cursor AI Unresponsive

**Symptoms:**

- Cursor doesn't respond to prompts
- Long delays or timeouts
- "AI is not available" messages

**Solutions:**

1. **Check authentication:**
   - Possibly re-login to Cursor
   - Verify API keys haven't expired
   - Check your account status

2. **Test with smaller prompt:**
   - Try a simple "Hello" or basic request
   - If it responds, the issue was prompt size/complexity
   - Break down your original request

3. **Restart Cursor:**
   - Close and reopen Cursor IDE
   - Use `Ctrl+Shift+P` → "Reload Window"

4. **Check network:**
   - Verify internet connection
   - Check firewall settings
   - Try switching networks if possible

### Context Lost Mid-Conversation

**Symptoms:**

- AI "forgets" earlier parts of conversation
- Contradicts previous statements
- Asks for information already provided

**Solutions:**

1. **Summarize the conversation:**

   ```
   To summarize our conversation so far:
   - We're working on adding audit trail to CUSTMAINT
   - We've created the copybook CP-CUST-AUDIT
   - We're now adding the logging logic to the update section

   [Continue with your request]
   ```

2. **Start a new chat:**
   - Open fresh chat window
   - Provide comprehensive context upfront
   - Include @files and @memory references

3. **Pin important files:**
   - Use "Add Context" to pin files
   - Keep memory files always included
   - Reference with @ in each prompt

### Large Output Gets Cut Off

**Symptoms:**

- AI response stops mid-sentence
- Code is incomplete
- "..." at end of response

**Solutions:**

1. **Request continuation:**

   ```
   Continue from where you left off
   ```

2. **Request specific section:**

   ```
   Show me the complete implementation of the VALIDATE-CREDIT-LIMIT paragraph
   ```

3. **Break into smaller requests:**
   - Ask for one function/paragraph at a time
   - Build the complete file incrementally

### Rules Not Being Followed

**Symptoms:**

- AI ignores coding standards
- Doesn't follow patterns from PROJECT_MEMORY
- Uses wrong naming conventions

**Solutions:**

1. **Verify rules location:**
   - Check `.cursor/rules/` directory exists
   - Verify files have `.md` extension
   - Ensure rules are properly formatted

2. **Reference rules explicitly:**

   ```
   @.cursor/rules/coding-standards.md
   @PROJECT_MEMORY.md

   Following our coding standards, implement...
   ```

3. **Keep rules file open:**
   - Open `.cursorrules` or rules file in a tab
   - Helps AI maintain context across reloads

4. **Use AGENTS.md:**
   - Consider switching from `.cursor/rules/` to `AGENTS.md`
   - Sometimes more effective for context retention

## ChatGPT Issues

### Responses Too Generic

**Symptoms:**

- Answers lack project-specific context
- Generic advice that doesn't apply to your situation
- Missing domain knowledge

**Solutions:**

1. **Provide more context:**
   - Upload relevant files
   - Share code snippets
   - Describe your specific environment

2. **Use Custom Instructions:**
   - Settings → Personalization → Custom Instructions
   - Describe your role, tech stack, and preferences
   - Include project-specific terminology

3. **Create a Project:**
   - Group related conversations
   - Upload reference documentation
   - Set project-specific instructions

4. **Ask follow-up questions:**
   ```
   That's helpful, but in the context of COBOL and Fujitsu syntax specifically, how would I...
   ```

### Lost Context Between Sessions

**Symptoms:**

- New conversation doesn't remember previous discussions
- Have to re-explain project repeatedly

**Solutions:**

1. **Use Projects feature:**
   - Create project for your codebase
   - Upload key reference files
   - All conversations in project share context

2. **Reference previous conversations:**

   ```
   In our earlier discussion about error handling, you suggested...
   Now I want to apply that to...
   ```

3. **Maintain PROJECT_MEMORY.md:**
   - Export learnings from ChatGPT to memory file
   - Upload memory file to new conversations

### Code Interpreter Not Available

**Symptoms:**

- Can't run Python code for analysis
- "Code Interpreter" option missing

**Solutions:**

1. **Check account type:**
   - Verify you have Enterprise access
   - Code Interpreter may be called "Advanced Data Analysis"

2. **Enable in settings:**
   - Check ChatGPT settings
   - Look for "Advanced Data Analysis" toggle

3. **Contact administrator:**
   - May need to be enabled by org admin
   - Confirm it's included in your plan

## Codex CLI Issues

### Codex Not Responding

**Symptoms:**

- CLI hangs or doesn't respond
- Commands don't execute
- Connection errors

**Solutions:**

1. **Check internet connection:**
   - Verify network access
   - Test with `ping google.com`

2. **Re-authenticate:**

   ```bash
   codex
   # Follow authentication prompts again
   ```

3. **Restart Codex:**
   - `Ctrl+C` to stop
   - Run `codex` again

4. **Update Codex:**
   ```bash
   npm update -g @openai/codex
   ```

### Unexpected Changes to Files

**Symptoms:**

- Codex made changes you didn't expect
- Files modified without confirmation
- Wrong files affected

**Solutions:**

1. **Use read-only mode first:**

   ```bash
   /approvals read-only
   ```

   Review plan before switching to auto mode

2. **Revert via version control:**

   ```bash
   svn revert [FILENAME]
   # or
   svn update -r [REVISION] [FILENAME]
   ```

3. **Interrupt and clarify:**
   - Press `Ctrl+C` to stop
   - Provide more specific instructions
   - Explicitly list files to modify/avoid

4. **Work in a branch:**
   - Always work in version-controlled environment
   - Easy to revert or compare changes

### Rate Limiting Errors

**Symptoms:**

- "Rate limit exceeded" errors
- Requests being throttled
- Temporary blocks

**Solutions:**

1. **Space out requests:**
   - Don't rapid-fire multiple requests
   - Wait a minute between large operations

2. **Check usage:**
   - Enterprise accounts have high limits
   - Contact admin if consistently hitting limits

3. **Use different tool:**
   - Switch to Cursor for some tasks
   - Use ChatGPT for planning while waiting

### Context Confusion

**Symptoms:**

- Codex seems confused about project structure
- References wrong files or paths
- Doesn't understand codebase

**Solutions:**

1. **Restart in correct directory:**

   ```bash
   cd C:\path\to\your\project
   codex
   ```

2. **Provide explicit context:**

   ```
   I'm working on a COBOL project. The main program is CUSTMAINT.cob
   in the src/ directory. It uses copybooks from copybooks/ directory.

   [Your request]
   ```

3. **Reference memory files:**
   ```
   Read PROJECT_MEMORY.md for project context, then analyze CUSTMAINT.cob
   ```

## General AI Issues

### AI Suggesting Insecure or Bad Practices

**Symptoms:**

- Recommendations violate security best practices
- Suggests deprecated or discouraged patterns
- Code has obvious vulnerabilities

**Solutions:**

1. **Challenge the AI:**

   ```
   That approach seems to have security implications. What are the risks?
   Suggest a more secure alternative.
   ```

2. **Provide constraints:**

   ```
   Implement this feature ensuring:
   - No SQL injection vulnerabilities
   - Proper input validation
   - Error messages don't leak sensitive info
   ```

3. **Use code review step:**
   - Always run code review with fresh AI
   - Explicitly ask about security implications

4. **Consult PROJECT_MEMORY:**
   - Document security patterns in memory
   - Reference in prompts for consistency

### AI Making Assumptions

**Symptoms:**

- AI fills in details you didn't specify
- Makes changes beyond what you asked
- Adds "helpful" features you don't want

**Solutions:**

1. **Be more explicit:**

   ```
   ONLY add the credit limit validation.
   Do NOT modify any other logic.
   Do NOT add additional features.
   ```

2. **Use spec files:**
   - Document exactly what's in/out of scope
   - Reference spec in every prompt

3. **Review before accepting:**
   - Always review diffs
   - Question unexpected changes
   - Ask AI to explain reasoning

### Version Control Conflicts

**Symptoms:**

- AI-generated code conflicts with repo
- SVN merge conflicts
- Uncommitted changes lost

**Solutions:**

1. **Commit frequently:**
   - Commit after each successful step
   - Easy to revert or compare

2. **Use separate WIP:**
   - Work in separate directory with own SVN
   - Merge to main repo when stable

3. **Review diffs before committing:**

   ```bash
   svn diff CUSTMAINT.cob
   ```

4. **Use branches (if available):**
   - Branch for AI-assisted work
   - Merge when tested and approved

### Memory Files Getting Stale

**Symptoms:**

- PROJECT_MEMORY has outdated information
- CHANGE_HISTORY doesn't reflect recent work
- AI following old patterns

**Solutions:**

1. **Schedule regular reviews:**
   - Weekly or monthly review of memory
   - Update with recent learnings

2. **Update immediately after discoveries:**

   ```
   @PROJECT_MEMORY.md

   Update memory with today's learnings:
   - [New pattern discovered]
   - [Updated approach to X]
   ```

3. **Archive old entries:**
   - Move old CHANGE_HISTORY to archive files
   - Keep current memory focused and relevant

4. **Use prompts to refresh:**

   ```
   @PROJECT_MEMORY.md

   Review this memory file. Identify outdated entries and suggest updates.
   ```

## Performance Issues

### Slow AI Responses

**Symptoms:**

- Long wait times for responses
- Timeouts
- Poor performance

**Solutions:**

1. **Reduce context size:**
   - Don't include entire large files
   - Reference specific sections or functions
   - Close unnecessary tabs in Cursor

2. **Use faster model:**
   - Switch to GPT-3.5 for simple tasks
   - Save GPT-4/Claude for complex reasoning

3. **Break down requests:**
   - Smaller, focused requests respond faster
   - Build incrementally

4. **Check network:**
   - Slow internet affects response time
   - Try different network if possible

### High Token Usage / Costs

**Symptoms:**

- Hitting usage limits quickly
- High API costs
- Context window warnings

**Solutions:**

1. **Be concise:**
   - Don't paste entire files if not needed
   - Summarize instead of including everything

2. **Use cheaper models for simple tasks:**
   - GPT-3.5 for basic questions
   - GPT-4 only when needed

3. **Clear old conversations:**
   - Start fresh chats for new topics
   - Don't let context accumulate unnecessarily

4. **Optimize memory files:**
   - Keep them focused and lean
   - Archive old information

---

## Getting Help

If you're still stuck after trying these solutions:

1. **Check memory files:**
   - Review PROJECT_MEMORY.md and CHANGE_HISTORY.md
   - Look for similar past issues and solutions

2. **Consult the team:**
   - Ask colleagues if they've encountered similar issues
   - Share learnings in PROJECT_MEMORY

3. **Review documentation:**
   - [Cursor Docs](https://cursor.com/docs)
   - [OpenAI Documentation](https://platform.openai.com/docs)
   - [ChatGPT Help](https://help.openai.com/)

4. **Document the solution:**
   - When you find a fix, add it to PROJECT_MEMORY
   - Help future you and teammates

---

## Next Steps

- Review [Prompt Library](./prompt-library.md) for effective prompts
- Explore [Templates](./templates.md) for starter files
- Check [Workflow](./workflow.md) for best practices
