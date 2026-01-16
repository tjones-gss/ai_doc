---
title: QA Analyst AI Workflows
description: Leverage AI tools for test case generation, bug analysis, automated testing, and quality assurance in COBOL modernization projects
sidebar_position: 8
last_updated: 2026-01-16
tags: [qa, testing, automation, test-cases, bug-analysis, cobol, quality-assurance]
---

# QA Analyst AI Workflows

## Introduction

AI tools are transforming quality assurance workflows for legacy COBOL modernization projects. By leveraging ChatGPT Pro 5.2, Codex 5.2, Claude Code, and Augment AI, QA analysts can dramatically accelerate test case generation, automate bug analysis, and ensure comprehensive coverage of complex business logic.

This guide provides practical workflows and templates specifically designed for QA analysts working with Fujitsu COBOL and Visual Studio 2015 at Global Shop Solutions.

### Why AI for QA?

| Traditional Approach              | AI-Enhanced Approach               |
| --------------------------------- | ---------------------------------- |
| Manual test case creation (hours) | Automated generation (minutes)     |
| Limited edge case coverage        | Comprehensive boundary analysis    |
| Time-consuming bug reproduction   | Rapid root cause identification    |
| Reactive testing                  | Predictive test prioritization     |
| Documentation backlog             | Real-time documentation generation |

---

## Test Case Generation

### Using ChatGPT Pro 5.2 for Test Case Design

ChatGPT Pro 5.2 excels at understanding business requirements and generating comprehensive test scenarios:

```markdown
Template: Generate test cases for this COBOL program that handles [describe functionality]

Context:

- Program Name: [PROGRAM.cob]
- Business Function: [What the program does]
- Input Files/Parameters: [List inputs]
- Output Files/Results: [List outputs]
- Database Tables: [Tables accessed]

Generate test cases covering:

1. Happy path scenarios
2. Boundary conditions
3. Error handling paths
4. Edge cases specific to COBOL (spaces, zeros, packed decimal)
5. Integration points with other programs

For each test case provide:

- Test Case ID
- Description
- Preconditions
- Test Steps
- Expected Results
- Test Data
```

#### Example: Accounts Receivable Test Cases

```markdown
Generate test cases for ARCALC.cob that handles accounts receivable aging calculations

Context:

- Program calculates aging buckets (Current, 30, 60, 90, 120+ days)
- Input: INVOICE-MASTER file, PAYMENT-HISTORY file
- Output: AGING-REPORT file, AR-SUMMARY record
- Uses customer credit limits from CUST-MASTER

Focus on:

- Date calculation edge cases (leap years, month boundaries)
- Multiple partial payments on single invoice
- Credit memo applications
- Currency rounding (2 decimal precision)
```

### Using Codex 5.2 for Automated Test Script Generation

Codex 5.2 translates test cases into executable test scripts:

```markdown
@ARCALC.cob
@test-cases.md

Generate automated test scripts for the aging calculation test cases.
Target framework: MSTest for Visual Studio 2015
Include:

- Test data setup and teardown
- File I/O mocking for COBOL flat files
- Database state verification
- Assertion helpers for COBOL data types
```

### Using Claude Code for Comprehensive Test Planning

Claude Code provides deep analysis for test strategy development:

```bash
claude "Analyze ARCALC.cob and create a comprehensive test plan covering all paragraphs,
decision branches, and error handlers. Include test data requirements and
risk-based prioritization."
```

### Test Case Categories Matrix

| Category              | Description                                   | Priority | AI Tool         |
| --------------------- | --------------------------------------------- | -------- | --------------- |
| **Boundary Tests**    | Min/max values, field limits, date boundaries | High     | ChatGPT Pro 5.2 |
| **Edge Cases**        | Empty files, zero records, null handling      | High     | Claude Code     |
| **Regression Tests**  | Existing functionality preservation           | Critical | Augment AI      |
| **Integration Tests** | Cross-program data flow, copybook consistency | High     | Codex 5.2       |
| **Performance Tests** | Large file processing, batch timing           | Medium   | ChatGPT Pro 5.2 |
| **Security Tests**    | Authorization checks, data masking            | High     | Claude Code     |
| **Negative Tests**    | Invalid inputs, corrupted data, file locks    | Medium   | ChatGPT Pro 5.2 |

---

## Bug Report Analysis and Reproduction

### Analyzing Bug Reports with AI

Use AI to quickly understand and categorize bug reports:

```markdown
Template: Analyze this bug report and suggest likely causes

Bug Report:
[Paste bug report content]

COBOL Program: [PROGRAM.cob]
Environment: Fujitsu NetCOBOL, Visual Studio 2015, SQL Server

Please provide:

1. Root cause analysis (top 3 most likely causes)
2. Affected code areas (paragraphs, copybooks)
3. Reproduction steps
4. Suggested fix approach
5. Regression test recommendations
6. Similar historical issues to check
```

### Generating Reproduction Steps

```markdown
Template: Generate detailed reproduction steps for this defect

Defect Summary: [Brief description]
Reported Behavior: [What happens]
Expected Behavior: [What should happen]
Environment: [Production/Test/Dev]

Provide:

1. Environment setup requirements
2. Test data preparation
3. Step-by-step execution instructions
4. Verification checkpoints
5. Screenshots/log capture points
6. Cleanup procedures
```

### Root Cause Analysis Prompts

```markdown
@ERROR-LOG.txt
@CUSTMAINT.cob
@CP-CUSTHDR.cpy

Analyze this runtime error and identify:

1. The specific line/paragraph causing the failure
2. Data conditions that triggered the error
3. Whether this is a code defect or data issue
4. Potential fix locations
5. Impact on related programs that call CUSTMAINT
```

> **💡 Tip:** When analyzing COBOL bugs, always include relevant copybooks in your context. Many issues stem from data definition mismatches between programs.

---

## Automated Testing Strategy Development

### Building Test Automation Frameworks with AI

```markdown
Create a test automation framework architecture for COBOL modernization validation

Requirements:

- Compare COBOL output with C# migrated code
- Support batch and online program testing
- Handle file-based I/O (sequential, indexed, relative)
- Database state comparison
- Report generation for stakeholders

Environment:

- Visual Studio 2015
- MSTest framework
- SQL Server 2014
- Fujitsu NetCOBOL runtime
```

### Integration Testing for COBOL-to-Modern-Code Migrations

```csharp
// AI-generated integration test pattern
[TestClass]
public class CobolMigrationIntegrationTests
{
    [TestMethod]
    public async Task CompareInventoryCalculation_CobolVsCSharp()
    {
        // Arrange - Load identical test data
        var testData = TestDataLoader.LoadInventoryScenario("INV-001");

        // Act - Run COBOL program
        var cobolResult = await CobolRunner.ExecuteAsync(
            "INVCALC", testData.InputFiles);

        // Act - Run C# equivalent
        var csharpResult = await _inventoryService
            .CalculateAsync(testData.ToDto());

        // Assert - Results match
        Assert.AreEqual(cobolResult.TotalValue,
            csharpResult.TotalValue, 0.01m);
        Assert.AreEqual(cobolResult.UnitCount,
            csharpResult.UnitCount);
    }
}
```

### Performance Testing Considerations

When testing modernized code, performance baselines are critical:

```markdown
Template: Create performance test plan for migrated COBOL batch program

Original Program: [PROGRAM.cob]
Migrated Version: [Program.cs]
Business Function: [Description]

Baseline metrics from COBOL:

- Average execution time: [X seconds]
- Peak memory usage: [X MB]
- Records processed per second: [X]
- Database connections: [X]

Define tests for:

1. Throughput comparison (records/second)
2. Memory consumption patterns
3. Database connection pooling efficiency
4. File I/O performance
5. CPU utilization under load
6. Scalability limits
```

### Using Augment AI Context Engine for Test Coverage

Augment AI's context engine helps identify untested code paths:

```bash
# Search for untested business logic
auggie search "paragraphs not covered by existing tests in ORDER-PROC.cob"

# Identify missing integration tests
auggie ask "What cross-program interactions are not tested between
ORDER-PROC and INVENTORY-UPDATE?"

# Analyze test coverage gaps
auggie review --coverage ./src/cobol/
```

---

## Code Review and Quality Assurance

### AI-Assisted Code Review Checklists

Generate targeted review checklists for COBOL code:

```markdown
Template: Create code review checklist for COBOL program modifications

Program: [PROGRAM.cob]
Change Description: [What was modified]
Modified Paragraphs: [List]
Affected Copybooks: [List]

Generate checklist covering:

1. Business logic correctness
2. Data type compatibility
3. Error handling completeness
4. Performance implications
5. Security considerations
6. Documentation updates needed
7. Regression risk assessment
```

### Security Vulnerability Detection

```markdown
@CUSTMAINT.cob
@CP-CUSTHDR.cpy

Analyze this COBOL program for security vulnerabilities:

1. SQL Injection risks in embedded SQL
2. Buffer overflow potential (field size mismatches)
3. Sensitive data exposure (SSN, credit card, passwords)
4. Authorization bypass possibilities
5. Audit trail gaps
6. Input validation weaknesses

Provide severity rating (Critical/High/Medium/Low) for each finding.
```

### Code Quality Metrics Analysis

| Metric                 | Target             | AI Tool for Analysis |
| ---------------------- | ------------------ | -------------------- |
| Cyclomatic Complexity  | < 15 per paragraph | Claude Code          |
| Comment Ratio          | > 20%              | ChatGPT Pro 5.2      |
| Dead Code              | 0%                 | Augment AI           |
| Duplicate Logic        | < 5%               | Codex 5.2            |
| Error Handler Coverage | 100%               | Claude Code          |
| Copybook Consistency   | 100%               | Augment AI           |

### Best Practices for Reviewing AI-Generated Code

> **⚠️ Warning:** Never deploy AI-generated code without thorough review and testing.

**Review Checklist for AI-Generated Code:**

- [ ] Business logic matches requirements exactly
- [ ] COBOL-specific patterns are correctly translated
- [ ] Data types maintain precision (especially COMP-3)
- [ ] Error handling covers all failure modes
- [ ] Performance is acceptable for production volumes
- [ ] Security vulnerabilities are addressed
- [ ] Code follows team coding standards
- [ ] Unit tests provide adequate coverage

---

## COBOL Legacy System Testing Approaches

### Testing Data File Handling

COBOL programs often use multiple file organizations:

#### Sequential Files

```markdown
Template: Create test cases for sequential file processing

Program: [PROGRAM.cob]
File: [FILENAME]
Record Layout: [Copybook reference]
Processing: [Read/Write/Update]

Test scenarios:

1. Empty file handling
2. Single record file
3. Maximum expected records ([N] records)
4. Record count validation
5. End-of-file handling
6. File status code verification (00, 10, 35, etc.)
```

#### Indexed Files (VSAM-style)

```markdown
Template: Create test cases for indexed file operations

Program: [PROGRAM.cob]
File: [FILENAME]
Primary Key: [KEY-FIELD]
Alternate Keys: [List if any]

Test scenarios:

1. Key not found (status 23)
2. Duplicate key handling (status 22)
3. Sequential read after random access
4. Key range boundary testing
5. Alternate key access patterns
6. File locking behavior
```

#### Relative Files

```markdown
Template: Create test cases for relative file operations

Test scenarios:

1. Slot 0 access attempt
2. Maximum slot number access
3. Empty slot read (status 23)
4. Slot reuse after delete
5. Relative key calculation verification
```

### Database Interaction Testing (Embedded SQL)

```markdown
@CUSTUPD.cob

Generate test cases for embedded SQL operations:

1. EXEC SQL SELECT
   - Record found
   - Record not found (SQLCODE = 100)
   - Multiple records returned (cursor handling)

2. EXEC SQL INSERT
   - Successful insert
   - Duplicate key violation
   - Constraint violation
   - NULL handling

3. EXEC SQL UPDATE
   - Row updated successfully
   - Row not found (SQLCODE = 100)
   - Optimistic locking conflict

4. EXEC SQL DELETE
   - Successful deletion
   - Referential integrity violation

5. Transaction handling
   - COMMIT success
   - ROLLBACK scenarios
   - Deadlock handling (SQLCODE = -911)
```

### Batch Processing Validation

```markdown
Template: Create a test plan for this COBOL batch program

Program: [BATCHPGM.cob]
Frequency: [Daily/Weekly/Monthly]
Input Sources: [Files, tables, parameters]
Output Targets: [Files, tables, reports]
Dependencies: [Prior jobs, data availability]

Test plan sections:

1. Normal processing path
2. Restart/recovery scenarios
3. Control total reconciliation
4. Checkpoint verification
5. Abend handling
6. JCL parameter variations
7. Date-dependent logic (month-end, year-end)
```

### Report Output Verification

```markdown
Template: Validate report output from COBOL program

Report: [REPORT-NAME]
Program: [PROGRAM.cob]
Format: [Print file, PDF, CSV]

Verification points:

1. Header information accuracy
2. Column alignment and formatting
3. Numeric totals and subtotals
4. Page break logic
5. Control break accuracy
6. Footer calculations
7. Special character handling
8. Date/time stamp correctness
```

### JCL and Job Stream Testing

```markdown
Template: Create JCL job stream test plan

Job: [JOBNAME]
Programs Called: [List]
Files Used: [DD names and datasets]
Condition Code Handling: [Expected return codes]

Test scenarios:

1. Normal completion (CC=0)
2. Warning conditions (CC=4)
3. Error conditions (CC=8, CC=12)
4. Abend recovery (CC=16, U-codes)
5. DD override testing
6. GDG (Generation Data Group) handling
7. Symbolic parameter substitution
8. Multi-step job dependencies
```

---

## AI-Assisted Regression Testing

### Identifying Regression Test Candidates

Use AI to intelligently select which tests to run:

```markdown
Template: Identify regression test candidates for code change

Modified Files:

- [PROGRAM.cob] - [Description of changes]
- [CP-COPYBOOK.cpy] - [Description of changes]

Analyze and identify:

1. All programs that CALL modified program
2. All programs that COPY modified copybook
3. All downstream processes affected
4. Data files that may be impacted
5. Reports that use modified data

Prioritize tests by:

- Business criticality (revenue impact)
- Change proximity (directly vs. indirectly affected)
- Historical defect density
- Customer visibility
```

### Automating Regression Test Maintenance

```markdown
@CUSTMAINT.cob (before change)
@CUSTMAINT.cob (after change)

Compare these two versions and:

1. Identify all modified paragraphs
2. List new or changed business rules
3. Suggest new test cases needed
4. Identify existing tests that need updates
5. Flag tests that are now obsolete
6. Recommend test data updates
```

### Using AI to Prioritize Test Execution

```csharp
// AI-generated test prioritization logic
public class TestPrioritizer
{
    public async Task<IOrderedEnumerable<TestCase>> PrioritizeTests(
        CodeChange change,
        IEnumerable<TestCase> availableTests)
    {
        // AI analyzes change impact and prioritizes tests
        var impactAnalysis = await _aiService.AnalyzeImpact(change);

        return availableTests
            .Select(t => new {
                Test = t,
                Priority = CalculatePriority(t, impactAnalysis)
            })
            .OrderByDescending(x => x.Priority)
            .Select(x => x.Test);
    }

    private int CalculatePriority(TestCase test, ImpactAnalysis impact)
    {
        int priority = 0;

        // Higher priority for tests covering changed code
        if (impact.DirectlyAffectedAreas.Contains(test.TargetArea))
            priority += 100;

        // Higher priority for historically flaky areas
        if (test.HistoricalFailureRate > 0.1)
            priority += 50;

        // Higher priority for customer-facing functionality
        if (test.BusinessCriticality == Criticality.High)
            priority += 75;

        return priority;
    }
}
```

> **💡 Tip:** Run AI-prioritized regression tests first during time-constrained testing cycles. This catches the most likely failures early.

---

## Practical Examples

### Example 1: Testing a COBOL Accounts Receivable Module

**Scenario:** ARAGING.cob calculates customer aging buckets and identifies past-due accounts.

**AI Prompt:**

```markdown
@ARAGING.cob
@CP-ARMASTER.cpy
@CP-CUSTMAST.cpy

Generate comprehensive test cases for the accounts receivable aging module:

Business Context:

- Aging buckets: Current, 1-30, 31-60, 61-90, 90+ days
- Credit hold triggered at 90+ days with balance > $1,000
- Interest calculation: 1.5% monthly on 60+ day balances
- Multi-currency support (USD, CAD, EUR)

Focus areas:

1. Date arithmetic accuracy (leap years, month boundaries)
2. Partial payment allocation rules
3. Credit memo application order
4. Currency conversion timing
5. Credit hold threshold edge cases
```

**Generated Test Cases (excerpt):**

| Test ID | Description                   | Input                                     | Expected Result                 |
| ------- | ----------------------------- | ----------------------------------------- | ------------------------------- |
| AR-001  | Current invoice, full payment | Invoice: $500, 15 days old, Payment: $500 | Balance: $0, Bucket: N/A        |
| AR-002  | 30-day boundary               | Invoice dated exactly 30 days ago         | Falls in 1-30 bucket, not 31-60 |
| AR-003  | Leap year Feb 29              | Invoice dated Feb 29, 2024                | Correct aging to current date   |
| AR-004  | Credit hold trigger           | 91 days old, balance $1,001               | Credit hold flag = 'Y'          |
| AR-005  | Credit hold edge              | 91 days old, balance $999                 | Credit hold flag = 'N'          |
| AR-006  | Multi-currency EUR            | EUR invoice, rate change mid-aging        | Correct USD equivalent          |

### Example 2: Validating a Data Migration Script

**Scenario:** Migrating customer data from COBOL flat files to SQL Server.

**AI Prompt:**

```markdown
Create validation test plan for customer data migration

Source: CUSTOMER.DAT (COBOL indexed file, 50,000 records)
Target: SQL Server CUSTOMER_MASTER table
Migration Tool: SSIS Package

Validation requirements:

1. Record count matching
2. Data type conversion accuracy
   - COMP-3 → DECIMAL
   - PIC X → VARCHAR
   - COBOL dates → SQL DATE
3. Key integrity (no duplicates, no orphans)
4. Business rule preservation
5. Special character handling
6. NULL vs. spaces vs. zeros logic
```

**Generated Validation Queries:**

```sql
-- Record count validation
SELECT
    'Source' AS Dataset, COUNT(*) AS RecordCount
FROM COBOL_STAGING.dbo.CUSTOMER_IMPORT
UNION ALL
SELECT
    'Target' AS Dataset, COUNT(*) AS RecordCount
FROM dbo.CUSTOMER_MASTER;

-- Data type conversion validation (COMP-3 to DECIMAL)
SELECT
    s.CUST_ID,
    s.CREDIT_LIMIT AS Source_CreditLimit,
    t.CREDIT_LIMIT AS Target_CreditLimit,
    ABS(s.CREDIT_LIMIT - t.CREDIT_LIMIT) AS Difference
FROM COBOL_STAGING.dbo.CUSTOMER_IMPORT s
JOIN dbo.CUSTOMER_MASTER t ON s.CUST_ID = t.CUSTOMER_ID
WHERE ABS(s.CREDIT_LIMIT - t.CREDIT_LIMIT) > 0.01;

-- NULL handling validation
SELECT
    COUNT(*) AS SpacesToNullCount
FROM COBOL_STAGING.dbo.CUSTOMER_IMPORT s
JOIN dbo.CUSTOMER_MASTER t ON s.CUST_ID = t.CUSTOMER_ID
WHERE LTRIM(RTRIM(s.ALTERNATE_CONTACT)) = ''
  AND t.ALTERNATE_CONTACT IS NULL;
```

### Example 3: Performance Testing a Modernized API

**Scenario:** CUSTINQ.cob has been migrated to a REST API. Need to validate performance.

**AI Prompt:**

```markdown
Create performance test plan comparing COBOL CUSTINQ to C# Customer API

COBOL Baseline:

- Average response: 150ms
- 95th percentile: 400ms
- Throughput: 200 TPS
- Concurrent users: 50

Test scenarios:

1. Single user response time comparison
2. Load test: 50 concurrent users, 10-minute duration
3. Stress test: Find breaking point
4. Endurance test: 8-hour sustained load
5. Database connection pool behavior
6. Memory leak detection
```

**Generated JMeter Test Plan Outline:**

```xml
<!-- Performance Test Configuration -->
<TestPlan>
    <ThreadGroup name="Baseline Comparison">
        <threads>1</threads>
        <duration>60</duration>
        <HTTPSampler path="/api/customers/{id}" method="GET"/>
        <ResponseAssertion testfield="RESPONSE_CODE" pattern="200"/>
        <DurationAssertion duration="500"/>
    </ThreadGroup>

    <ThreadGroup name="Load Test">
        <threads>50</threads>
        <rampUp>60</rampUp>
        <duration>600</duration>
        <HTTPSampler path="/api/customers/{id}" method="GET"/>
        <ConstantThroughputTimer throughput="200"/>
    </ThreadGroup>

    <ThreadGroup name="Stress Test">
        <threads>200</threads>
        <rampUp>300</rampUp>
        <duration>900</duration>
        <!-- Monitor for errors, response time degradation -->
    </ThreadGroup>
</TestPlan>
```

---

## QA Tool Integration Matrix

### Which AI Tool for Each QA Task

| QA Task                       | Best Tool       | Why                                                                             | Integration              |
| ----------------------------- | --------------- | ------------------------------------------------------------------------------- | ------------------------ |
| **Test Case Design**          | ChatGPT Pro 5.2 | Excels at understanding business context and generating comprehensive scenarios | Export to Excel/TestRail |
| **Test Script Generation**    | Codex 5.2       | Purpose-built for code generation with high accuracy                            | Direct IDE integration   |
| **Test Coverage Analysis**    | Augment AI      | Context engine understands full codebase relationships                          | VS Code, JetBrains       |
| **Bug Root Cause Analysis**   | Claude Code     | Deep reasoning for complex multi-file issues                                    | CLI, plan mode           |
| **Regression Test Selection** | Augment AI      | Tracks dependencies across programs                                             | GitHub integration       |
| **Security Testing**          | Claude Code     | Thorough vulnerability analysis                                                 | CLI review command       |
| **Performance Testing**       | ChatGPT Pro 5.2 | Test plan generation and analysis                                               | Export to JMeter         |
| **Documentation Generation**  | All tools       | Each provides unique perspective                                                | Markdown export          |

### Integration with Test Management Tools

```markdown
## TestRail Integration Workflow

1. Generate test cases with ChatGPT Pro 5.2
2. Export as CSV with required fields:
   - Title, Section, Priority, Type
   - Preconditions, Steps, Expected Results
3. Import to TestRail via bulk import
4. Link to requirements and defects

## Azure DevOps Integration

1. Use Codex 5.2 to generate test methods
2. Tag with [TestCategory] for organization
3. Link test cases to work items
4. Execute via Azure Pipelines
5. Track results in Test Plans

## Jira/Zephyr Integration

1. Create test cases from AI output
2. Link to Jira stories and epics
3. Execute test cycles
4. Report defects with AI-generated reproduction steps
```

### Recommended Workflow by Phase

```
┌─────────────────────────────────────────────────────────────────┐
│                    QA WORKFLOW WITH AI TOOLS                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  PLANNING PHASE                                                  │
│  ├── ChatGPT Pro 5.2: Test strategy and case design             │
│  └── Augment AI: Coverage gap analysis                          │
│                                                                  │
│  DEVELOPMENT PHASE                                               │
│  ├── Codex 5.2: Automated test script generation                │
│  └── Claude Code: Code review and security analysis             │
│                                                                  │
│  EXECUTION PHASE                                                 │
│  ├── Augment AI: Regression test prioritization                 │
│  └── All tools: Defect analysis and triage                      │
│                                                                  │
│  REPORTING PHASE                                                 │
│  ├── ChatGPT Pro 5.2: Test summary generation                   │
│  └── Claude Code: Technical root cause documentation            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Best Practices Summary

### DO

✅ Use multiple AI tools for comprehensive coverage
✅ Always validate AI-generated test cases against requirements
✅ Include COBOL-specific edge cases (spaces, zeros, packed decimal)
✅ Maintain traceability from requirements to test cases
✅ Version control test cases alongside code
✅ Run regression tests comparing COBOL and migrated code outputs
✅ Document AI prompts that produce good results
✅ Review AI suggestions with domain experts

### DON'T

❌ Trust AI-generated test data without validation
❌ Skip negative and edge case testing
❌ Ignore COBOL-specific behaviors (implicit conversions, COMP-3)
❌ Deploy without parallel testing between old and new systems
❌ Assume AI understands your business rules perfectly
❌ Replace human judgment with AI for critical decisions

---

## Next Steps

- Explore [Augment AI](./augment-ai.md) for context-aware testing
- Learn [Claude Code](./claude-code.md) for deep code analysis
- Review [Codex 5.2](./codex.md) for test automation
- See [ChatGPT Pro 5.2](./chatgpt.md) for test case design
- Follow the [Spec → Plan → Code → Review workflow](./workflow.md)
- Reference the [COBOL Migration Playbook](./cobol-migration-playbook.md) for migration testing
