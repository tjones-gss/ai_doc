# COBOL Coding Standards for Manufacturing ERP

## Overview

This rule file defines coding standards for COBOL programs in our manufacturing ERP system. All AI-generated COBOL code must adhere to these guidelines.

## Language Specifics

- **Compiler**: Fujitsu NetCOBOL for .NET (supports both mainframe and .NET interoperability)
- **COBOL Standard**: COBOL-85 with Object-Oriented extensions where applicable
- **Character Encoding**: EBCDIC for mainframe, ASCII for .NET interop

## File Organization

### Program Structure

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. <PROGRAM-NAME>.
*****************************************************************
* Program Name: <NAME>
* Description:  <PURPOSE>
* Author:       <AUTHOR>
* Date Created: <DATE>
* Last Modified: <DATE> - <DESCRIPTION OF CHANGE>
*****************************************************************

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
SOURCE-COMPUTER. <SYSTEM>.
OBJECT-COMPUTER. <SYSTEM>.

INPUT-OUTPUT SECTION.
FILE-CONTROL.
    [File definitions here]

DATA DIVISION.
FILE SECTION.
    [File record descriptions]

WORKING-STORAGE SECTION.
    [Variables and constants]

LINKAGE SECTION.
    [Parameters passed to/from program]

PROCEDURE DIVISION.
    [Main program logic]
```

### Copybook Organization

- Prefix all copybooks with `CP-` for clarity
- Organize copybooks by function:
  - `CP-<ENTITY>-HDR` - Header/main record layout
  - `CP-<ENTITY>-DTL` - Detail/line item layout
  - `CP-<ENTITY>-WRK` - Working storage variables
  - `CP-CONST-<MODULE>` - Constants
  - `CP-ERR-<MODULE>` - Error codes and messages

## Naming Conventions

### Variables

- **UPPER-CASE-WITH-HYPHENS** for all COBOL identifiers
- Prefix conventions:
  - `WS-` - Working Storage variables
  - `LS-` - Linkage Section parameters
  - `SQL-` - SQL-related variables
  - `ERR-` - Error handling variables
  - `CNT-` - Counter variables
  - `IDX-` - Index variables
  - `SW-` - Switch/flag variables (88-levels preferred)
  - `SAVE-` - Save areas for original values

### File Names

- Maximum 8 characters for program ID (mainframe compatibility)
- Use descriptive prefixes:
  - `MFGORD` - Manufacturing Orders
  - `INVMGR` - Inventory Manager
  - `CUSTMNT` - Customer Maintenance
  - `PRTSCH` - Production Scheduling
  - `QALCTL` - Quality Control

### Paragraph Names

- Use descriptive, hierarchical naming:
  ```cobol
  1000-MAIN-PROCESS.
  1100-INITIALIZE-PROGRAM.
  1200-PROCESS-RECORDS.
  1210-READ-INPUT-FILE.
  1220-VALIDATE-RECORD.
  1230-UPDATE-DATABASE.
  1240-WRITE-OUTPUT.
  1300-FINALIZE-PROGRAM.
  9000-ERROR-HANDLER.
  9100-SQL-ERROR-HANDLER.
  9999-ABEND-ROUTINE.
  ```
- Number paragraphs in increments of 10 or 100 for future insertions
- Error handling paragraphs start with 9000

## Data Declarations

### PIC Clauses

- Be explicit with picture clauses:
  - Numeric: `PIC 9(9)V99` or `PIC S9(7)V99 COMP-3`
  - Alphanumeric: `PIC X(30)`
  - Packed decimal: `PIC S9(7)V99 COMP-3` (for performance)
- Use COMP-3 for numeric fields in databases (saves space)
- Use BINARY/COMP for loop counters and array indices

### 88-Level Condition Names

- Always use 88-levels for switches instead of comparing literals:
  ```cobol
  01  WS-RECORD-STATUS           PIC X.
      88  RECORD-FOUND           VALUE 'Y'.
      88  RECORD-NOT-FOUND       VALUE 'N'.
      88  RECORD-ERROR           VALUE 'E'.
  ```

### REDEFINES

- Document all REDEFINES clearly
- Ensure alignment and size compatibility
- Use for date manipulation and format conversions

## SQL Integration

### Embedded SQL Standards

```cobol
EXEC SQL
    SELECT CUSTOMER_NAME,
           CREDIT_LIMIT,
           LAST_ORDER_DATE
      INTO :WS-CUST-NAME,
           :WS-CREDIT-LIMIT,
           :WS-LAST-ORDER-DATE
      FROM CUSTOMER
     WHERE CUSTOMER_ID = :WS-CUST-ID
END-EXEC.

EVALUATE SQLCODE
    WHEN 0
        PERFORM 2100-PROCESS-CUSTOMER
    WHEN 100
        PERFORM 9100-CUSTOMER-NOT-FOUND
    WHEN OTHER
        PERFORM 9200-SQL-ERROR
END-EVALUATE.
```

### SQL Best Practices

- Always check SQLCODE after every SQL operation
- Use host variables (prefixed with colon) for all parameters
- Include SQLCA (SQL Communication Area) in WORKING-STORAGE
- Use prepared statements for repeated queries
- Always use explicit column names (never SELECT \*)
- Add appropriate indexes for performance

## Error Handling

### Standard Error Pattern

```cobol
9000-ERROR-HANDLER.
    DISPLAY 'ERROR IN ' WS-PROGRAM-NAME.
    DISPLAY 'PARAGRAPH: ' WS-CURRENT-PARAGRAPH.
    DISPLAY 'ERROR CODE: ' WS-ERROR-CODE.
    DISPLAY 'ERROR MESSAGE: ' WS-ERROR-MESSAGE.

    PERFORM 9100-WRITE-ERROR-LOG.

    IF WS-ERROR-SEVERITY = 'FATAL'
        PERFORM 9999-ABEND-ROUTINE
    ELSE
        SET WS-ERROR-FLAG TO TRUE
        CONTINUE
    END-IF.
```

### Error Codes

- Use standardized error code format: `ERR-XXXX`
  - `ERR-1XXX` - File I/O errors
  - `ERR-2XXX` - Data validation errors
  - `ERR-3XXX` - Database errors
  - `ERR-4XXX` - Business logic errors
  - `ERR-5XXX` - Interface/integration errors
  - `ERR-9XXX` - System/fatal errors

### Error Logging

- Always log errors to both:
  1. System error file
  2. Application log table (for database-connected programs)
- Include: timestamp, program name, paragraph, error code, error message, user ID

## File Handling

### File Operations Standard

```cobol
OPEN INPUT CUSTOMER-FILE.
IF WS-FILE-STATUS NOT = '00'
    MOVE 'ERR-1001' TO WS-ERROR-CODE
    MOVE 'CUSTOMER FILE OPEN FAILED' TO WS-ERROR-MESSAGE
    PERFORM 9000-ERROR-HANDLER
END-IF.

READ CUSTOMER-FILE
    AT END
        SET WS-END-OF-FILE TO TRUE
    NOT AT END
        PERFORM 2000-PROCESS-CUSTOMER-RECORD
END-READ.

CLOSE CUSTOMER-FILE.
```

### File Status Checks

- Always declare file status variables for each file
- Check file status after OPEN, READ, WRITE, REWRITE, DELETE, CLOSE
- Use meaningful file status code handling

## Performance Optimization

### Guidelines

1. **Use COMP-3 for arithmetic** - Packed decimal is faster than display numeric
2. **Minimize I/O operations** - Batch reads/writes when possible
3. **Use indexes for table searches** - SEARCH ALL vs. SEARCH
4. **Avoid nested PERFORMs** - Flatten when possible for performance
5. **Close files explicitly** - Don't rely on program termination
6. **Use CALL instead of PERFORM** for large reusable code blocks

### Table Handling

```cobol
01  WS-CUSTOMER-TABLE.
    05  WS-CUSTOMER-ENTRY OCCURS 1000 TIMES
        INDEXED BY IDX-CUSTOMER.
        10  WS-CUST-ID             PIC 9(8).
        10  WS-CUST-NAME           PIC X(30).
        10  WS-CUST-CREDIT-LIMIT   PIC 9(9)V99 COMP-3.

* Use SEARCH ALL for binary search (requires ASCENDING KEY)
SEARCH ALL WS-CUSTOMER-ENTRY
    AT END
        PERFORM 9100-CUSTOMER-NOT-FOUND
    WHEN WS-CUST-ID (IDX-CUSTOMER) = WS-SEARCH-CUST-ID
        PERFORM 2000-PROCESS-CUSTOMER
END-SEARCH.
```

## .NET Interoperability

### Calling C# or VB.NET from COBOL

```cobol
CALL "AssemblyName.NameSpace.ClassName.MethodName"
    USING BY REFERENCE WS-PARAMETER-1
          BY CONTENT WS-PARAMETER-2
    RETURNING WS-RETURN-VALUE.
```

### Parameter Passing

- **BY REFERENCE** - Pass memory address (modifiable by called program)
- **BY CONTENT** - Pass value copy (read-only to called program)
- Always document parameter direction (IN, OUT, INOUT)

### Data Type Compatibility

- COBOL `PIC 9(4) COMP` ↔ C# `short` (Int16)
- COBOL `PIC 9(9) COMP` ↔ C# `int` (Int32)
- COBOL `PIC X(n)` ↔ C# `string` (requires marshaling)
- COBOL `PIC 9(n)V99 COMP-3` ↔ C# `decimal`

## Comments and Documentation

### Program Header Comments

- Required fields:
  - Program name and ID
  - Purpose/description
  - Author
  - Date created
  - Modification history (date, author, description)
  - Dependencies (copybooks, called programs)
  - Input/output files or parameters

### Inline Comments

```cobol
*****************************************************************
* Initialize counters and flags for processing
*****************************************************************
MOVE ZERO TO WS-RECORD-COUNT.
SET WS-ERROR-FLAG TO FALSE.

* Calculate extended price with quantity discount
COMPUTE WS-EXTENDED-PRICE =
    WS-UNIT-PRICE * WS-QUANTITY * (1 - WS-DISCOUNT-RATE).
```

### Comment Every Paragraph

```cobol
1200-PROCESS-CUSTOMER-ORDERS.
*****************************************************************
* Process all orders for the current customer
* Validates order data, updates inventory, calculates totals
* Input:  WS-CURRENT-CUSTOMER-ID
* Output: WS-ORDER-TOTAL, WS-ORDERS-PROCESSED
*****************************************************************
```

## Manufacturing ERP Specific Patterns

### Inventory Transaction Processing

```cobol
* Always lock record before update
EXEC SQL
    SELECT QUANTITY_ON_HAND,
           QUANTITY_ALLOCATED
      INTO :WS-QTY-ON-HAND,
           :WS-QTY-ALLOCATED
      FROM INVENTORY
     WHERE ITEM_ID = :WS-ITEM-ID
       FOR UPDATE
END-EXEC.

* Validate sufficient quantity
IF WS-QTY-ON-HAND < WS-REQUESTED-QTY
    PERFORM 9400-INSUFFICIENT-INVENTORY
END-IF.

* Update with commit
EXEC SQL
    UPDATE INVENTORY
       SET QUANTITY_ON_HAND = :WS-NEW-QTY-ON-HAND,
           QUANTITY_ALLOCATED = :WS-NEW-QTY-ALLOCATED,
           LAST_UPDATE_DATE = CURRENT_TIMESTAMP,
           LAST_UPDATE_USER = :WS-USER-ID
     WHERE ITEM_ID = :WS-ITEM-ID
END-EXEC.

EXEC SQL COMMIT END-EXEC.
```

### Audit Trail Requirements

- All data modifications must create audit records
- Include: old value, new value, user ID, timestamp, program name
- Use database triggers or explicit audit table inserts

### Business Date Handling

```cobol
* Use business date, not system date for transaction processing
MOVE WS-BUSINESS-DATE TO WS-TRANSACTION-DATE.

* Handle fiscal period calculations
PERFORM 5000-CALCULATE-FISCAL-PERIOD.
```

## Testing Guidelines

### Unit Test Comments

```cobol
*****************************************************************
* TEST CASE: Validate credit limit check
* GIVEN: Customer with $50,000 credit limit
* WHEN: Order totaling $60,000 is processed
* THEN: Order should be flagged for review
*****************************************************************
```

### Test Data Requirements

- Include test data in copybook `CP-TEST-DATA`
- Document edge cases in comments
- Test both success and failure paths

## AI Code Generation Instructions

When generating COBOL code:

1. Always include full error handling
2. Add comprehensive comments
3. Use the standard paragraph numbering scheme
4. Include file status checks for all file operations
5. Add SQL error handling for all database operations
6. Follow the naming conventions strictly
7. Generate corresponding copybooks when needed
8. Include audit trail logic for data modifications
9. Add performance optimization hints (COMP-3, indexes, etc.)
10. Consider .NET interoperability requirements

## Code Review Checklist

Before accepting AI-generated COBOL code:

- [ ] Follows naming conventions
- [ ] Includes proper error handling
- [ ] Has adequate comments and documentation
- [ ] Uses 88-levels for condition names
- [ ] Checks file status after all I/O
- [ ] Checks SQLCODE after all SQL operations
- [ ] Includes audit trail for data changes
- [ ] Uses appropriate data types (COMP-3 for decimal)
- [ ] Handles edge cases
- [ ] Compatible with .NET interop if required
- [ ] Performance optimized (indexes, efficient searches)
- [ ] Follows paragraph numbering scheme

## References

- Fujitsu NetCOBOL for .NET Documentation
- Enterprise COBOL Programming Guide
- Manufacturing ERP Business Rules Documentation
- DATABASE.md (database standards)
- CP-STANDARDS.cpy (standard copybook patterns)
