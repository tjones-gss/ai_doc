# Database Access Patterns for Manufacturing ERP

## Overview

Standards for database access across COBOL, VB.NET, and C# in our manufacturing ERP system.

## Database Platform

- **DBMS**: Microsoft SQL Server 2019+
- **Connection**: Integrated Security (Windows Authentication) or SQL Authentication
- **Default Schema**: `dbo`
- **Naming Convention**: UPPER_CASE_WITH_UNDERSCORES for tables and columns (legacy standard)

## Table Naming Conventions

### Standard Prefixes

- `MFG_` - Manufacturing tables (orders, BOMs, routings)
- `INV_` - Inventory tables
- `CUST_` - Customer tables
- `VEND_` - Vendor/Supplier tables
- `GL_` - General Ledger integration
- `SYS_` - System configuration tables
- `AUDIT_` - Audit trail tables

### Example Table Names

```sql
MFG_ORDER            -- Manufacturing orders
MFG_ORDER_DETAIL     -- MO line items
INV_ITEM_MASTER      -- Item master file
INV_TRANSACTION      -- Inventory movements
INV_LOT_MASTER       -- Lot tracking
CUST_MASTER          -- Customer master
CUST_ORDER_HEADER    -- Sales orders
VEND_MASTER          -- Vendor master
GL_INTERFACE         -- GL journal entries
SYS_PARAMETER        -- System config
AUDIT_CHANGE_LOG     -- Audit trail
```

## Column Naming Standards

### Key Fields

- Primary Key: `{TABLE}_ID` (e.g., `MFG_ORDER_ID`)
- Foreign Keys: `{REFERENCED_TABLE}_ID` (e.g., `CUST_MASTER_ID`)
- Natural Keys: Descriptive name (e.g., `ORDER_NUMBER`, `ITEM_NUMBER`)

### Standard Audit Columns

Every transactional table must include:

```sql
CREATED_DATE     DATETIME2      NOT NULL DEFAULT GETDATE()
CREATED_BY       VARCHAR(30)    NOT NULL
LAST_UPDATED_DATE DATETIME2     NOT NULL DEFAULT GETDATE()
LAST_UPDATED_BY  VARCHAR(30)    NOT NULL
ROW_VERSION      ROWVERSION     NOT NULL  -- Optimistic concurrency
```

### Common Field Types

```sql
-- Identifiers
{ENTITY}_ID         INT IDENTITY(1,1) PRIMARY KEY
{ENTITY}_NUMBER     VARCHAR(20)    -- Natural key (user-visible)

-- Amounts and Quantities
QUANTITY            DECIMAL(15,4)  -- Inventory quantities
UNIT_PRICE          DECIMAL(15,4)  -- Prices
EXTENDED_AMOUNT     DECIMAL(15,2)  -- Calculated amounts

-- Dates
ORDER_DATE          DATE           -- Date only
SHIP_DATETIME       DATETIME2      -- Date and time
EFFECTIVE_DATE      DATE           -- Date range start
OBSOLETE_DATE       DATE NULL      -- Date range end

-- Status and Codes
STATUS_CODE         CHAR(1)        -- Single character codes
TYPE_CODE           VARCHAR(10)    -- Multi-character codes

-- Descriptions
SHORT_DESC          VARCHAR(50)    -- Brief description
LONG_DESC           VARCHAR(500)   -- Detailed description
NOTES               VARCHAR(MAX)   -- Unlimited text

-- Flags/Booleans (COBOL compatibility)
IS_ACTIVE_FLAG      CHAR(1)        -- 'Y'/'N' instead of BIT
LOT_TRACKED_FLAG    CHAR(1)        -- 'Y'/'N'
```

## COBOL Embedded SQL Patterns

### Standard SELECT Pattern

```cobol
EXEC SQL
    SELECT CUSTOMER_NAME,
           CREDIT_LIMIT,
           PAYMENT_TERMS
      INTO :WS-CUST-NAME,
           :WS-CREDIT-LIMIT,
           :WS-PAYMENT-TERMS
      FROM CUST_MASTER
     WHERE CUST_MASTER_ID = :WS-CUST-ID
END-EXEC.

EVALUATE SQLCODE
    WHEN 0
        PERFORM 2000-PROCESS-CUSTOMER
    WHEN 100
        DISPLAY 'Customer not found: ' WS-CUST-ID
        PERFORM 9100-NOT-FOUND-ERROR
    WHEN OTHER
        DISPLAY 'SQL Error: ' SQLCODE
        PERFORM 9200-SQL-ERROR
END-EVALUATE.
```

### Standard INSERT Pattern

```cobol
EXEC SQL
    INSERT INTO INV_TRANSACTION (
        ITEM_NUMBER,
        TRANSACTION_TYPE,
        QUANTITY,
        TRANSACTION_DATE,
        CREATED_BY,
        CREATED_DATE
    ) VALUES (
        :WS-ITEM-NUMBER,
        :WS-TRANS-TYPE,
        :WS-QUANTITY,
        CURRENT_TIMESTAMP,
        :WS-USER-ID,
        CURRENT_TIMESTAMP
    )
END-EXEC.

IF SQLCODE NOT = 0
    DISPLAY 'Insert failed: ' SQLCODE
    PERFORM 9200-SQL-ERROR
END-IF.
```

### Standard UPDATE with Optimistic Locking

```cobol
EXEC SQL
    UPDATE CUST_MASTER
       SET CREDIT_LIMIT = :WS-NEW-CREDIT-LIMIT,
           LAST_UPDATED_BY = :WS-USER-ID,
           LAST_UPDATED_DATE = CURRENT_TIMESTAMP
     WHERE CUST_MASTER_ID = :WS-CUST-ID
       AND CONVERT(BIGINT, ROW_VERSION) = :WS-ROW-VERSION
END-EXEC.

EVALUATE SQLCODE
    WHEN 0
        IF SQLERRD(3) = 0
            DISPLAY 'Record changed by another user'
            PERFORM 9300-CONCURRENCY-ERROR
        END-IF
    WHEN OTHER
        PERFORM 9200-SQL-ERROR
END-EVALUATE.
```

### Cursor Processing Pattern

```cobol
EXEC SQL
    DECLARE ORDER_CURSOR CURSOR FOR
    SELECT ORDER_NUMBER,
           CUSTOMER_ID,
           ORDER_DATE,
           TOTAL_AMOUNT
      FROM CUST_ORDER_HEADER
     WHERE ORDER_DATE = :WS-PROCESS-DATE
       AND STATUS_CODE = 'N'
     ORDER BY ORDER_NUMBER
END-EXEC.

EXEC SQL OPEN ORDER_CURSOR END-EXEC.

PERFORM UNTIL WS-END-OF-CURSOR = 'Y'
    EXEC SQL
        FETCH ORDER_CURSOR
         INTO :WS-ORDER-NUMBER,
              :WS-CUSTOMER-ID,
              :WS-ORDER-DATE,
              :WS-TOTAL-AMOUNT
    END-EXEC

    EVALUATE SQLCODE
        WHEN 0
            PERFORM 3000-PROCESS-ORDER
        WHEN 100
            MOVE 'Y' TO WS-END-OF-CURSOR
        WHEN OTHER
            PERFORM 9200-SQL-ERROR
            MOVE 'Y' TO WS-END-OF-CURSOR
    END-EVALUATE
END-PERFORM.

EXEC SQL CLOSE ORDER_CURSOR END-EXEC.
```

## Entity Framework Core Patterns (C#)

### DbContext Configuration

```csharp
public class ErpDbContext : DbContext
{
    public DbSet<Customer> Customers { get; set; }
    public DbSet<ManufacturingOrder> ManufacturingOrders { get; set; }
    public DbSet<InventoryTransaction> InventoryTransactions { get; set; }

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        // Map to legacy table names
        modelBuilder.Entity<Customer>()
            .ToTable("CUST_MASTER")
            .HasKey(c => c.Id);

        modelBuilder.Entity<Customer>()
            .Property(c => c.Id)
            .HasColumnName("CUST_MASTER_ID");

        modelBuilder.Entity<Customer>()
            .Property(c => c.Name)
            .HasColumnName("CUSTOMER_NAME")
            .HasMaxLength(50)
            .IsRequired();

        // Optimistic concurrency with RowVersion
        modelBuilder.Entity<Customer>()
            .Property(c => c.RowVersion)
            .HasColumnName("ROW_VERSION")
            .IsRowVersion();

        // Indexes for performance
        modelBuilder.Entity<Customer>()
            .HasIndex(c => c.CustomerNumber)
            .IsUnique();
    }
}
```

### Repository Query Patterns

```csharp
public class InventoryRepository : IInventoryRepository
{
    private readonly ErpDbContext _context;

    public async Task<InventoryItem?> GetByItemNumberAsync(string itemNumber)
    {
        return await _context.InventoryItems
            .AsNoTracking() // Read-only query - better performance
            .FirstOrDefaultAsync(i => i.ItemNumber == itemNumber);
    }

    public async Task<List<InventoryTransaction>> GetTransactionsByDateRangeAsync(
        DateTime startDate, DateTime endDate)
    {
        return await _context.InventoryTransactions
            .AsNoTracking()
            .Where(t => t.TransactionDate >= startDate && t.TransactionDate <= endDate)
            .OrderBy(t => t.TransactionDate)
            .ThenBy(t => t.TransactionId)
            .ToListAsync();
    }

    public async Task<int> UpdateWithConcurrencyCheckAsync(InventoryItem item)
    {
        try
        {
            _context.Entry(item).State = EntityState.Modified;
            return await _context.SaveChangesAsync();
        }
        catch (DbUpdateConcurrencyException ex)
        {
            // Handle optimistic concurrency failure
            throw new ConcurrencyException($"Item {item.ItemNumber} was modified by another user", ex);
        }
    }
}
```

## Stored Procedure Standards

### Naming Convention

- Prefix: `usp_` (user stored procedure)
- Action: CRUD verb (Get, Insert, Update, Delete)
- Entity: Table or business object
- Example: `usp_GetCustomerById`, `usp_InsertManufacturingOrder`

### Standard Stored Procedure Template

```sql
CREATE OR ALTER PROCEDURE dbo.usp_UpdateCustomerCreditLimit
    @CustomerId INT,
    @NewCreditLimit DECIMAL(15,2),
    @UserId VARCHAR(30),
    @RowVersion BIGINT,
    @ResultCode INT OUTPUT,
    @ResultMessage VARCHAR(500) OUTPUT
AS
BEGIN
    SET NOCOUNT ON;
    SET XACT_ABORT ON;

    DECLARE @CurrentRowVersion BIGINT;

    BEGIN TRY
        BEGIN TRANSACTION;

        -- Optimistic concurrency check
        SELECT @CurrentRowVersion = CONVERT(BIGINT, ROW_VERSION)
        FROM CUST_MASTER
        WHERE CUST_MASTER_ID = @CustomerId;

        IF @CurrentRowVersion IS NULL
        BEGIN
            SET @ResultCode = -1;
            SET @ResultMessage = 'Customer not found';
            ROLLBACK TRANSACTION;
            RETURN;
        END

        IF @CurrentRowVersion <> @RowVersion
        BEGIN
            SET @ResultCode = -2;
            SET @ResultMessage = 'Record modified by another user';
            ROLLBACK TRANSACTION;
            RETURN;
        END

        -- Business validation
        IF @NewCreditLimit < 0
        BEGIN
            SET @ResultCode = -3;
            SET @ResultMessage = 'Credit limit cannot be negative';
            ROLLBACK TRANSACTION;
            RETURN;
        END

        -- Perform update
        UPDATE CUST_MASTER
        SET CREDIT_LIMIT = @NewCreditLimit,
            LAST_UPDATED_BY = @UserId,
            LAST_UPDATED_DATE = GETDATE()
        WHERE CUST_MASTER_ID = @CustomerId;

        -- Audit trail
        INSERT INTO AUDIT_CHANGE_LOG (
            TABLE_NAME, RECORD_ID, FIELD_NAME,
            OLD_VALUE, NEW_VALUE, CHANGED_BY, CHANGED_DATE
        )
        SELECT 'CUST_MASTER', @CustomerId, 'CREDIT_LIMIT',
               CONVERT(VARCHAR(50), CREDIT_LIMIT),
               CONVERT(VARCHAR(50), @NewCreditLimit),
               @UserId, GETDATE()
        FROM CUST_MASTER
        WHERE CUST_MASTER_ID = @CustomerId;

        COMMIT TRANSACTION;

        SET @ResultCode = 0;
        SET @ResultMessage = 'Success';
    END TRY
    BEGIN CATCH
        IF @@TRANCOUNT > 0
            ROLLBACK TRANSACTION;

        SET @ResultCode = ERROR_NUMBER();
        SET @ResultMessage = ERROR_MESSAGE();
    END CATCH
END;
```

## Transaction Management

### COBOL Transaction Pattern

```cobol
EXEC SQL BEGIN TRANSACTION END-EXEC.

PERFORM 3100-INSERT-ORDER-HEADER.
IF WS-SQL-ERROR = 'N'
    PERFORM 3200-INSERT-ORDER-DETAILS
END-IF.

IF WS-SQL-ERROR = 'N'
    PERFORM 3300-UPDATE-INVENTORY
END-IF.

IF WS-SQL-ERROR = 'Y'
    EXEC SQL ROLLBACK TRANSACTION END-EXEC
    DISPLAY 'Transaction rolled back'
ELSE
    EXEC SQL COMMIT TRANSACTION END-EXEC
    DISPLAY 'Transaction committed'
END-IF.
```

### C# Transaction Pattern

```csharp
using var transaction = await _context.Database.BeginTransactionAsync();
try
{
    // Create order header
    var order = new ManufacturingOrder { ... };
    _context.ManufacturingOrders.Add(order);
    await _context.SaveChangesAsync();

    // Create order details
    foreach (var item in request.Items)
    {
        var detail = new ManufacturingOrderDetail { OrderId = order.Id, ... };
        _context.ManufacturingOrderDetails.Add(detail);
    }
    await _context.SaveChangesAsync();

    // Update inventory
    await _inventoryService.AllocateInventoryAsync(order.Id);

    await transaction.CommitAsync();
    return Result<int>.Success(order.Id);
}
catch (Exception ex)
{
    await transaction.RollbackAsync();
    _logger.LogError(ex, "Failed to create manufacturing order");
    return Result<int>.Failure($"Transaction failed: {ex.Message}");
}
```

## Performance Optimization

### Indexing Strategy

```sql
-- Primary keys (clustered index)
ALTER TABLE MFG_ORDER
ADD CONSTRAINT PK_MFG_ORDER PRIMARY KEY CLUSTERED (MFG_ORDER_ID);

-- Foreign keys (non-clustered indexes)
CREATE NONCLUSTERED INDEX IX_MFG_ORDER_ITEM
ON MFG_ORDER (ITEM_NUMBER)
INCLUDE (ORDER_QTY, STATUS_CODE);

-- Frequent query patterns
CREATE NONCLUSTERED INDEX IX_MFG_ORDER_STATUS_DATE
ON MFG_ORDER (STATUS_CODE, DUE_DATE)
WHERE STATUS_CODE IN ('R', 'I'); -- Filtered index

-- Composite indexes for joins
CREATE NONCLUSTERED INDEX IX_INV_TRANS_ITEM_DATE
ON INV_TRANSACTION (ITEM_NUMBER, TRANSACTION_DATE DESC);
```

### Query Optimization Tips

- Always use parameterized queries (prevent SQL injection and enable plan reuse)
- Use `WITH (NOLOCK)` only for read-only reporting queries
- Avoid `SELECT *` - specify columns explicitly
- Use covering indexes for frequently accessed queries
- Avoid functions in WHERE clause (prevents index usage)
- Use EXISTS instead of IN for large result sets

## AI Code Generation Instructions

When generating database access code:

1. Use parameterized queries (never string concatenation)
2. Always check SQLCODE in COBOL (0=success, 100=not found, other=error)
3. Include optimistic concurrency checks for updates
4. Wrap related operations in transactions
5. Update audit columns (CREATED_BY, LAST_UPDATED_BY, etc.)
6. Use appropriate data types matching column definitions
7. Include error handling for all database operations
8. Add appropriate indexes for new queries
9. Use AsNoTracking() for read-only EF Core queries
10. Follow naming conventions strictly

## Code Review Checklist

- [ ] Parameterized queries used (no SQL injection risk)
- [ ] SQLCODE checked after every SQL operation (COBOL)
- [ ] Transaction boundaries correct
- [ ] Optimistic concurrency implemented for updates
- [ ] Audit columns populated
- [ ] Error handling comprehensive
- [ ] Indexes appropriate for query patterns
- [ ] Column names match database schema exactly
- [ ] Data types match database definitions
- [ ] Foreign key relationships respected
- [ ] No N+1 query problems (eager loading used)
- [ ] Query performance acceptable (<100ms for simple queries)

## References

- SQL Server Best Practices Documentation
- Entity Framework Core Documentation
- COBOL Embedded SQL Reference
- Database Schema Diagram (see internal wiki)
- erp-domain-knowledge.md (business rules)
