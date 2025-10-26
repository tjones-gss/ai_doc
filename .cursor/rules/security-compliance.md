# Security and Compliance Standards for Manufacturing ERP

## Overview

Security and compliance requirements for AI-generated code in our manufacturing ERP system. All code must adhere to these standards to protect data, ensure audit compliance, and prevent security vulnerabilities.

## Authentication and Authorization

### User Authentication Standards

- **Primary Method**: Windows Integrated Authentication (for internal users)
- **API Authentication**: JWT (JSON Web Tokens) with 1-hour expiration
- **Service Accounts**: Managed identities or Windows service accounts (never hard-coded credentials)

### Authorization Patterns

#### COBOL Authorization Check

```cobol
WORKING-STORAGE SECTION.
01  WS-USER-ID               PIC X(30).
01  WS-USER-ROLE             PIC X(20).
01  WS-REQUIRED-PERMISSION   PIC X(30).
01  WS-HAS-PERMISSION        PIC X VALUE 'N'.
    88  USER-AUTHORIZED      VALUE 'Y'.
    88  USER-NOT-AUTHORIZED  VALUE 'N'.

PROCEDURE DIVISION.
*    Get current user from environment or parameter
     ACCEPT WS-USER-ID FROM ENVIRONMENT "USERNAME".

*    Check if user has required permission
     MOVE 'UPDATE_CUSTOMER_CREDIT' TO WS-REQUIRED-PERMISSION.
     PERFORM 9500-CHECK-PERMISSION.

     IF USER-NOT-AUTHORIZED
         DISPLAY 'ERROR: User ' WS-USER-ID ' not authorized'
         DISPLAY 'Required permission: ' WS-REQUIRED-PERMISSION
         PERFORM 9999-UNAUTHORIZED-EXIT
     END-IF.

*    Proceed with authorized operation
     PERFORM 2000-UPDATE-CREDIT-LIMIT.

9500-CHECK-PERMISSION.
     EXEC SQL
         SELECT 'Y'
           INTO :WS-HAS-PERMISSION
           FROM SYS_USER_ROLE UR
           JOIN SYS_ROLE_PERMISSION RP
             ON UR.ROLE_CODE = RP.ROLE_CODE
          WHERE UR.USER_ID = :WS-USER-ID
            AND RP.PERMISSION_CODE = :WS-REQUIRED-PERMISSION
            AND UR.IS_ACTIVE_FLAG = 'Y'
            AND RP.IS_ACTIVE_FLAG = 'Y'
     END-EXEC.

     IF SQLCODE = 100
         SET USER-NOT-AUTHORIZED TO TRUE
     END-IF.
```

#### C# Authorization with Policy-Based Access

```csharp
[Authorize(Policy = "UpdateCustomerCredit")]
[HttpPut("customers/{id}/credit-limit")]
public async Task<IActionResult> UpdateCreditLimit(
    int id,
    [FromBody] UpdateCreditLimitRequest request)
{
    var result = await _customerService.UpdateCreditLimitAsync(id, request);
    return result.IsSuccess ? NoContent() : BadRequest(result.Error);
}

// In Startup.cs or Program.cs
services.AddAuthorization(options =>
{
    options.AddPolicy("UpdateCustomerCredit", policy =>
        policy.RequireClaim("Permission", "UPDATE_CUSTOMER_CREDIT"));

    options.AddPolicy("ViewFinancialData", policy =>
        policy.RequireRole("Finance", "Accounting", "Executive"));

    options.AddPolicy("ManufacturingManager", policy =>
        policy.RequireRole("MfgManager", "PlantManager", "Admin"));
});
```

### Role-Based Access Control (RBAC)

#### Standard Roles

```
Role Code    | Description                  | Typical Permissions
-------------|------------------------------|--------------------
ADMIN        | System Administrator         | All permissions
EXEC         | Executive Management         | Read all, approve high-value
SALES        | Sales Representative         | Customer orders, quotes
SALESM       | Sales Manager                | Sales + pricing, credit overrides
PROD         | Production Worker            | MO operations, time entry
PRODM        | Production Manager           | PROD + scheduling, release MOs
PURCH        | Purchasing Agent             | Create POs, receive materials
PURCHM       | Purchasing Manager           | PURCH + vendor management, approve POs
INVCTL       | Inventory Controller         | Inventory transactions, cycle counts
QAINSP       | Quality Inspector            | Inspection results, NCRs
QAENG        | Quality Engineer             | QA + approve dispositions, SOPs
ACCTG        | Accounting Clerk             | GL entry, invoice processing
ACCTM        | Accounting Manager           | ACCTG + close periods, approvals
ENG          | Engineer                     | BOM/routing changes, ECOs
ENGM         | Engineering Manager          | ENG + approve ECOs
READONLY     | Read-Only User               | View-only access
```

## Input Validation and Sanitization

### SQL Injection Prevention

**ALWAYS use parameterized queries:**

#### COBOL - CORRECT ✅

```cobol
MOVE INPUT-CUSTOMER-ID TO WS-CUST-ID.
EXEC SQL
    SELECT CUSTOMER_NAME
      INTO :WS-CUST-NAME
      FROM CUST_MASTER
     WHERE CUST_MASTER_ID = :WS-CUST-ID
END-EXEC.
```

#### COBOL - WRONG ❌ (Never do this!)

```cobol
STRING 'SELECT CUSTOMER_NAME FROM CUST_MASTER WHERE CUST_MASTER_ID = '
       INPUT-CUSTOMER-ID
       DELIMITED BY SIZE INTO WS-SQL-STRING.
EXEC SQL EXECUTE IMMEDIATE :WS-SQL-STRING END-EXEC.
```

#### C# - CORRECT ✅

```csharp
// Entity Framework - automatic parameterization
var customer = await _context.Customers
    .FirstOrDefaultAsync(c => c.CustomerId == customerId);

// Raw SQL with parameters
var customers = await _context.Customers
    .FromSqlRaw("SELECT * FROM CUST_MASTER WHERE CUSTOMER_NAME LIKE {0}", searchTerm)
    .ToListAsync();
```

#### C# - WRONG ❌ (Never do this!)

```csharp
var sql = $"SELECT * FROM CUST_MASTER WHERE CUSTOMER_NAME = '{searchTerm}'";
var customers = _context.Customers.FromSqlRaw(sql).ToList();
```

### Input Validation Standards

```csharp
public class UpdateCustomerValidator : AbstractValidator<UpdateCustomerRequest>
{
    public UpdateCustomerValidator()
    {
        // Required fields
        RuleFor(x => x.CustomerId)
            .GreaterThan(0)
            .WithMessage("Customer ID must be positive");

        RuleFor(x => x.Name)
            .NotEmpty().WithMessage("Customer name is required")
            .MaximumLength(50).WithMessage("Name cannot exceed 50 characters")
            .Matches(@"^[a-zA-Z0-9\s\-\.,']+$").WithMessage("Name contains invalid characters");

        // Email validation
        RuleFor(x => x.Email)
            .EmailAddress().When(x => !string.IsNullOrEmpty(x.Email))
            .WithMessage("Invalid email format");

        // Range validation
        RuleFor(x => x.CreditLimit)
            .InclusiveBetween(0, 10_000_000)
            .WithMessage("Credit limit must be between $0 and $10,000,000");

        // Pattern matching
        RuleFor(x => x.PhoneNumber)
            .Matches(@"^\d{3}-\d{3}-\d{4}$").When(x => !string.IsNullOrEmpty(x.PhoneNumber))
            .WithMessage("Phone must be in format: 555-123-4567");

        // Custom business rule
        RuleFor(x => x.PaymentTerms)
            .Must(BeValidPaymentTerms)
            .WithMessage("Invalid payment terms code");
    }

    private bool BeValidPaymentTerms(string terms)
    {
        var validTerms = new[] { "NET30", "NET60", "2/10NET30", "COD", "PREPAY" };
        return validTerms.Contains(terms);
    }
}
```

### Output Encoding

```csharp
// HTML encoding for web display
public string GetSafeCustomerName(string name)
{
    return HttpUtility.HtmlEncode(name);
}

// JavaScript encoding for JSON
public string GetSafeJsonValue(string value)
{
    return JsonSerializer.Serialize(value).Trim('"');
}

// URL encoding for query parameters
public string GetSafeUrlParameter(string param)
{
    return Uri.EscapeDataString(param);
}
```

## Sensitive Data Protection

### Encryption at Rest

```csharp
using System.Security.Cryptography;

public class DataProtectionService
{
    private readonly IDataProtectionProvider _provider;

    public DataProtectionService(IDataProtectionProvider provider)
    {
        _provider = provider;
    }

    public string EncryptSensitiveData(string plaintext)
    {
        var protector = _provider.CreateProtector("ErpSensitiveData");
        return protector.Protect(plaintext);
    }

    public string DecryptSensitiveData(string ciphertext)
    {
        var protector = _provider.CreateProtector("ErpSensitiveData");
        return protector.Unprotect(ciphertext);
    }
}
```

### Sensitive Data Fields

**Never log or display these fields in plain text:**

- Credit card numbers
- Bank account numbers
- Social Security Numbers (SSN)
- Tax IDs
- Passwords or password hashes
- API keys or tokens
- Personally Identifiable Information (PII) when not necessary

### Data Masking

```csharp
public static class DataMasking
{
    public static string MaskCreditCard(string cardNumber)
    {
        if (string.IsNullOrEmpty(cardNumber) || cardNumber.Length < 4)
            return "****";

        return $"****-****-****-{cardNumber[^4..]}";
    }

    public static string MaskSsn(string ssn)
    {
        if (string.IsNullOrEmpty(ssn) || ssn.Length < 4)
            return "***-**-****";

        return $"***-**-{ssn[^4..]}";
    }

    public static string MaskEmail(string email)
    {
        if (string.IsNullOrEmpty(email) || !email.Contains("@"))
            return "****@****.com";

        var parts = email.Split('@');
        var local = parts[0].Length > 2 ? $"{parts[0][..2]}****" : "****";
        return $"{local}@{parts[1]}";
    }
}
```

## Audit Trail Requirements

### Comprehensive Audit Logging

```cobol
*    Standard audit trail insert pattern
PROCEDURE DIVISION.
3900-WRITE-AUDIT-TRAIL.
     MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-TIMESTAMP.

     EXEC SQL
         INSERT INTO AUDIT_CHANGE_LOG (
             TABLE_NAME,
             RECORD_ID,
             FIELD_NAME,
             OLD_VALUE,
             NEW_VALUE,
             CHANGE_TYPE,
             CHANGED_BY,
             CHANGED_DATE,
             PROGRAM_NAME,
             TRANSACTION_ID
         ) VALUES (
             :WS-TABLE-NAME,
             :WS-RECORD-ID,
             :WS-FIELD-NAME,
             :WS-OLD-VALUE,
             :WS-NEW-VALUE,
             :WS-CHANGE-TYPE,
             :WS-USER-ID,
             CURRENT_TIMESTAMP,
             :WS-PROGRAM-ID,
             :WS-TRANSACTION-ID
         )
     END-EXEC.

     IF SQLCODE NOT = 0
         DISPLAY 'Warning: Audit trail insert failed: ' SQLCODE
         *    Continue processing - audit failure should not block business
     END-IF.
```

```csharp
public class AuditService
{
    private readonly ErpDbContext _context;
    private readonly IHttpContextAccessor _httpContext;

    public async Task LogChangeAsync<T>(
        string tableName,
        int recordId,
        string fieldName,
        string oldValue,
        string newValue,
        string changeType = "UPDATE")
    {
        var userId = _httpContext.HttpContext?.User?.Identity?.Name ?? "SYSTEM";

        var auditLog = new AuditChangeLog
        {
            TableName = tableName,
            RecordId = recordId,
            FieldName = fieldName,
            OldValue = oldValue?.Substring(0, Math.Min(oldValue.Length, 500)),
            NewValue = newValue?.Substring(0, Math.Min(newValue.Length, 500)),
            ChangeType = changeType,
            ChangedBy = userId,
            ChangedDate = DateTime.UtcNow,
            ProgramName = typeof(T).Name,
            IpAddress = _httpContext.HttpContext?.Connection?.RemoteIpAddress?.ToString()
        };

        _context.AuditChangeLogs.Add(auditLog);
        await _context.SaveChangesAsync();
    }
}
```

### What to Audit

**ALWAYS log:**

- All INSERT, UPDATE, DELETE operations on master data
- Authentication attempts (success and failure)
- Authorization failures (attempted unauthorized access)
- High-value transactions (orders >$10K, credit limit changes)
- Configuration changes
- User permission changes
- Data exports
- Financial adjustments

**DO NOT log:**

- SELECT queries (too voluminous)
- Password values (security risk)
- Excessive detail for low-value transactions

## Secrets Management

### Configuration Management

```csharp
// appsettings.json - NEVER store secrets here
{
  "ConnectionStrings": {
    "ErpDatabase": "Server=ERPSQL;Database=Manufacturing;Integrated Security=True"
  }
}

// Use Azure Key Vault or User Secrets for sensitive data
public class Startup
{
    public void ConfigureAppConfiguration(IConfigurationBuilder builder)
    {
        if (Environment.IsDevelopment())
        {
            builder.AddUserSecrets<Startup>();
        }
        else
        {
            builder.AddAzureKeyVault(
                $"https://{Environment.GetEnvironmentVariable("KEY_VAULT_NAME")}.vault.azure.net/",
                new DefaultAzureCredential());
        }
    }
}

// Access secrets
var apiKey = _configuration["ThirdPartyApi:ApiKey"];
```

### COBOL Configuration (Environment Variables)

```cobol
WORKING-STORAGE SECTION.
01  WS-DB-PASSWORD           PIC X(100).

PROCEDURE DIVISION.
*    Get password from environment variable, not hard-coded
     ACCEPT WS-DB-PASSWORD FROM ENVIRONMENT "ERP_DB_PASSWORD".

     IF WS-DB-PASSWORD = SPACES
         DISPLAY 'ERROR: Database password not configured'
         PERFORM 9999-ABEND-ROUTINE
     END-IF.
```

**NEVER hard-code:**

- Database passwords
- API keys
- Encryption keys
- Service account credentials
- OAuth client secrets

## Error Handling and Information Disclosure

### Safe Error Messages

```csharp
// CORRECT ✅ - Generic error message to user
public IActionResult ProcessOrder(OrderRequest request)
{
    try
    {
        // Process order
    }
    catch (Exception ex)
    {
        // Log detailed error internally
        _logger.LogError(ex, "Order processing failed for customer {CustomerId}", request.CustomerId);

        // Return generic message to user
        return StatusCode(500, new
        {
            Message = "An error occurred processing your order. Please contact support.",
            ReferenceId = Guid.NewGuid() // For support to trace
        });
    }
}

// WRONG ❌ - Exposes internal details
public IActionResult ProcessOrder(OrderRequest request)
{
    try
    {
        // Process order
    }
    catch (SqlException ex)
    {
        return BadRequest($"Database error: {ex.Message}\nQuery: {ex.Procedure}");
    }
}
```

## Compliance Requirements

### FDA 21 CFR Part 11 (Electronic Records)

- **Electronic Signatures**: Require password re-authentication for critical operations
- **Audit Trail**: Immutable record of all changes (no deletion/modification of audit logs)
- **System Validation**: Document and test all critical functionality
- **Access Controls**: Role-based access with periodic reviews

### SOX Compliance (Sarbanes-Oxley)

- **Segregation of Duties**: No single person can initiate, approve, and record transactions
- **Change Management**: All code changes require approval and testing
- **Access Logging**: Log all access to financial data
- **Data Retention**: 7-year retention for financial records

### GDPR (if applicable)

- **Data Minimization**: Only collect necessary PII
- **Right to Erasure**: Support data deletion requests
- **Data Portability**: Provide export functionality
- **Consent Management**: Track consent for data processing

### ISO 9001 (Quality Management)

- **Document Control**: Version control for procedures and work instructions
- **Traceability**: Lot/serial tracking for materials
- **Corrective Actions**: Track and resolve quality issues
- **Management Review**: Periodic audit and review processes

## Secure Coding Practices

### Prevent Common Vulnerabilities

#### Prevent Directory Traversal

```csharp
// CORRECT ✅
public IActionResult DownloadFile(string fileName)
{
    // Whitelist allowed characters
    if (!Regex.IsMatch(fileName, @"^[a-zA-Z0-9_\-\.]+$"))
    {
        return BadRequest("Invalid file name");
    }

    var safePath = Path.Combine(_baseDirectory, fileName);

    // Ensure path is within allowed directory
    var fullPath = Path.GetFullPath(safePath);
    if (!fullPath.StartsWith(_baseDirectory))
    {
        return BadRequest("Invalid file path");
    }

    return File(System.IO.File.ReadAllBytes(fullPath), "application/octet-stream");
}

// WRONG ❌
public IActionResult DownloadFile(string fileName)
{
    var path = Path.Combine(_baseDirectory, fileName);
    return File(System.IO.File.ReadAllBytes(path), "application/octet-stream");
}
```

#### Prevent XML External Entity (XXE) Attacks

```csharp
// CORRECT ✅
public XmlDocument SafeLoadXml(string xml)
{
    var settings = new XmlReaderSettings
    {
        DtdProcessing = DtdProcessing.Prohibit,
        XmlResolver = null
    };

    using var stringReader = new StringReader(xml);
    using var xmlReader = XmlReader.Create(stringReader, settings);

    var doc = new XmlDocument();
    doc.Load(xmlReader);
    return doc;
}
```

#### Prevent Insecure Deserialization

```csharp
// Use modern JSON serialization with type constraints
public T DeserializeJson<T>(string json) where T : class
{
    var options = new JsonSerializerOptions
    {
        PropertyNameCaseInsensitive = true,
        MaxDepth = 32 // Prevent stack overflow
    };

    return JsonSerializer.Deserialize<T>(json, options);
}

// DO NOT use BinaryFormatter - it's insecure
```

## AI Code Generation Security Rules

When AI generates code, it MUST:

1. Use parameterized queries (NEVER string concatenation for SQL)
2. Validate and sanitize all user inputs
3. Implement authorization checks before sensitive operations
4. Create audit trail entries for data modifications
5. Use encryption for sensitive data at rest and in transit
6. Handle errors without exposing internal details
7. Never hard-code secrets or credentials
8. Implement proper session management and timeout
9. Use HTTPS for all API endpoints
10. Follow principle of least privilege for database access

When AI generates code, it MUST NOT:

1. Create SQL injection vulnerabilities
2. Expose sensitive data in logs or error messages
3. Store passwords in plain text
4. Skip input validation
5. Disable security features (e.g., SSL verification)
6. Use weak cryptography (MD5, SHA1 for security purposes)
7. Trust user input without validation
8. Allow unauthorized access to resources
9. Create hard-coded credentials
10. Ignore error conditions silently

## Security Code Review Checklist

- [ ] No SQL injection vulnerabilities (parameterized queries used)
- [ ] Input validation present for all user inputs
- [ ] Output encoding for display contexts (HTML, JavaScript, URL)
- [ ] Authorization checks before sensitive operations
- [ ] Audit trail logging for data changes
- [ ] Secrets not hard-coded (use configuration/Key Vault)
- [ ] Error messages don't expose internal details
- [ ] Encryption used for sensitive data
- [ ] HTTPS enforced for web endpoints
- [ ] Session management secure (timeout, secure cookies)
- [ ] No insecure deserialization
- [ ] File upload validation (size, type, content)
- [ ] Rate limiting for APIs
- [ ] CORS configured properly (not wide-open)
- [ ] Dependencies up-to-date (no known vulnerabilities)

## References

- OWASP Top 10: https://owasp.org/www-project-top-ten/
- OWASP Cheat Sheet Series: https://cheatsheetseries.owasp.org/
- FDA 21 CFR Part 11 Guidance
- ISO 27001 Information Security Standard
- Microsoft Security Development Lifecycle
- NIST Cybersecurity Framework
