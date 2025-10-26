# C# Integration Patterns for Manufacturing ERP

## Overview

This rule file defines standards for C# code that integrates with legacy COBOL/VB.NET systems and implements modern services in our manufacturing ERP system.

## C# Version & Framework

- **Target Frameworks**:
  - .NET Framework 4.8 (for COM interop with legacy)
  - .NET 6+ (for new services and APIs)
- **C# Language Version**: C# 10.0+ (use latest features)
- **IDE**: Visual Studio 2022 or Visual Studio Code with C# Dev Kit

## Modern Architecture Patterns

### Clean Architecture Structure

```
/ErpModernization
    /Core
        /Domain          # Entities, Value Objects, Domain Events
        /Application     # Use Cases, Interfaces, DTOs
    /Infrastructure
        /Persistence     # Entity Framework, Repositories
        /Legacy          # COBOL/VB.NET integration
        /External        # Third-party services
    /Presentation
        /WebApi          # REST/GraphQL APIs
        /BlazorApp       # Modern UI
    /Tests
        /UnitTests
        /IntegrationTests
```

## COBOL Integration from C#

### P/Invoke for COBOL DLLs

```csharp
using System.Runtime.InteropServices;

public static class CobolInterop
{
    // Import COBOL program compiled as DLL
    [DllImport("CUSTMAIN.dll", CallingConvention = CallingConvention.StdCall)]
    private static extern int CUSTMAIN(
        ref int customerId,
        [MarshalAs(UnmanagedType.LPStr)] string customerName,
        ref decimal creditLimit,
        out int returnCode);

    public static Result<CustomerData> GetCustomer(int customerId)
    {
        try
        {
            // COBOL expects fixed-length strings
            var customerName = new string(' ', 30);
            decimal creditLimit = 0;
            int returnCode;

            int result = CUSTMAIN(
                ref customerId,
                customerName,
                ref creditLimit,
                out returnCode);

            if (returnCode == 0)
            {
                return Result<CustomerData>.Success(new CustomerData
                {
                    Id = customerId,
                    Name = customerName.Trim(),
                    CreditLimit = creditLimit
                });
            }

            return Result<CustomerData>.Failure($"COBOL error code: {returnCode}");
        }
        catch (Exception ex)
        {
            return Result<CustomerData>.Failure($"Interop error: {ex.Message}");
        }
    }
}
```

### NetCOBOL for .NET Integration

```csharp
// For Fujitsu NetCOBOL compiled as .NET assemblies
using FujitsuCobolAssembly;

public class CobolProgramWrapper : IDisposable
{
    private readonly CUSTMAIN _cobolProgram;
    private bool _disposed;

    public CobolProgramWrapper()
    {
        _cobolProgram = new CUSTMAIN();
    }

    public async Task<Result<Customer>> GetCustomerAsync(int customerId)
    {
        return await Task.Run(() =>
        {
            // Set input parameters
            _cobolProgram.CUSTOMER_ID = customerId;

            // Execute COBOL program
            _cobolProgram.Execute();

            // Check return code
            if (_cobolProgram.RETURN_CODE == "00")
            {
                return Result<Customer>.Success(new Customer
                {
                    Id = _cobolProgram.CUSTOMER_ID,
                    Name = _cobolProgram.CUSTOMER_NAME.Trim(),
                    CreditLimit = _cobolProgram.CREDIT_LIMIT
                });
            }

            return Result<Customer>.Failure($"COBOL returned: {_cobolProgram.RETURN_CODE}");
        });
    }

    public void Dispose()
    {
        if (!_disposed)
        {
            _cobolProgram?.Dispose();
            _disposed = true;
        }
        GC.SuppressFinalize(this);
    }
}
```

## Data Type Mapping

### COBOL to C# Type Mapping

```csharp
public static class CobolDataMapper
{
    // COBOL PIC 9(n) -> C# int/long
    public static int ToInt32(string cobolNumeric)
    {
        return int.Parse(cobolNumeric.Trim());
    }

    // COBOL PIC X(n) -> C# string (trim spaces)
    public static string ToString(string cobolString, int length)
    {
        return cobolString?.PadRight(length)[..length].Trim() ?? string.Empty;
    }

    // C# string -> COBOL PIC X(n) (pad with spaces)
    public static string ToCobolString(string value, int length)
    {
        return (value ?? string.Empty).PadRight(length)[..length];
    }

    // COBOL PIC 9(n)V99 COMP-3 -> C# decimal
    public static decimal ToDecimal(byte[] packedDecimal)
    {
        // Unpack COMP-3 (packed decimal) format
        return UnpackComp3(packedDecimal);
    }

    // C# decimal -> COBOL COMP-3
    public static byte[] ToComp3(decimal value, int totalDigits, int decimalPlaces)
    {
        // Pack decimal into COMP-3 format
        return PackComp3(value, totalDigits, decimalPlaces);
    }

    private static decimal UnpackComp3(byte[] packed)
    {
        // Implementation of COMP-3 unpacking logic
        var result = 0m;
        var multiplier = 1m;

        for (int i = packed.Length - 1; i >= 0; i--)
        {
            var highNibble = (packed[i] & 0xF0) >> 4;
            var lowNibble = packed[i] & 0x0F;

            if (i == packed.Length - 1)
            {
                // Last byte: low nibble is sign
                result = highNibble;
                var isNegative = (lowNibble == 0x0D);
                result = isNegative ? -result : result;
            }
            else
            {
                result += lowNibble * multiplier;
                multiplier *= 10;
                result += highNibble * multiplier;
                multiplier *= 10;
            }
        }

        return result;
    }

    private static byte[] PackComp3(decimal value, int totalDigits, int decimalPlaces)
    {
        // Implementation of COMP-3 packing logic
        var isNegative = value < 0;
        var absValue = Math.Abs(value);
        var scaledValue = (long)(absValue * (decimal)Math.Pow(10, decimalPlaces));
        var digits = scaledValue.ToString().PadLeft(totalDigits, '0');

        var byteCount = (totalDigits + 1) / 2 + 1;
        var result = new byte[byteCount];

        // Pack digits into bytes
        for (int i = 0, j = 0; i < digits.Length; i += 2, j++)
        {
            var high = digits[i] - '0';
            var low = i + 1 < digits.Length ? digits[i + 1] - '0' : 0;
            result[j] = (byte)((high << 4) | low);
        }

        // Set sign nibble
        result[^1] = (byte)((result[^1] & 0xF0) | (isNegative ? 0x0D : 0x0C));

        return result;
    }

    // Date conversion: COBOL date (YYYYMMDD) to C# DateTime
    public static DateTime? ToDateTime(string cobolDate)
    {
        if (string.IsNullOrWhiteSpace(cobolDate) || cobolDate.Length != 8)
            return null;

        if (DateTime.TryParseExact(cobolDate, "yyyyMMdd",
            CultureInfo.InvariantCulture, DateTimeStyles.None, out var result))
        {
            return result;
        }

        return null;
    }

    // C# DateTime to COBOL date (YYYYMMDD)
    public static string ToCobolDate(DateTime? date)
    {
        return date?.ToString("yyyyMMdd") ?? "00000000";
    }
}
```

## Repository Pattern for Legacy Data Access

### Generic Repository Interface

```csharp
public interface IRepository<T> where T : class
{
    Task<T?> GetByIdAsync(int id, CancellationToken cancellationToken = default);
    Task<IEnumerable<T>> GetAllAsync(CancellationToken cancellationToken = default);
    Task<T> AddAsync(T entity, CancellationToken cancellationToken = default);
    Task UpdateAsync(T entity, CancellationToken cancellationToken = default);
    Task DeleteAsync(int id, CancellationToken cancellationToken = default);
}

public class CustomerRepository : IRepository<Customer>
{
    private readonly ErpDbContext _context;
    private readonly ICobolInterop _cobolInterop;
    private readonly ILogger<CustomerRepository> _logger;

    public CustomerRepository(
        ErpDbContext context,
        ICobolInterop cobolInterop,
        ILogger<CustomerRepository> logger)
    {
        _context = context;
        _cobolInterop = cobolInterop;
        _logger = logger;
    }

    public async Task<Customer?> GetByIdAsync(int id, CancellationToken cancellationToken = default)
    {
        // Option 1: Direct database access (faster)
        return await _context.Customers
            .AsNoTracking()
            .FirstOrDefaultAsync(c => c.Id == id, cancellationToken);

        // Option 2: Use COBOL program (maintains business logic)
        // var result = await _cobolInterop.GetCustomerAsync(id);
        // return result.IsSuccess ? result.Value : null;
    }

    public async Task UpdateAsync(Customer entity, CancellationToken cancellationToken = default)
    {
        // Always use COBOL for updates to maintain business rules and audit trail
        var result = await _cobolInterop.UpdateCustomerAsync(entity);

        if (!result.IsSuccess)
        {
            _logger.LogError("COBOL update failed: {Error}", result.Error);
            throw new InvalidOperationException(result.Error);
        }
    }
}
```

## Modern Web API Layer

### REST API with Minimal APIs (NET 6+)

```csharp
using Microsoft.AspNetCore.Mvc;

var builder = WebApplication.CreateBuilder(args);

// Add services
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen();
builder.Services.AddScoped<ICustomerService, CustomerService>();
builder.Services.AddScoped<ICobolInterop, CobolInteropService>();

var app = builder.Build();

// Configure middleware
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();
}

app.UseHttpsRedirection();

// Customer endpoints
app.MapGet("/api/customers/{id:int}", async (
    int id,
    [FromServices] ICustomerService customerService,
    CancellationToken cancellationToken) =>
{
    var result = await customerService.GetCustomerAsync(id, cancellationToken);
    return result.IsSuccess
        ? Results.Ok(result.Value)
        : Results.NotFound(result.Error);
})
.WithName("GetCustomer")
.WithOpenApi();

app.MapPost("/api/customers", async (
    [FromBody] CreateCustomerRequest request,
    [FromServices] ICustomerService customerService,
    CancellationToken cancellationToken) =>
{
    var result = await customerService.CreateCustomerAsync(request, cancellationToken);
    return result.IsSuccess
        ? Results.Created($"/api/customers/{result.Value.Id}", result.Value)
        : Results.BadRequest(result.Error);
})
.WithName("CreateCustomer")
.WithOpenApi();

app.Run();
```

### Controller-based API (Traditional)

```csharp
[ApiController]
[Route("api/[controller]")]
public class CustomersController : ControllerBase
{
    private readonly ICustomerService _customerService;
    private readonly ILogger<CustomersController> _logger;

    public CustomersController(
        ICustomerService customerService,
        ILogger<CustomersController> logger)
    {
        _customerService = customerService;
        _logger = logger;
    }

    /// <summary>
    /// Gets a customer by ID
    /// </summary>
    /// <param name="id">Customer ID</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Customer details</returns>
    [HttpGet("{id}")]
    [ProducesResponseType(typeof(CustomerDto), StatusCodes.Status200OK)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<ActionResult<CustomerDto>> GetCustomer(
        int id,
        CancellationToken cancellationToken)
    {
        _logger.LogInformation("Getting customer {CustomerId}", id);

        var result = await _customerService.GetCustomerAsync(id, cancellationToken);

        if (result.IsSuccess)
        {
            return Ok(result.Value);
        }

        _logger.LogWarning("Customer {CustomerId} not found", id);
        return NotFound(new { message = result.Error });
    }

    /// <summary>
    /// Updates a customer
    /// </summary>
    [HttpPut("{id}")]
    [ProducesResponseType(StatusCodes.Status204NoContent)]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    public async Task<IActionResult> UpdateCustomer(
        int id,
        [FromBody] UpdateCustomerRequest request,
        CancellationToken cancellationToken)
    {
        if (id != request.Id)
        {
            return BadRequest(new { message = "ID mismatch" });
        }

        var result = await _customerService.UpdateCustomerAsync(request, cancellationToken);

        if (result.IsSuccess)
        {
            return NoContent();
        }

        if (result.Error.Contains("not found", StringComparison.OrdinalIgnoreCase))
        {
            return NotFound(new { message = result.Error });
        }

        return BadRequest(new { message = result.Error });
    }
}
```

## CQRS Pattern for Complex Operations

### Command Handler

```csharp
public record CreateOrderCommand(int CustomerId, List<OrderLineItem> Items) : IRequest<Result<int>>;

public class CreateOrderCommandHandler : IRequestHandler<CreateOrderCommand, Result<int>>
{
    private readonly ErpDbContext _context;
    private readonly ICobolInterop _cobolInterop;
    private readonly IEventPublisher _eventPublisher;
    private readonly ILogger<CreateOrderCommandHandler> _logger;

    public CreateOrderCommandHandler(
        ErpDbContext context,
        ICobolInterop cobolInterop,
        IEventPublisher eventPublisher,
        ILogger<CreateOrderCommandHandler> logger)
    {
        _context = context;
        _cobolInterop = cobolInterop;
        _eventPublisher = eventPublisher;
        _logger = logger;
    }

    public async Task<Result<int>> Handle(CreateOrderCommand request, CancellationToken cancellationToken)
    {
        using var transaction = await _context.Database.BeginTransactionAsync(cancellationToken);

        try
        {
            // Validate customer using COBOL business logic
            var customerResult = await _cobolInterop.ValidateCustomerAsync(request.CustomerId);
            if (!customerResult.IsSuccess)
            {
                return Result<int>.Failure($"Invalid customer: {customerResult.Error}");
            }

            // Process order through COBOL (maintains legacy business rules)
            var orderResult = await _cobolInterop.CreateOrderAsync(new CobolOrderRequest
            {
                CustomerId = request.CustomerId,
                Items = request.Items.Select(item => new CobolOrderItem
                {
                    ItemId = item.ItemId,
                    Quantity = item.Quantity,
                    UnitPrice = item.UnitPrice
                }).ToList()
            });

            if (!orderResult.IsSuccess)
            {
                await transaction.RollbackAsync(cancellationToken);
                return Result<int>.Failure(orderResult.Error);
            }

            await transaction.CommitAsync(cancellationToken);

            // Publish domain event for other services
            await _eventPublisher.PublishAsync(new OrderCreatedEvent
            {
                OrderId = orderResult.Value,
                CustomerId = request.CustomerId,
                CreatedAt = DateTime.UtcNow
            }, cancellationToken);

            _logger.LogInformation("Order {OrderId} created successfully", orderResult.Value);

            return Result<int>.Success(orderResult.Value);
        }
        catch (Exception ex)
        {
            await transaction.RollbackAsync(cancellationToken);
            _logger.LogError(ex, "Error creating order for customer {CustomerId}", request.CustomerId);
            return Result<int>.Failure($"System error: {ex.Message}");
        }
    }
}
```

### Query Handler

```csharp
public record GetCustomerOrdersQuery(int CustomerId, int PageNumber, int PageSize) : IRequest<Result<PagedList<OrderSummary>>>;

public class GetCustomerOrdersQueryHandler : IRequestHandler<GetCustomerOrdersQuery, Result<PagedList<OrderSummary>>>
{
    private readonly ErpDbContext _context;

    public GetCustomerOrdersQueryHandler(ErpDbContext context)
    {
        _context = context;
    }

    public async Task<Result<PagedList<OrderSummary>>> Handle(
        GetCustomerOrdersQuery request,
        CancellationToken cancellationToken)
    {
        var query = _context.Orders
            .AsNoTracking()
            .Where(o => o.CustomerId == request.CustomerId)
            .OrderByDescending(o => o.OrderDate)
            .Select(o => new OrderSummary
            {
                OrderId = o.Id,
                OrderNumber = o.OrderNumber,
                OrderDate = o.OrderDate,
                TotalAmount = o.TotalAmount,
                Status = o.Status
            });

        var totalCount = await query.CountAsync(cancellationToken);
        var items = await query
            .Skip((request.PageNumber - 1) * request.PageSize)
            .Take(request.PageSize)
            .ToListAsync(cancellationToken);

        var pagedList = new PagedList<OrderSummary>(items, totalCount, request.PageNumber, request.PageSize);

        return Result<PagedList<OrderSummary>>.Success(pagedList);
    }
}
```

## Error Handling and Result Pattern

### Result Type

```csharp
public class Result<T>
{
    public bool IsSuccess { get; }
    public T? Value { get; }
    public string Error { get; }

    private Result(bool isSuccess, T? value, string error)
    {
        IsSuccess = isSuccess;
        Value = value;
        Error = error ?? string.Empty;
    }

    public static Result<T> Success(T value) => new(true, value, string.Empty);
    public static Result<T> Failure(string error) => new(false, default, error);

    public TResult Match<TResult>(Func<T, TResult> success, Func<string, TResult> failure)
        => IsSuccess ? success(Value!) : failure(Error);
}

// Non-generic version for operations without return value
public class Result
{
    public bool IsSuccess { get; }
    public string Error { get; }

    private Result(bool isSuccess, string error)
    {
        IsSuccess = isSuccess;
        Error = error ?? string.Empty;
    }

    public static Result Success() => new(true, string.Empty);
    public static Result Failure(string error) => new(false, error);
}
```

### Global Exception Handler

```csharp
public class GlobalExceptionHandler : IExceptionHandler
{
    private readonly ILogger<GlobalExceptionHandler> _logger;

    public GlobalExceptionHandler(ILogger<GlobalExceptionHandler> logger)
    {
        _logger = logger;
    }

    public async ValueTask<bool> TryHandleAsync(
        HttpContext httpContext,
        Exception exception,
        CancellationToken cancellationToken)
    {
        _logger.LogError(exception, "An unhandled exception occurred");

        var problemDetails = exception switch
        {
            ValidationException validationEx => new ProblemDetails
            {
                Status = StatusCodes.Status400BadRequest,
                Title = "Validation Error",
                Detail = validationEx.Message,
                Instance = httpContext.Request.Path
            },
            NotFoundException notFoundEx => new ProblemDetails
            {
                Status = StatusCodes.Status404NotFound,
                Title = "Resource Not Found",
                Detail = notFoundEx.Message,
                Instance = httpContext.Request.Path
            },
            CobolInteropException cobolEx => new ProblemDetails
            {
                Status = StatusCodes.Status500InternalServerError,
                Title = "Legacy System Error",
                Detail = $"COBOL error code: {cobolEx.ErrorCode}",
                Instance = httpContext.Request.Path
            },
            _ => new ProblemDetails
            {
                Status = StatusCodes.Status500InternalServerError,
                Title = "Internal Server Error",
                Detail = "An unexpected error occurred",
                Instance = httpContext.Request.Path
            }
        };

        httpContext.Response.StatusCode = problemDetails.Status ?? 500;
        await httpContext.Response.WriteAsJsonAsync(problemDetails, cancellationToken);

        return true;
    }
}
```

## Dependency Injection Setup

### Service Registration

```csharp
public static class ServiceCollectionExtensions
{
    public static IServiceCollection AddErpServices(this IServiceCollection services, IConfiguration configuration)
    {
        // Database
        services.AddDbContext<ErpDbContext>(options =>
            options.UseSqlServer(configuration.GetConnectionString("ErpDatabase")));

        // Legacy Integration
        services.AddScoped<ICobolInterop, CobolInteropService>();
        services.AddScoped<IVbNetInterop, VbNetInteropService>();

        // Repositories
        services.AddScoped<ICustomerRepository, CustomerRepository>();
        services.AddScoped<IOrderRepository, OrderRepository>();
        services.AddScoped<IInventoryRepository, InventoryRepository>();

        // Application Services
        services.AddScoped<ICustomerService, CustomerService>();
        services.AddScoped<IOrderService, OrderService>();
        services.AddScoped<IInventoryService, InventoryService>();

        // MediatR for CQRS
        services.AddMediatR(cfg => cfg.RegisterServicesFromAssembly(Assembly.GetExecutingAssembly()));

        // AutoMapper for DTO mapping
        services.AddAutoMapper(Assembly.GetExecutingAssembly());

        // FluentValidation
        services.AddValidatorsFromAssembly(Assembly.GetExecutingAssembly());

        // Caching
        services.AddMemoryCache();
        services.AddDistributedMemoryCache(); // Or Redis in production

        // Logging
        services.AddLogging(builder =>
        {
            builder.AddConsole();
            builder.AddDebug();
            builder.AddApplicationInsights(); // Azure Application Insights
        });

        return services;
    }
}
```

## Background Jobs with Hangfire

### Background Job Setup

```csharp
public class InventorySyncJob
{
    private readonly ICobolInterop _cobolInterop;
    private readonly IInventoryRepository _repository;
    private readonly ILogger<InventorySyncJob> _logger;

    public InventorySyncJob(
        ICobolInterop cobolInterop,
        IInventoryRepository repository,
        ILogger<InventorySyncJob> logger)
    {
        _cobolInterop = cobolInterop;
        _repository = repository;
        _logger = logger;
    }

    [AutomaticRetry(Attempts = 3)]
    public async Task SyncInventoryFromCobol()
    {
        _logger.LogInformation("Starting inventory sync from COBOL");

        try
        {
            // Call COBOL batch job to export inventory
            var result = await _cobolInterop.ExportInventoryAsync();

            if (!result.IsSuccess)
            {
                _logger.LogError("COBOL inventory export failed: {Error}", result.Error);
                throw new InvalidOperationException(result.Error);
            }

            // Import data into modern database
            await _repository.BulkImportAsync(result.Value);

            _logger.LogInformation("Inventory sync completed successfully. {Count} items synced", result.Value.Count);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Inventory sync failed");
            throw; // Hangfire will retry
        }
    }
}

// Register recurring job
RecurringJob.AddOrUpdate<InventorySyncJob>(
    "inventory-sync",
    job => job.SyncInventoryFromCobol(),
    Cron.Hourly);
```

## Testing Strategies

### Unit Tests with xUnit and Moq

```csharp
public class CustomerServiceTests
{
    private readonly Mock<ICustomerRepository> _mockRepository;
    private readonly Mock<ICobolInterop> _mockCobolInterop;
    private readonly Mock<ILogger<CustomerService>> _mockLogger;
    private readonly CustomerService _sut; // System Under Test

    public CustomerServiceTests()
    {
        _mockRepository = new Mock<ICustomerRepository>();
        _mockCobolInterop = new Mock<ICobolInterop>();
        _mockLogger = new Mock<ILogger<CustomerService>>();
        _sut = new CustomerService(_mockRepository.Object, _mockCobolInterop.Object, _mockLogger.Object);
    }

    [Fact]
    public async Task GetCustomerAsync_ExistingCustomer_ReturnsSuccess()
    {
        // Arrange
        var customerId = 123;
        var expectedCustomer = new Customer { Id = customerId, Name = "Test Customer" };
        _mockRepository.Setup(r => r.GetByIdAsync(customerId, It.IsAny<CancellationToken>()))
            .ReturnsAsync(expectedCustomer);

        // Act
        var result = await _sut.GetCustomerAsync(customerId);

        // Assert
        Assert.True(result.IsSuccess);
        Assert.Equal(expectedCustomer.Name, result.Value.Name);
        _mockRepository.Verify(r => r.GetByIdAsync(customerId, It.IsAny<CancellationToken>()), Times.Once);
    }

    [Fact]
    public async Task UpdateCustomerAsync_ValidData_CallsCobolInterop()
    {
        // Arrange
        var request = new UpdateCustomerRequest { Id = 123, Name = "Updated Name" };
        _mockCobolInterop.Setup(c => c.UpdateCustomerAsync(It.IsAny<Customer>()))
            .ReturnsAsync(Result.Success());

        // Act
        var result = await _sut.UpdateCustomerAsync(request);

        // Assert
        Assert.True(result.IsSuccess);
        _mockCobolInterop.Verify(c => c.UpdateCustomerAsync(
            It.Is<Customer>(cust => cust.Name == "Updated Name")), Times.Once);
    }
}
```

### Integration Tests

```csharp
public class CustomerApiIntegrationTests : IClassFixture<WebApplicationFactory<Program>>
{
    private readonly WebApplicationFactory<Program> _factory;
    private readonly HttpClient _client;

    public CustomerApiIntegrationTests(WebApplicationFactory<Program> factory)
    {
        _factory = factory;
        _client = factory.CreateClient();
    }

    [Fact]
    public async Task GetCustomer_ReturnsCustomerData()
    {
        // Act
        var response = await _client.GetAsync("/api/customers/1");

        // Assert
        response.EnsureSuccessStatusCode();
        var content = await response.Content.ReadAsStringAsync();
        var customer = JsonSerializer.Deserialize<CustomerDto>(content);
        Assert.NotNull(customer);
        Assert.Equal(1, customer.Id);
    }
}
```

## AI Code Generation Instructions

When generating C# integration code:

1. Use latest C# features (records, pattern matching, null-coalescing)
2. Implement async/await for all I/O operations
3. Use dependency injection for all dependencies
4. Apply Result pattern for error handling (avoid throwing exceptions for business errors)
5. Include comprehensive XML documentation comments
6. Add appropriate logging statements
7. Implement IDisposable when managing unmanaged resources
8. Use CancellationToken for long-running operations
9. Follow repository pattern for data access
10. Include unit tests for business logic
11. Use proper data type mapping for COBOL interop
12. Implement retry logic for transient failures
13. Add performance monitoring and metrics
14. Follow SOLID principles
15. Use meaningful variable names and avoid abbreviations

## Code Review Checklist

Before accepting AI-generated C# code:

- [ ] Uses C# 10+ features appropriately
- [ ] Async/await used for I/O operations
- [ ] Dependency injection properly configured
- [ ] Result pattern used for error handling
- [ ] XML documentation comments present
- [ ] Logging added at appropriate levels
- [ ] Unit tests included with good coverage
- [ ] COBOL data type mapping correct
- [ ] IDisposable implemented where needed
- [ ] CancellationToken support for async methods
- [ ] No hard-coded configuration values
- [ ] Follows naming conventions (PascalCase for public, \_camelCase for private)
- [ ] SOLID principles applied
- [ ] Performance considerations addressed
- [ ] Security best practices followed

## References

- Microsoft C# Programming Guide
- ASP.NET Core Documentation
- Entity Framework Core Documentation
- MediatR Documentation (CQRS)
- cobol-standards.md (COBOL interop requirements)
- vbnet-interop.md (VB.NET integration patterns)
- database-patterns.md (Database access standards)
