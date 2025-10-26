# VB.NET Interoperability Standards for Manufacturing ERP

## Overview

This rule file defines standards for VB.NET code that interoperates with COBOL programs and modern .NET components in our manufacturing ERP system.

## VB.NET Version & Framework

- **Target Framework**: .NET Framework 4.8 or .NET 6+ (specify in project)
- **Language Version**: VB.NET 16.0+
- **IDE**: Visual Studio 2019/2022

## Calling COBOL from VB.NET

### Basic COBOL Program Call

```vb.net
Imports System.Runtime.InteropServices

Public Class CobolInterop
    ' Declare the COBOL program as an external function
    <DllImport("CUSTMAIN.dll", CallingConvention:=CallingConvention.StdCall)>
    Private Shared Function CUSTMAIN(
        ByRef customerId As Integer,
        ByVal customerName As String,
        ByRef creditLimit As Decimal,
        <Out> ByRef returnCode As Integer
    ) As Integer
    End Function

    Public Function ProcessCustomer(custId As Integer) As Boolean
        Dim custName As String = Space(30) ' Pre-allocate for COBOL
        Dim credit As Decimal
        Dim retCode As Integer

        Try
            Dim result As Integer = CUSTMAIN(custId, custName, credit, retCode)

            If retCode = 0 Then
                ' Success - process results
                Console.WriteLine($"Customer: {custName.Trim()}")
                Console.WriteLine($"Credit Limit: {credit:C}")
                Return True
            Else
                ' Handle COBOL error
                LogError($"COBOL returned error code: {retCode}")
                Return False
            End If
        Catch ex As Exception
            LogError($"Error calling COBOL: {ex.Message}")
            Return False
        End Try
    End Function
End Class
```

### NetCOBOL for .NET Integration

```vb.net
' For Fujitsu NetCOBOL compiled as .NET assemblies
Imports FujitsuCobolAssembly

Public Class NetCobolIntegration
    Public Sub CallCobolProgram()
        ' Create instance of COBOL class
        Dim cobolProg As New CUSTMAIN()

        ' Set parameters
        cobolProg.CUSTOMER_ID = 12345
        cobolProg.CUSTOMER_NAME = "ACME Manufacturing"

        ' Execute the COBOL program
        cobolProg.Execute()

        ' Get results
        Dim creditLimit As Decimal = cobolProg.CREDIT_LIMIT
        Dim status As String = cobolProg.RETURN_CODE
    End Sub
End Class
```

## Data Type Mapping

### COBOL to VB.NET Type Conversions

| COBOL Type           | VB.NET Type           | Notes                         |
| -------------------- | --------------------- | ----------------------------- |
| `PIC 9(4) COMP`      | `Short` (Int16)       | 2-byte integer                |
| `PIC 9(9) COMP`      | `Integer` (Int32)     | 4-byte integer                |
| `PIC 9(18) COMP`     | `Long` (Int64)        | 8-byte integer                |
| `PIC X(n)`           | `String`              | Fixed length, pad with spaces |
| `PIC 9(n)V99 COMP-3` | `Decimal`             | Packed decimal                |
| `PIC 9(n)V99`        | `Decimal` or `String` | Display numeric               |
| `PIC X`              | `Char` or `String`    | Single character              |

### String Handling

```vb.net
' COBOL expects fixed-length strings
Public Function PadForCobol(value As String, length As Integer) As String
    If value Is Nothing Then value = ""
    Return value.PadRight(length).Substring(0, length)
End Function

' Clean COBOL strings (remove trailing spaces)
Public Function TrimCobolString(value As String) As String
    Return value?.TrimEnd() ?? ""
End Function

' Example usage
Dim cobolName As String = PadForCobol(customerName, 30)
Dim displayName As String = TrimCobolString(resultName)
```

### Decimal and Numeric Conversion

```vb.net
' Convert VB.NET Decimal to COBOL COMP-3 compatible format
Public Function ToCobolDecimal(value As Decimal) As Decimal
    ' Round to 2 decimal places for COMP-3 compatibility
    Return Math.Round(value, 2, MidpointRounding.AwayFromZero)
End Function

' Handle COBOL signed numerics
Public Function ParseCobolNumeric(cobolString As String) As Decimal?
    ' COBOL may use trailing sign notation
    If String.IsNullOrWhiteSpace(cobolString) Then Return Nothing

    Dim cleanValue As String = cobolString.Trim()
    Dim isNegative As Boolean = False

    ' Check for trailing sign (COBOL format)
    If cleanValue.EndsWith("-") Then
        isNegative = True
        cleanValue = cleanValue.Substring(0, cleanValue.Length - 1)
    End If

    Dim result As Decimal
    If Decimal.TryParse(cleanValue, result) Then
        Return If(isNegative, -result, result)
    End If

    Return Nothing
End Function
```

## COM Interoperability

### Calling COM Components from VB.NET

```vb.net
Imports System.Runtime.InteropServices

' Late binding for flexible COM interop
Public Class ComInterop
    Public Function CallLegacyComComponent(itemId As String) As Object
        Dim comType As Type = Type.GetTypeFromProgID("LegacyERP.InventoryManager")
        If comType Is Nothing Then
            Throw New COMException("COM component not registered")
        End If

        Dim comObject As Object = Activator.CreateInstance(comType)
        Try
            ' Call COM method via late binding
            Dim result = comObject.GetType().InvokeMember(
                "GetItemDetails",
                Reflection.BindingFlags.InvokeMethod,
                Nothing,
                comObject,
                New Object() {itemId}
            )
            Return result
        Finally
            ' Always release COM objects
            If comObject IsNot Nothing Then
                Marshal.ReleaseComObject(comObject)
            End If
        End Try
    End Function
End Class

' Early binding with COM reference
<ComImport>
<Guid("12345678-1234-1234-1234-123456789ABC")>
<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)>
Public Interface ILegacyInventory
    Function GetItemDetails(itemId As String) As InventoryItem
    Sub UpdateQuantity(itemId As String, quantity As Integer)
End Interface
```

## Error Handling and Logging

### Standard Error Handling Pattern

```vb.net
Public Class ErpInteropService
    Private ReadOnly _logger As ILogger

    Public Function ExecuteCobolTransaction(
        transactionData As TransactionData
    ) As Result(Of TransactionResult)
        Try
            ' Validate input
            Dim validation = ValidateTransactionData(transactionData)
            If Not validation.IsValid Then
                Return Result(Of TransactionResult).Failure(validation.Errors)
            End If

            ' Call COBOL
            Dim cobolResult = CallCobolProgram(transactionData)

            ' Check COBOL return code
            Select Case cobolResult.ReturnCode
                Case 0
                    _logger.LogInformation($"Transaction {transactionData.Id} completed successfully")
                    Return Result(Of TransactionResult).Success(cobolResult.Data)

                Case 100
                    _logger.LogWarning($"Transaction {transactionData.Id}: Record not found")
                    Return Result(Of TransactionResult).NotFound("Record not found")

                Case Else
                    _logger.LogError($"Transaction {transactionData.Id} failed with code {cobolResult.ReturnCode}")
                    Return Result(Of TransactionResult).Failure($"COBOL error: {cobolResult.ReturnCode}")
            End Select

        Catch ex As COMException
            _logger.LogError(ex, $"COM error in transaction {transactionData.Id}")
            Return Result(Of TransactionResult).Failure($"COM interop error: {ex.Message}")

        Catch ex As Exception
            _logger.LogError(ex, $"Unexpected error in transaction {transactionData.Id}")
            Return Result(Of TransactionResult).Failure($"System error: {ex.Message}")
        End Try
    End Function
End Class
```

## Database Access Patterns

### Shared Database Access with COBOL

```vb.net
Imports System.Data.SqlClient

Public Class SharedDatabaseAccess
    ' Use same connection string as COBOL programs
    Private Const ConnectionString As String = "Data Source=ERPSQL;Initial Catalog=Manufacturing;Integrated Security=True"

    Public Function GetInventoryItem(itemId As String) As InventoryItem
        Using conn As New SqlConnection(ConnectionString)
            conn.Open()

            ' Use same stored procedures as COBOL
            Using cmd As New SqlCommand("sp_GetInventoryItem", conn)
                cmd.CommandType = CommandType.StoredProcedure
                cmd.Parameters.AddWithValue("@ItemId", itemId)

                ' Use read committed isolation level (same as COBOL)
                Using trans = conn.BeginTransaction(IsolationLevel.ReadCommitted)
                    cmd.Transaction = trans
                    Try
                        Using reader = cmd.ExecuteReader()
                            If reader.Read() Then
                                Return MapInventoryItem(reader)
                            End If
                        End Using
                        trans.Commit()
                    Catch ex As Exception
                        trans.Rollback()
                        Throw
                    End Try
                End Using
            End Using
        End Using

        Return Nothing
    End Function

    Private Function MapInventoryItem(reader As SqlDataReader) As InventoryItem
        ' Handle COBOL-style data (fixed decimals, trimmed strings)
        Return New InventoryItem With {
            .ItemId = reader("ITEM_ID").ToString().Trim(),
            .Description = reader("DESCRIPTION").ToString().Trim(),
            .QuantityOnHand = Convert.ToDecimal(reader("QTY_ON_HAND")),
            .UnitCost = Math.Round(Convert.ToDecimal(reader("UNIT_COST")), 2),
            .LastUpdateDate = Convert.ToDateTime(reader("LAST_UPDATE_DATE"))
        }
    End Function
End Class
```

## Naming Conventions

### VB.NET Naming Standards

```vb.net
' Classes: PascalCase
Public Class CustomerService
Public Class InventoryManager

' Interfaces: IPascalCase
Public Interface ICustomerRepository
Public Interface IOrderProcessor

' Methods: PascalCase
Public Function GetCustomer() As Customer
Public Sub UpdateInventory()

' Properties: PascalCase
Public Property CustomerId As Integer
Public Property CustomerName As String

' Private fields: _camelCase
Private _repository As ICustomerRepository
Private _logger As ILogger

' Constants: PascalCase or UPPER_CASE
Public Const MaxRetries As Integer = 3
Private Const DEFAULT_CREDIT_LIMIT As Decimal = 5000D

' Parameters: camelCase
Public Function ProcessOrder(orderId As Integer, customerId As Integer) As Boolean

' Local variables: camelCase
Dim customerName As String
Dim orderTotal As Decimal
```

### File Organization

```
/LegacyInterop
    /CobolWrappers       ' COBOL program wrapper classes
    /ComInterop          ' COM component wrappers
    /DataMapping         ' Data type converters
/Services
    /Inventory           ' Business logic
    /Orders
    /Customers
/Models                  ' Data models shared with COBOL
/Utilities               ' Helper functions
```

## Configuration Management

### App.config / Web.config Integration

```xml
<configuration>
  <appSettings>
    <!-- COBOL DLL locations -->
    <add key="CobolDllPath" value="C:\ERP\Cobol\Bin" />

    <!-- COM component registration -->
    <add key="LegacyComProgId" value="LegacyERP.InventoryManager" />

    <!-- Shared database connection -->
    <add key="ErpConnectionString" value="Data Source=ERPSQL;..." />

    <!-- Business date for COBOL compatibility -->
    <add key="BusinessDate" value="2025-10-25" />
  </appSettings>
</configuration>
```

```vb.net
Public Class ConfigurationManager
    Public Shared Function GetCobolDllPath() As String
        Return ConfigurationManager.AppSettings("CobolDllPath")
    End Function

    Public Shared Function GetBusinessDate() As Date
        Dim dateString = ConfigurationManager.AppSettings("BusinessDate")
        Return Date.Parse(dateString)
    End Function
End Class
```

## Threading and Concurrency

### Thread-Safe COBOL Interop

```vb.net
Public Class ThreadSafeCobolWrapper
    ' COBOL programs may not be thread-safe
    Private Shared ReadOnly _cobolLock As New Object()

    Public Function CallCobolSafely(parameters As Object()) As Object
        ' Serialize COBOL calls to prevent threading issues
        SyncLock _cobolLock
            Return InvokeCobolProgram(parameters)
        End SyncLock
    End Function

    ' For async operations, use Task-based approach
    Public Async Function CallCobolAsync(parameters As Object()) As Task(Of Object)
        Return Await Task.Run(Function()
            SyncLock _cobolLock
                Return InvokeCobolProgram(parameters)
            End SyncLock
        End Function)
    End Function
End Class
```

## Memory Management

### Proper Resource Disposal

```vb.net
Public Class ResourceManagement
    Implements IDisposable

    Private _disposed As Boolean = False
    Private _comObject As Object

    Public Sub Dispose() Implements IDisposable.Dispose
        Dispose(True)
        GC.SuppressFinalize(Me)
    End Sub

    Protected Overridable Sub Dispose(disposing As Boolean)
        If Not _disposed Then
            If disposing Then
                ' Dispose managed resources
                If _comObject IsNot Nothing Then
                    Marshal.ReleaseComObject(_comObject)
                    _comObject = Nothing
                End If
            End If

            ' Free unmanaged resources here if any
            _disposed = True
        End If
    End Sub

    Protected Overrides Sub Finalize()
        Dispose(False)
    End Sub
End Class
```

## Testing Strategies

### Unit Testing COBOL Interop

```vb.net
<TestClass>
Public Class CobolInteropTests
    <TestMethod>
    Public Sub TestCustomerUpdate_ValidData_ReturnsSuccess()
        ' Arrange
        Dim wrapper As New CustomerCobolWrapper()
        Dim testCustomer As New Customer With {
            .CustomerId = 12345,
            .Name = "Test Customer",
            .CreditLimit = 50000D
        }

        ' Act
        Dim result = wrapper.UpdateCustomer(testCustomer)

        ' Assert
        Assert.IsTrue(result.IsSuccess)
        Assert.AreEqual(0, result.ReturnCode)
    End Sub

    <TestMethod>
    Public Sub TestInventoryUpdate_InsufficientQuantity_ReturnsError()
        ' Arrange
        Dim wrapper As New InventoryCobolWrapper()

        ' Act
        Dim result = wrapper.AllocateInventory("ITEM-001", 99999)

        ' Assert
        Assert.IsFalse(result.IsSuccess)
        Assert.AreEqual(400, result.ReturnCode) ' ERR-4XXX business logic error
    End Sub
End Class
```

### Integration Testing

```vb.net
<TestClass>
Public Class IntegrationTests
    <TestMethod>
    Public Sub TestEndToEndOrderProcessing()
        ' Arrange
        Dim orderService As New OrderService()
        Dim testOrder As New Order With {
            .CustomerId = 1001,
            .Items = New List(Of OrderItem) From {
                New OrderItem With {.ItemId = "ITEM-001", .Quantity = 10}
            }
        }

        ' Act - This tests VB.NET -> COBOL -> Database -> COBOL -> VB.NET
        Dim result = orderService.ProcessOrder(testOrder)

        ' Assert
        Assert.IsTrue(result.IsSuccess)
        Assert.IsNotNull(result.OrderNumber)

        ' Verify in database (same way COBOL would)
        Dim dbOrder = GetOrderFromDatabase(result.OrderNumber)
        Assert.IsNotNull(dbOrder)
    End Sub
End Class
```

## Performance Optimization

### Caching COBOL Results

```vb.net
Imports System.Runtime.Caching

Public Class CobolCacheWrapper
    Private Shared ReadOnly _cache As MemoryCache = MemoryCache.Default

    Public Function GetCustomerCached(customerId As Integer) As Customer
        Dim cacheKey As String = $"Customer_{customerId}"

        ' Check cache first
        Dim cached = TryCast(_cache.Get(cacheKey), Customer)
        If cached IsNot Nothing Then
            Return cached
        End If

        ' Call COBOL if not cached
        Dim customer = CallCobolGetCustomer(customerId)

        ' Cache for 5 minutes
        Dim policy As New CacheItemPolicy With {
            .AbsoluteExpiration = DateTimeOffset.Now.AddMinutes(5)
        }
        _cache.Set(cacheKey, customer, policy)

        Return customer
    End Function
End Class
```

## AI Code Generation Instructions

When generating VB.NET interop code:

1. Always include proper error handling with Try-Catch
2. Use proper data type conversions between COBOL and VB.NET
3. Implement IDisposable for COM interop
4. Add thread safety for COBOL calls
5. Include logging for all interop operations
6. Follow VB.NET naming conventions (PascalCase, \_camelCase)
7. Add XML documentation comments
8. Handle fixed-length COBOL strings properly (padding/trimming)
9. Use proper decimal rounding for COBOL compatibility
10. Include unit tests for interop methods

## Code Review Checklist

Before accepting AI-generated VB.NET interop code:

- [ ] Proper data type mapping between COBOL and VB.NET
- [ ] String padding/trimming for fixed-length COBOL strings
- [ ] Decimal rounding for COMP-3 compatibility
- [ ] Thread safety for COBOL program calls
- [ ] COM object disposal (ReleaseComObject)
- [ ] Error handling with specific return code checks
- [ ] Logging for interop operations
- [ ] XML documentation comments
- [ ] Unit tests included
- [ ] Integration test coverage
- [ ] Configuration externalized (no hard-coded paths)
- [ ] Resource cleanup (IDisposable pattern)

## References

- Fujitsu NetCOBOL for .NET Interoperability Guide
- Microsoft .NET Framework COM Interop Documentation
- cobol-standards.md (COBOL coding standards)
- csharp-integration.md (C# integration patterns)
