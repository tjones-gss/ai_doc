# Manufacturing ERP Domain Knowledge

## Overview

This rule file defines domain-specific business rules, terminology, and patterns for our manufacturing ERP system. AI code generation must respect these business rules and use this terminology correctly.

## Business Domain: Manufacturing Resource Planning

### Core Concepts

#### Manufacturing Order (MO)

- **Definition**: Work order for producing finished goods from raw materials
- **Lifecycle**: Created → Released → In Progress → Completed → Closed
- **Key Fields**:
  - MO Number (unique identifier)
  - Item Number (what to produce)
  - Quantity Ordered
  - Quantity Completed
  - Start Date / Due Date / Complete Date
  - Work Center Assignment
  - Status Code

#### Bill of Materials (BOM)

- **Definition**: Recipe/formula defining components needed to build a product
- **Types**:
  - **Single-level BOM**: Immediate components only
  - **Multi-level BOM**: Full explosion to raw materials
  - **Phantom BOM**: Components consumed without inventory transaction
- **Key Fields**:
  - Parent Item
  - Component Item
  - Quantity Per (how much needed per parent)
  - Scrap Factor (percentage)
  - Operation Sequence
  - Effective Date / Obsolete Date

#### Work Center

- **Definition**: Production location/machine where operations are performed
- **Capacity Planning**: Hours available per shift, utilization rate
- **Key Fields**:
  - Work Center Code
  - Description
  - Department
  - Capacity (hours/day)
  - Labor Rate
  - Overhead Rate
  - Queue Time / Setup Time / Run Time

#### Routing

- **Definition**: Sequence of operations to manufacture an item
- **Components**:
  - Operation Number (sequence)
  - Work Center
  - Setup Time / Run Time per unit
  - Move Time / Queue Time
  - Labor Hours / Machine Hours
- **Relationship**: One routing per item, multiple operations per routing

## Inventory Management

### Inventory Transaction Types

```
Type Code | Description              | Debit/Credit | Common Usage
----------|--------------------------|--------------|------------------
ISS       | Issue to Manufacturing   | Credit       | Material consumption
RCT       | Receipt from MO          | Debit        | Finished goods
ADJ       | Inventory Adjustment     | Debit/Credit | Cycle count correction
TRN       | Transfer between sites   | Both         | Warehouse moves
PUR       | Purchase Receipt         | Debit        | Receiving dock
SHP       | Sales Shipment          | Credit       | Customer order fulfillment
RET       | Customer Return         | Debit        | RMA processing
SCR       | Scrap/Waste             | Credit       | Quality failure
```

### Lot/Serial Number Tracking

- **Lot-Tracked Items**: Batch number for traceability (raw materials, chemicals)
- **Serial-Tracked Items**: Unique ID per unit (machines, electronics)
- **Traceability Rules**:
  - Forward trace: Which lots went into which finished goods
  - Backward trace: Which raw material lots were consumed
  - Required for FDA/ISO compliance

### Inventory Valuation Methods

- **Standard Cost**: Pre-determined cost, variance tracked
- **Average Cost**: Weighted average of purchases
- **FIFO**: First In, First Out (not implemented in legacy COBOL)
- **Current Method**: Standard cost (set in COBOL program STDCOST)

## Production Scheduling

### Master Production Schedule (MPS)

- **Time Fence**: Period when MPS is frozen (typically 4 weeks)
- **Planning Horizon**: How far out to plan (typically 12-26 weeks)
- **Demand Sources**:
  - Customer Orders (firm)
  - Forecast (soft)
  - Safety Stock Requirements

### Material Requirements Planning (MRP)

- **Logic**:
  1. Gross Requirements (from MPS and lower-level MOs)
  2. Subtract On-Hand Inventory
  3. Add Scheduled Receipts (open POs/MOs)
  4. Net Requirements
  5. Apply Lot Sizing Rules
  6. Planned Orders
- **Lot Sizing Methods**:
  - Lot-for-Lot (make exactly what's needed)
  - Fixed Order Quantity
  - Economic Order Quantity (EOQ)
  - Period Order Quantity

### Capacity Requirements Planning (CRP)

- **Infinite Loading**: Schedule without capacity constraints
- **Finite Loading**: Respect work center capacity limits
- **Load Leveling**: Balance work across time periods
- **Current Implementation**: Infinite loading in legacy COBOL

## Quality Management

### Inspection Points

- **Receiving Inspection**: Purchased materials (program RCVINSP)
- **In-Process Inspection**: During manufacturing
- **Final Inspection**: Before shipping to customer
- **Sampling Plans**: AQL (Acceptable Quality Level) based

### Non-Conformance Handling

- **NCR**: Non-Conformance Report
- **Disposition Options**:
  - Use As-Is (engineering waiver)
  - Rework (return to production)
  - Scrap (destroy/recycle)
  - Return to Vendor (RTV)
- **Root Cause Analysis**: Required for major defects

### Statistical Process Control (SPC)

- **Control Charts**: Monitor process variation
- **Cp/Cpk**: Process capability indices
- **Out-of-Control Conditions**: Trigger investigation

## Order Management

### Sales Order Processing

- **Order Types**:
  - Stock Order: Ship from inventory
  - Make-to-Order: Trigger MO for specific customer
  - Configure-to-Order: Build variant per specs
  - Engineering-to-Order: Custom design
- **Order Hold Reasons**:
  - Credit Limit Exceeded
  - Payment Past Due
  - Quality Hold
  - Engineering Approval Required
- **Allocation Rules**:
  - FIFO by order date
  - Priority customers first
  - Partial allocation allowed/not allowed

### Pricing and Discounts

- **Price Levels**:
  - List Price (base)
  - Customer-Specific Pricing
  - Quantity Break Pricing
  - Promotional Pricing (date range)
- **Discount Hierarchy**:
  1. Line Item Discount
  2. Customer Discount
  3. Volume Discount
  4. Payment Terms Discount (2/10 Net 30)

## Purchasing

### Purchase Order Lifecycle

```
Created → Approved → Sent to Vendor → Acknowledged →
Received (partial/complete) → Invoiced → Paid → Closed
```

### Vendor Management

- **Vendor Rating**: On-time delivery, quality, price competitiveness
- **Approved Vendor List (AVL)**: By item or commodity
- **Blanket POs**: Annual agreement with releases
- **Drop Ship**: Vendor ships directly to customer

### Receiving Process

- **Three-Way Match**:
  1. PO (quantity, price)
  2. Packing Slip/Receiver
  3. Vendor Invoice
- **Variance Tolerance**: ±5% quantity, ±2% price (configurable)
- **Rejection Process**: Return to vendor, replacement/credit

## Cost Accounting

### Standard Cost Components

```
Material Cost    = Sum of (Component Std Cost × Qty Per) × (1 + Scrap%)
Labor Cost       = Sum of (Operation Labor Hours × Labor Rate)
Overhead Cost    = Sum of (Operation Machine Hours × Overhead Rate)
────────────────────────────────────────────────────────────────────
Total Std Cost   = Material + Labor + Overhead + Outside Processing
```

### Variance Tracking

- **Material Variance**: (Actual Cost - Standard Cost) × Qty
- **Labor Variance**: (Actual Hours - Standard Hours) × Rate
- **Overhead Variance**: Applied vs. Actual overhead
- **Scrap Variance**: Excess scrap beyond standard factor

### Month-End Close Process

1. Complete all pending inventory transactions
2. Run MRP/CRP
3. Post work-in-process (WIP) to GL
4. Calculate variances
5. Close manufacturing orders
6. Update standard costs (if annual revaluation period)
7. Generate financial statements

## Regulatory Compliance

### Traceability Requirements

- **FDA 21 CFR Part 11**: Electronic records and signatures
- **ISO 9001**: Quality management system
- **AS9100**: Aerospace quality (if applicable)
- **ITAR/EAR**: Export control (defense products)

### Audit Trail Requirements

- **Who**: User ID who made change
- **What**: Old value → New value
- **When**: Timestamp with time zone
- **Where**: Program/module name
- **Why**: Optional reason code or notes
- **Retention**: 7 years minimum

### Document Control

- **Revision Control**: Engineering drawings, BOMs, routers
- **Change Orders**: ECO (Engineering Change Order)
- **Approval Workflow**: Design → Engineering → Quality → Production
- **Effectivity Date**: When change takes effect

## Key Business Rules

### Credit Management

```cobol
IF CUSTOMER-ORDER-TOTAL > CUSTOMER-CREDIT-LIMIT THEN
    IF CUSTOMER-PAST-DUE-BALANCE > 0 THEN
        SET ORDER-HOLD-FLAG TO 'CREDIT-EXCEEDED'
        NOTIFY CREDIT-MANAGER
    ELSE
        IF (CUSTOMER-ORDER-TOTAL - CUSTOMER-CREDIT-LIMIT) < 10000 THEN
            SET ORDER-HOLD-FLAG TO 'CREDIT-REVIEW'
            NOTIFY CREDIT-MANAGER
        ELSE
            SET ORDER-HOLD-FLAG TO 'CREDIT-EXCEEDED'
            BLOCK-ORDER-PROCESSING
        END-IF
    END-IF
END-IF.
```

### Inventory Allocation Priority

1. Customer orders with promised ship dates in the past (late orders)
2. Customer orders with promised ship dates within 7 days
3. Make-to-stock replenishment
4. Customer orders beyond 7 days (by promised date)

### Manufacturing Order Release Rules

- **Material Availability**: All components available or on order with lead time
- **Capacity Check**: Work center capacity available in required timeframe
- **Tooling/Fixtures**: Required tools available
- **Engineering Approval**: BOM and Routing approved (not in ECO)

### Lot Sizing Constraints

- **Minimum Order Quantity**: Don't make less than MOQ
- **Maximum Order Quantity**: Don't exceed equipment capacity
- **Order Multiples**: Round up to full containers/pallets
- **Shelf Life**: Don't make more than X days' supply for perishables

## Common Calculations

### Lead Time Calculation

```
Manufacturing Lead Time =
    Queue Time (waiting for work center) +
    Setup Time (prepare machine/tools) +
    Run Time (actual production: Qty × Run Time Per Unit) +
    Move Time (transport to next operation) +
    Inspection Time (if quality check required)
```

### Available-to-Promise (ATP)

```
ATP = On-Hand Qty + Scheduled Receipts - Customer Orders - Safety Stock
```

### Reorder Point (ROP)

```
ROP = (Average Daily Demand × Lead Time Days) + Safety Stock
```

### Economic Order Quantity (EOQ)

```
EOQ = SQRT((2 × Annual Demand × Ordering Cost) / Holding Cost per Unit)
```

## System Integration Points

### Interfaces with External Systems

- **Accounting System (GL)**: Journal entries for inventory transactions, WIP, variances
- **CAD/PLM**: Engineering drawings, BOM data
- **Shop Floor Data Collection**: Actual labor hours, quantities produced
- **Shipping System**: Shipment notifications, tracking numbers
- **CRM**: Customer order entry, quotes
- **Supplier Portal**: PO transmission, ASN (Advanced Ship Notice)

## Data Validation Rules

### Item Master Validation

- Item Number: Alphanumeric, 1-20 characters, must be unique
- Description: Required, 1-50 characters
- Unit of Measure: Must exist in UOM table
- Item Type: Raw Material, WIP, Finished Good, Service
- Lot/Serial Tracked: If yes, lot/serial required on all transactions
- Lead Time: Positive integer, days
- Standard Cost: Positive decimal, 2 decimals

### Customer Order Validation

- Customer ID: Must exist in customer master
- Ship-To Address: Required
- Payment Terms: Must exist in terms table
- Item Numbers: Must exist and be sellable (not obsolete)
- Quantities: Positive, respect order multiples
- Prices: Cannot be less than cost (warning, not error)
- Ship Date: Cannot be in the past

### Purchase Order Validation

- Vendor ID: Must exist in vendor master, status = Active
- Item Numbers: Must exist and be purchasable
- Unit Price: Cannot exceed ±20% of last purchase price (warning)
- Required Date: Must be >= today + vendor lead time
- Buyer: Must be valid user with purchasing authority

## Terminology Standards

### Use These Terms (Not These)

- Manufacturing Order (not "work order", "job", "production order")
- Bill of Materials (not "BOM list", "recipe", "formula")
- Customer (not "client", "account")
- Vendor (not "supplier" in COBOL programs, though "supplier" OK in C#)
- Item (not "part", "SKU", "product" unless specifically finished goods)
- Quantity On Hand (not "inventory", "stock quantity")
- Work Center (not "machine", "station")
- Operation (not "step", "process")
- Component (not "ingredient", "material" unless specifically raw material)

### Status Codes

```
Status | Meaning                  | Valid Transitions
-------|--------------------------|------------------
N      | New/Planned              | → A (Approved)
A      | Approved                 | → R (Released), → X (Cancelled)
R      | Released to Shop Floor   | → I (In Progress)
I      | In Progress              | → C (Completed), → H (On Hold)
H      | On Hold                  | → I (Resume), → X (Cancel)
C      | Completed                | → L (Closed)
L      | Closed                   | (final state)
X      | Cancelled                | (final state)
```

## AI Code Generation Guidance

### When Writing Manufacturing Code:

1. **Always validate business rules** before database updates
2. **Maintain audit trails** for all data changes
3. **Respect transaction boundaries** - use database transactions for multi-step operations
4. **Check inventory availability** before allocating or consuming
5. **Enforce lot/serial tracking** per item master settings
6. **Calculate costs correctly** using standard cost components
7. **Use correct terminology** from this document
8. **Handle partial transactions** (partial receipts, partial shipments)
9. **Implement proper holds and releases** for orders, inventory, etc.
10. **Generate appropriate events** for downstream systems (MRP, accounting, etc.)

### Domain-Specific Error Messages

```
ERR-4001: Insufficient inventory to allocate to order
ERR-4002: Item is on quality hold - cannot ship
ERR-4003: Customer credit limit exceeded - order on hold
ERR-4004: Manufacturing order cannot be released - missing components
ERR-4005: BOM explosion failed - circular reference detected
ERR-4006: Invalid operation sequence in routing
ERR-4007: Work center over capacity - cannot schedule
ERR-4008: Lot number required for this item
ERR-4009: Item is obsolete - cannot create new orders
ERR-4010: Price below cost - requires management approval
```

### Testing Scenarios to Consider

- **Happy path**: Normal order-to-cash or procure-to-pay flow
- **Inventory shortage**: Not enough to fulfill order
- **Credit hold**: Customer over limit
- **Partial receipt**: Vendor ships less than PO quantity
- **Lot traceability**: Forward and backward trace
- **Month-end close**: All transactions posted, variances calculated
- **BOM changes**: Effectivity dates, pending ECOs
- **Backorder processing**: Allocate newly received inventory
- **Returns processing**: Customer returns, vendor returns
- **Scrap reporting**: In-process scrap, receiving rejection

## References

- Manufacturing Business Process Documentation (internal wiki)
- Standard Operating Procedures (SOPs)
- Quality Management System (QMS) Manual
- Accounting Policies Manual
- database-patterns.md (database schema details)
- cobol-standards.md (COBOL implementation patterns)
