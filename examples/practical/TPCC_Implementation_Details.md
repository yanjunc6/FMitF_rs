# TPC-C Implementation Details in `StoredProcedureClient.java`

This document provides a complete and detailed breakdown of the TPC-C benchmark transactions as implemented in the `StoredProcedureClient.java` file. The schemas and logic are derived directly from the Java code's interactions with the database service.

## Table Schemas

The following table schemas are inferred from the `read`, `write`, and `insert` operations within the transaction methods.

### `warehouse` table
*   **Primary Key**: `w_id`
*   **Fields**:
    *   `w_id`: Warehouse ID (Identifier)
    *   `w_ytd`: Warehouse Year-To-Date balance (Numeric)

### `district` table
*   **Primary Key**: `d_w_id, d_id` (Composite Key)
*   **Fields**:
    *   `d_w_id`: Warehouse ID (Foreign Key to `warehouse.w_id`)
    *   `d_id`: District ID (Identifier)
    *   `d_ytd`: District Year-To-Date balance (Numeric)
    *   `d_next_o_id`: Next available Order ID for this district (Numeric)

### `customer` table
*   **Primary Key**: `c_w_id, c_d_id, c_id` (Composite Key)
*   **Fields**:
    *   `c_w_id`: Customer's Warehouse ID (Foreign Key to `warehouse.w_id`)
    *   `c_d_id`: Customer's District ID (Foreign Key to `district.d_id`)
    *   `c_id`: Customer ID (Identifier)
    *   `c_balance`: Customer's balance (Numeric)
    *   `c_ytd_payment`: Customer's Year-To-Date payment total (Numeric)
    *   `c_payment_cnt`: Customer's total number of payments (Numeric)
    *   `c_credit`: Customer's credit status (e.g., "BC" for bad credit) (Text)
    *   `c_data`: Miscellaneous customer data (Text)

### `oorder` table (Represents an Order)
*   **Primary Key**: `o_w_id, o_d_id, o_id` (Composite Key, where `o_id` is `d_next_o_id`)
*   **Fields**:
    *   `o_w_id`: Order's Warehouse ID
    *   `o_d_id`: Order's District ID
    *   `o_id`: Order ID
    *   `o_c_id`: Customer ID (Foreign Key to `customer.c_id`)
    *   `o_carrier_id`: Carrier ID (can be null)
    *   `o_ol_cnt`: Order line count (Numeric)
    *   `o_all_local`: Flag indicating if all items are from the local warehouse (Numeric)
    *   `o_entry_d`: Order entry date/timestamp (Timestamp)

### `new_order` table
*   **Primary Key**: `no_w_id, no_d_id, no_o_id` (Composite Key)
*   **Fields**:
    *   `no_w_id`: New Order's Warehouse ID
    *   `no_d_id`: New Order's District ID
    *   `no_o_id`: New Order's Order ID
    *   *(This table acts as an index for new orders; the implementation inserts with no other column data.)*

### `item` table
*   **Primary Key**: `i_id`
*   **Fields**:
    *   `i_id`: Item ID (Identifier)
    *   `i_price`: Price of the item (Numeric)

### `stock` table
*   **Primary Key**: `s_w_id, s_i_id` (Composite Key)
*   **Fields**:
    *   `s_w_id`: Stock's Warehouse ID (Foreign Key to `warehouse.w_id`)
    *   `s_i_id`: Stock's Item ID (Foreign Key to `item.i_id`)
    *   `s_quantity`: Quantity of the item in stock (Numeric)
    *   `s_ytd`: Year-To-Date sales quantity for the item (Numeric)
    *   `s_order_cnt`: Total number of times the item has been ordered (Numeric)
    *   `s_remote_cnt`: Total number of times the item has been ordered from a remote warehouse (Numeric)
    *   `s_dist_01`: District 01 specific information (Text)
    *   `s_dist_02`: District 02 specific information (Text)
    *   `s_dist_03`: District 03 specific information (Text)
    *   `s_dist_04`: District 04 specific information (Text)
    *   `s_dist_05`: District 05 specific information (Text)
    *   `s_dist_06`: District 06 specific information (Text)
    *   `s_dist_07`: District 07 specific information (Text)
    *   `s_dist_08`: District 08 specific information (Text)
    *   `s_dist_09`: District 09 specific information (Text)
    *   `s_dist_10`: District 10 specific information (Text)

### `order_line` table
*   **Primary Key**: `ol_w_id, ol_d_id, ol_o_id, ol_number` (Composite Key)
*   **Fields**:
    *   `ol_w_id`: Order Line's Warehouse ID
    *   `ol_d_id`: Order Line's District ID
    *   `ol_o_id`: Order Line's Order ID (Foreign Key to `oorder.o_id`)
    *   `ol_number`: The line number of the item in the order (Identifier)
    *   `ol_i_id`: Item ID (Foreign Key to `item.i_id`)
    *   `ol_delivery_d`: Delivery date/timestamp (Timestamp)
    *   `ol_amount`: Total price for this line item (`ol_quantity` * `i_price`) (Numeric)
    *   `ol_supply_w_id`: Supplying warehouse ID (Foreign Key to `warehouse.w_id`)
    *   `ol_quantity`: Quantity of the item ordered (Numeric)
    *   `ol_dist_info`: District-specific information (copied from `stock.s_dist_xx`) (Text)

### `history` table
*   **Primary Key**: `h_c_id, h_c_d_id, h_c_w_id, h_d_id, h_w_id` (Composite Key)
*   **Fields**:
    *   `h_c_id`: Customer ID
    *   `h_c_d_id`: Customer's District ID
    *   `h_c_w_id`: Customer's Warehouse ID
    *   `h_d_id`: District ID
    *   `h_w_id`: Warehouse ID
    *   `h_date`: Payment date/timestamp (Timestamp)
    *   `h_amount`: Payment amount (Numeric)
    *   `h_data`: Miscellaneous data (Text)

---

## Transaction Logic Details

### `TPCC_newOrderWW` Transaction

This transaction processes a new customer order.

1.  **Start Transaction**: A new database transaction is initiated.
2.  **Read Customer**:
    *   Acquires a `READ` lock on the `customer` table.
    *   Reads the customer record using the composite key `c_w_id, c_d_id, c_id`.
    *   Checks if the customer exists; aborts on failure.
3.  **Read Warehouse**:
    *   Acquires a `READ` lock on the `warehouse` table.
    *   Reads the warehouse record using the key `w_id`.
    *   Checks if the warehouse exists; aborts on failure.
4.  **Read District**:
    *   Acquires a `READ` lock on the `district` table.
    *   Reads the district record using the composite key `d_w_id, d_id`.
    *   Retrieves the current `d_next_o_id`.
    *   Checks if the district exists; aborts on failure.
5.  **Update District**:
    *   Acquires a `WRITE` lock on the `district` table for the same record.
    *   Increments the retrieved `d_next_o_id` by 1.
    *   Writes the new `d_next_o_id` back to the district record.
6.  **Insert Order**:
    *   Acquires an `INSERT` lock for the `oorder` table with the new order ID.
    *   Inserts a new record into the `oorder` table with the following fields: `o_c_id`, `o_carrier_id` (as null), `o_ol_cnt`, `o_all_local`, and `o_entry_d` (current timestamp).
7.  **Insert New Order**:
    *   Acquires an `INSERT` lock for the `new_order` table with the new order ID.
    *   Inserts a new record into the `new_order` table to mark it as a new, unprocessed order.
8.  **Process Order Lines** (Loop from 1 to `orderLineCount`):
    a. **Read Item**:
        *   Acquires a `READ` lock on the `item` table using `i_id`.
        *   Reads the item record to get `i_price`.
    b. **Calculate Line Amount**:
        *   `ol_amount` = `ol_quantity` * `i_price`.
    c. **Read Stock**:
        *   Acquires a `READ` lock on the `stock` table using the composite key `s_w_id, s_i_id`.
        *   Reads the stock record to get `s_quantity`, `s_ytd`, `s_order_cnt`, `s_remote_cnt`, and the relevant `s_dist_xx` field.
    d. **Calculate New Stock Quantity**:
        *   If `s_quantity - ol_quantity >= 10`, then `newStockQuantity = s_quantity - ol_quantity`.
        *   Otherwise, `newStockQuantity = s_quantity - ol_quantity + 91`.
    e. **Insert Order Line**:
        *   Acquires an `INSERT` lock for the `order_line` table.
        *   Inserts a new record with: `ol_i_id`, `ol_delivery_d` (as current timestamp), `ol_amount`, `ol_supply_w_id`, `ol_quantity`, and `ol_dist_info`.
    f. **Update Stock**:
        *   Acquires a `WRITE` lock on the `stock` table for the same record.
        *   Updates the following fields:
            *   `s_quantity` = `newStockQuantity`.
            *   `s_ytd` = `s_ytd` + `ol_quantity`.
            *   `s_order_cnt` = `s_order_cnt` + 1.
            *   `s_remote_cnt` is incremented by 1 if the item is from a remote warehouse.
9.  **Commit Transaction**: If all steps succeed, the transaction is committed. Otherwise, it is rolled back.

### `TPCC_paymentWW` Transaction

This transaction processes a customer's payment.

1.  **Start Transaction**: A new database transaction is initiated.
2.  **Read Warehouse**:
    *   Acquires a `READ` lock on the `warehouse` table using `w_id`.
    *   Reads the warehouse record to get `w_ytd`.
3.  **Update Warehouse**:
    *   Acquires a `WRITE` lock on the same `warehouse` record.
    *   Calculates `newWarehouseBalance` = `w_ytd` + `paymentAmount`.
    *   Writes the new balance back to the `w_ytd` field.
4.  **Read District**:
    *   Acquires a `READ` lock on the `district` table using `d_w_id, d_id`.
    *   Reads the district record to get `d_ytd`.
5.  **Update District**:
    *   Acquires a `WRITE` lock on the same `district` record.
    *   Calculates `newDistrictBalance` = `d_ytd` + `paymentAmount`.
    *   Writes the new balance back to the `d_ytd` field.
6.  **Read Customer**:
    *   Acquires a `READ` lock on the `customer` table using `c_w_id, c_d_id, c_id`.
    *   Reads the customer record to get `c_balance`, `c_ytd_payment`, `c_payment_cnt`, and `c_credit`.
7.  **Update Customer**:
    *   Acquires a `WRITE` lock on the same `customer` record.
    *   Updates the following fields:
        *   `c_balance` = `c_balance` - `paymentAmount`.
        *   `c_ytd_payment` = `c_ytd_payment` + `paymentAmount`.
        *   `c_payment_cnt` = `c_payment_cnt` + 1.
    *   If `c_credit` is "BC" (bad credit), a new `c_data` string is constructed by prepending payment info to the existing `c_data` (truncated to 500 characters).
8.  **Insert History**:
    *   Inserts a new record into the `history` table.
    *   The record contains the customer and warehouse/district IDs, the current timestamp, the payment amount, and constructed history data.
    *   *(Note: The implementation states no lock is needed for this insert.)*
9.  **Commit Transaction**: If all steps succeed, the transaction is committed. Otherwise, it is rolled back.
