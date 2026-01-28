# Type Coercion

L4 provides built-in type coercion functions that are always available without importing any library. These are implemented in the compiler core for performance and consistency.

## Functions

### TOSTRING

Converts a value to its string representation.

**Signature:** `NUMBER | BOOLEAN | DATE | STRING → STRING`

**Behavior:**

- Numbers render with canonical decimal text (no trailing zeros)
- Booleans render as `TRUE` or `FALSE`
- Dates render as `YYYY-MM-DD`
- Strings pass through unchanged

**Example:**

[coercion-example.l4](coercion-example.l4)

---

### TONUMBER

Parses a string to a number.

**Signature:** `STRING → MAYBE NUMBER`

**Behavior:**

- Accepts trimmed ASCII numeric literals
- Supports optional sign (`+` or `-`)
- Supports decimal point
- Supports scientific notation (e.g., `1.2E3`)
- Returns `NOTHING` on parse failure

**Accepted formats:**

- `"42"` → `JUST 42`
- `"-12.50"` → `JUST -12.5`
- `"  3.14  "` → `JUST 3.14` (whitespace trimmed)
- `"1.2E3"` → `JUST 1200`
- `"oops"` → `NOTHING`

---

### TODATE

Parses a string to a date.

**Signature:** `STRING → MAYBE DATE`

**Accepted formats:**

- `YYYY-MM-DD` (ISO 8601): `"2024-02-29"`
- `YYYY/MM/DD`: `"2024/02/29"`
- `DD-MMM-YYYY`: `"29-Feb-2024"`
- `DD/MM/YYYY`: `"29/02/2024"`
- `MMM DD, YYYY`: `"Feb 29, 2024"`

**Behavior:**

- Month names are case-insensitive
- Rejects dates outside `0001-01-01` to `9999-12-31`
- Returns `NOTHING` for invalid dates (e.g., `"31/02/2024"`)

---

### TRUNC

Truncates a number toward zero.

**Signature:** `NUMBER, NUMBER → NUMBER`

**Parameters:**

- `value` - The number to truncate
- `digits` - Number of decimal places to keep (rounded to nearest integer)

**Behavior:**

- Non-negative `digits` truncate fractional places
- Negative `digits` truncate to the left of the decimal point
- Truncates toward zero (not floor/ceiling)

**Examples:**

- `TRUNC 12.987 0` → `12`
- `TRUNC 12.987 2` → `12.98`
- `TRUNC 123.45 (-1)` → `120`
- `TRUNC (-12.987) 1` → `-12.9`

---

### AS STRING

Inline syntax for type conversion, equivalent to `TOSTRING`.

**Syntax:** `value AS STRING`

**Examples:**

- `42 AS STRING` → `"42"`
- `TRUE AS STRING` → `"TRUE"`
- `(Date 1 1 2024) AS STRING` → `"2024-01-01"`

---

## Locale Independence

All coercion functions are **deterministic and locale-independent**. This ensures consistent behavior across different systems and environments.

---

## See Also

- **[Types](README.md)** - Type system documentation
- **[Functions](../functions/README.md)** - Function keywords
- **[Libraries](../libraries/README.md)** - Core libraries
