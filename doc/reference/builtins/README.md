# Built-in Functions (Not a Library)

L4 includes several **built-in functions** that are always available without importing any library. These are implemented in the compiler core.

> For operators (arithmetic, comparison, boolean), see also [Operators](../operators/README.md) for precedence and usage details.

### Type Coercion Builtins

| Function     | Signature                                    | Description                      |
| ------------ | -------------------------------------------- | -------------------------------- |
| `TOSTRING`   | `NUMBER/BOOLEAN/DATE/TIME/DATETIME → STRING` | Convert to string representation |
| `TONUMBER`   | `STRING → MAYBE NUMBER`                      | Parse string to number           |
| `TODATE`     | `STRING → MAYBE DATE`                        | Parse string to date             |
| `TOTIME`     | `STRING → MAYBE TIME`                        | Parse string to time             |
| `TODATETIME` | `STRING → MAYBE DATETIME`                    | Parse string to datetime         |
| `TRUNC`      | `NUMBER, NUMBER → NUMBER`                    | Truncate decimal places          |
| `AS STRING`  | `value AS STRING`                            | Inline string conversion         |

### Numeric Builtins

| Function               | Signature                  | Description                       |
| ---------------------- | -------------------------- | --------------------------------- |
| `FLOOR`                | `NUMBER → NUMBER`          | Round down to integer             |
| `CEILING`              | `NUMBER → NUMBER`          | Round up to integer               |
| `ROUND`                | `NUMBER → NUMBER`          | Round to nearest integer          |
| `EXPONENT`             | `NUMBER → NUMBER → NUMBER` | Exponentiation (base, power)      |
| `LN`                   | `NUMBER → MAYBE NUMBER`    | Natural logarithm                 |
| `LOG10`                | `NUMBER → MAYBE NUMBER`    | Base-10 logarithm                 |
| `SQRT`                 | `NUMBER → MAYBE NUMBER`    | Square root                       |
| `SIN`, `COS`, `TAN`    | `NUMBER → MAYBE NUMBER`    | Trigonometric functions (radians) |
| `ASIN`, `ACOS`, `ATAN` | `NUMBER → MAYBE NUMBER`    | Inverse trigonometric functions   |

### String Builtins

| Function       | Signature                           | Description          |
| -------------- | ----------------------------------- | -------------------- |
| `STRINGLENGTH` | `STRING → NUMBER`                   | Length of string     |
| `SUBSTRING`    | `STRING → NUMBER → NUMBER → STRING` | Extract substring    |
| `TOUPPER`      | `STRING → STRING`                   | Convert to uppercase |
| `TOLOWER`      | `STRING → STRING`                   | Convert to lowercase |

### Date Builtins

| Function           | Signature                         | Description                           |
| ------------------ | --------------------------------- | ------------------------------------- |
| `DATE_FROM_DMY`    | `NUMBER → NUMBER → NUMBER → DATE` | Construct DATE from day, month, year  |
| `DATE_FROM_SERIAL` | `NUMBER → DATE`                   | Construct DATE from serial number     |
| `DATE_SERIAL`      | `DATE → NUMBER`                   | Get serial number from DATE           |
| `DATE_DAY`         | `DATE → NUMBER`                   | Extract day from DATE                 |
| `DATE_MONTH`       | `DATE → NUMBER`                   | Extract month from DATE               |
| `DATE_YEAR`        | `DATE → NUMBER`                   | Extract year from DATE                |
| `TODAY`            | `DATE`                            | Current date (requires `TIMEZONE IS`) |

### Time Builtins

| Function      | Signature       | Description                                 |
| ------------- | --------------- | ------------------------------------------- |
| `TIME_SERIAL` | `TIME → NUMBER` | Get serial number (day fraction) from TIME  |
| `CURRENTTIME` | `TIME`          | Current local time (requires `TIMEZONE IS`) |

### DateTime Builtins

| Function          | Signature           | Description                                                   |
| ----------------- | ------------------- | ------------------------------------------------------------- |
| `DATETIME_SERIAL` | `DATETIME → NUMBER` | Get UTC serial number from DATETIME                           |
| `NOW`             | `DATETIME`          | Current date and time (defaults to UTC without `TIMEZONE IS`) |

### Timezone Builtins

| Function      | Signature   | Description                                                   |
| ------------- | ----------- | ------------------------------------------------------------- |
| `TIMEZONE`    | `STRING`    | Returns the document timezone string (requires `TIMEZONE IS`) |
| `TIMEZONE IS` | declaration | Top-level declaration setting the document timezone           |

### Arithmetic Operators

These operators are always available without import.

| Operator   | Text Alias   | Signature                  | Description           |
| ---------- | ------------ | -------------------------- | --------------------- |
| `+`        | `PLUS`       | `NUMBER → NUMBER → NUMBER` | Addition              |
| `-`        | `MINUS`      | `NUMBER → NUMBER → NUMBER` | Subtraction           |
| `*`        | `TIMES`      | `NUMBER → NUMBER → NUMBER` | Multiplication        |
| `/`        | `DIVIDED BY` | `NUMBER → NUMBER → NUMBER` | Division              |
| `MODULO`   | --           | `NUMBER → NUMBER → NUMBER` | Remainder             |
| `EXPONENT` | --           | `NUMBER → NUMBER → NUMBER` | Exponentiation        |
| `FLOOR`    | --           | `NUMBER → NUMBER`          | Round down to integer |
| `CEILING`  | --           | `NUMBER → NUMBER`          | Round up to integer   |
| `TRUNC`    | --           | `NUMBER → NUMBER`          | Truncate toward zero  |

### Comparison Operators

| Operator | Text Alias     | Signature         | Description               |
| -------- | -------------- | ----------------- | ------------------------- |
| `=`      | `EQUALS`       | `a → a → BOOLEAN` | Equality (not assignment) |
| `>`      | `GREATER THAN` | `a → a → BOOLEAN` | Greater than              |
| `<`      | `LESS THAN`    | `a → a → BOOLEAN` | Less than                 |
| `>=`     | `AT LEAST`     | `a → a → BOOLEAN` | Greater than or equal     |
| `<=`     | `AT MOST`      | `a → a → BOOLEAN` | Less than or equal        |

**Note:** `=` is equality, NOT assignment. L4 has no assignment (pure functional).

### Boolean Operators

| Operator  | Symbol Alias | Precedence  | Description             |
| --------- | ------------ | ----------- | ----------------------- |
| `NOT`     | --           | Highest     | Logical negation        |
| `AND`     | `&&`         | High        | Logical and             |
| `OR`      | `\|\|`       | Medium      | Logical or              |
| `IMPLIES` | `=>`         | Lowest      | Logical implication     |
| `UNLESS`  | --           | = `AND NOT` | Shorthand for `AND NOT` |

### String Operations

| Function           | Signature                           | Description                    |
| ------------------ | ----------------------------------- | ------------------------------ |
| `CONCAT x, y, ...` | `STRING → ... → STRING`             | Concatenate multiple strings   |
| `CONTAINS`         | `STRING → STRING → BOOLEAN`         | Substring test                 |
| `STARTS WITH`      | `STRING → STRING → BOOLEAN`         | Prefix test                    |
| `ENDS WITH`        | `STRING → STRING → BOOLEAN`         | Suffix test                    |
| `INDEXOF`          | `STRING → STRING → NUMBER`          | Find position of substring     |
| `SPLIT`            | `STRING → STRING → LIST OF STRING`  | Split by delimiter             |
| `CHARAT`           | `STRING → NUMBER → STRING`          | Character at index             |
| `SUBSTRING`        | `STRING → NUMBER → NUMBER → STRING` | Substring (str, start, length) |
| `STRINGLENGTH`     | `STRING → NUMBER`                   | String length                  |
| `TOUPPER`          | `STRING → STRING`                   | Convert to uppercase           |
| `TOLOWER`          | `STRING → STRING`                   | Convert to lowercase           |

### List Construction

| Syntax             | Description    |
| ------------------ | -------------- |
| `LIST x, y, z`     | Literal list   |
| `EMPTY`            | Empty list     |
| `x FOLLOWED BY xs` | Cons (prepend) |

### Nullary Builtins

| Function   | Type     | Description                       |
| ---------- | -------- | --------------------------------- |
| `TODAY`    | `DATE`   | Current date in document timezone |
| `TIMEZONE` | `STRING` | Document timezone (IANA name)     |

---

### HTTP and JSON Builtins

| Function     | Signature                         | Description                  |
| ------------ | --------------------------------- | ---------------------------- |
| `FETCH`      | `STRING → STRING`                 | HTTP GET request             |
| `POST`       | `STRING, STRING, STRING → STRING` | HTTP POST request            |
| `ENV`        | `STRING → STRING`                 | Read environment variable    |
| `JSONENCODE` | `a → STRING`                      | Convert value to JSON string |
| `JSONDECODE` | `STRING → EITHER STRING a`        | Parse JSON string to value   |

For detailed HTTP/JSON documentation, see [HTTP and JSON](http-json.md).

For detailed coercion documentation, see [Coercions](../types/coercions.md).
