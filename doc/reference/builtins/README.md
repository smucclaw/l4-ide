# Built-in Functions (Not a Library)

L4 includes several **built-in functions** that are always available without importing any library. These are implemented in the compiler core:

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

| Function               | Description                       |
| ---------------------- | --------------------------------- |
| `FLOOR`                | Round down to integer             |
| `CEILING`              | Round up to integer               |
| `ROUND`                | Round to nearest integer          |
| `EXPONENT`             | Exponentiation (base, power)      |
| `LN`                   | Natural logarithm                 |
| `LOG10`                | Base-10 logarithm                 |
| `SQRT`                 | Square root                       |
| `SIN`, `COS`, `TAN`    | Trigonometric functions (radians) |
| `ASIN`, `ACOS`, `ATAN` | Inverse trigonometric functions   |

### String Builtins

| Function       | Description          |
| -------------- | -------------------- |
| `STRINGLENGTH` | Length of string     |
| `SUBSTRING`    | Extract substring    |
| `TOUPPER`      | Convert to uppercase |
| `TOLOWER`      | Convert to lowercase |

### Date Builtins

| Function           | Description                           |
| ------------------ | ------------------------------------- |
| `DATE_FROM_DMY`    | Construct DATE from day, month, year  |
| `DATE_FROM_SERIAL` | Construct DATE from serial number     |
| `DATE_SERIAL`      | Get serial number from DATE           |
| `DATE_DAY`         | Extract day from DATE                 |
| `DATE_MONTH`       | Extract month from DATE               |
| `DATE_YEAR`        | Extract year from DATE                |
| `TODAY`            | Current date (requires `TIMEZONE IS`) |

### Time Builtins

| Function      | Description                                 |
| ------------- | ------------------------------------------- |
| `TIME_SERIAL` | Get serial number (day fraction) from TIME  |
| `CURRENTTIME` | Current local time (requires `TIMEZONE IS`) |

### DateTime Builtins

| Function          | Description                                                   |
| ----------------- | ------------------------------------------------------------- |
| `DATETIME_SERIAL` | Get UTC serial number from DATETIME                           |
| `NOW`             | Current date and time (defaults to UTC without `TIMEZONE IS`) |

### Timezone Builtins

| Function      | Description                                                   |
| ------------- | ------------------------------------------------------------- |
| `TIMEZONE`    | Returns the document timezone string (requires `TIMEZONE IS`) |
| `TIMEZONE IS` | Top-level declaration setting the document timezone           |

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
