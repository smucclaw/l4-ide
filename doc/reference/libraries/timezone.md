# Timezone Library

IANA timezone name constants for use with `TIMEZONE IS` declarations and DateTime constructors. Maps common timezone abbreviations to their full IANA names.
Can be imported into L4 files with `IMPORT timezone`.

### Location

[jl4-core/libraries/timezone.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/timezone.l4)

### Features

- Named constants for common timezone abbreviations
- Automatic DST handling (EST and EDT map to same IANA zone)
- Coverage: Americas, Europe, Asia-Pacific, Universal

### DST Handling

Standard and daylight-saving abbreviations map to the same IANA zone. The underlying timezone database handles DST transitions automatically based on the actual date:

```l4
IMPORT timezone

-- Both resolve to "America/New_York"
-- DST is determined by the date, not the abbreviation
#ASSERT EST EQUALS EDT
```

### Available Constants

**Americas:**

| Constant | IANA Zone           |
| -------- | ------------------- |
| EST, EDT | America/New_York    |
| CST, CDT | America/Chicago     |
| MST, MDT | America/Denver      |
| PST, PDT | America/Los_Angeles |

**Europe:**

| Constant  | IANA Zone       |
| --------- | --------------- |
| GMT, BST  | Europe/London   |
| CET, CEST | Europe/Berlin   |
| EET, EEST | Europe/Helsinki |

**Asia-Pacific:**

| Constant   | IANA Zone        |
| ---------- | ---------------- |
| IST        | Asia/Kolkata     |
| SGT        | Asia/Singapore   |
| HKT        | Asia/Hong_Kong   |
| JST        | Asia/Tokyo       |
| KST        | Asia/Seoul       |
| AEST, AEDT | Australia/Sydney |
| NZST, NZDT | Pacific/Auckland |

**Universal:**

| Constant | IANA Zone |
| -------- | --------- |
| UTC      | Etc/UTC   |

### Usage with TIMEZONE IS

```l4
IMPORT timezone

TIMEZONE IS SGT  -- sets document timezone to "Asia/Singapore"
```

You can also use IANA strings directly without importing this library:

```l4
TIMEZONE IS "Asia/Singapore"
```

### Example: Timezone Constants

[timezone-example.l4](timezone-example.l4)

**See [timezone.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/timezone.l4) source for all constants.**
