# Day & Date Functions Reference

Daydate is a library with some basic functions to enable math with dates and days following ISO 8601.
Functions return datestamps (Number of days since 1st January 0000).
A week begins on Monday.

See examples or play around with it in our [online-editor](https://jl4.legalese.com/?id=6f9b3d0a-9afa-4c2d-9360-84a98e3bdb35).

### General use of the DATE type and constructing dates

The type DATE is used throughout the library. It is important to not directly use the uppercase DATE type constructor but to use the following syntax to create DATE objects safely and to make sure the date actually exists in the calendar.

```l4
IMPORT daydate

-- Safe ways to construct dates:
#EVAL August 12 2020          -- DATE OF 12, 8, 2020
#EVAL Date 2 July 2027        -- DATE OF 2, 7, 2027
#EVAL Month Jul 2027          -- DATE OF 1, 7, 2027
#EVAL Year 2027               -- DATE OF 1, 1, 2027
#EVAL Week 1 2027             -- DATE OF 4, 1, 2027
#EVAL Date 12 8 2020          -- DATE OF 12, 8, 2020
#EVAL Date 35 -2 2025         -- DATE OF 4, 11, 2024 - Calculates the days and months fore and backwards based on existing dates

-- DO NOT DO THIS! Unsafe ways to construct dates:
#EVAL Date Apr 1 2025         -- CAREFUL! This returns (DATE OF 4, 1, 2025) or 4th of January 2025 because Apr is used as the NUMBER 4 in this context!
#EVAL DATE 35 2 2025          -- CAREFUL! This returns (DATE OF 35, 2, 2025) because the DATE constructor does not apply the calendar. Always use Date!

-- When using dates in PROVISION statements behind the AT or WITHIN keywords, use the Day function to convert DATE objects to NUMBER (Days since 1st January 0000)
`A provision` MEANS
    PARTY   `The party`
    MUST    `act`
    WITHIN  Day (Apr 2 2026)

`The party` MEANS "Party name"    -- Boilerplate
`act`       MEANS "Do something"
```

Here are some example on how to use more advanced functions:

```l4
IMPORT daydate

#EVAL `is leap year` 2400                                 -- TRUE - Century divisible by 400
#EVAL Feb 29 2000 EQUALS (Date 29 2 2000)                 -- TRUE - leap day
#EVAL Week 53 2009 EQUALS Dec 28 2009                     -- TRUE - Last week of 53-week year
#EVAL Jan 1 2000 MINUS 1 EQUALS Dec 31 1999               -- TRUE - Subtraction across millennium
#EVAL `Weekday of 1st day of month` 3 2025 EQUALS Saturday-- TRUE - First day of month
#EVAL `the earlier of` (Feb 29 2024) (Feb 28 2024)        -- DATE OF 28, 2, 2024 - Earlier date in leap month
```

## Constants

### Time Periods

- `Months in a year`: 12
- `Days in a year`: 365.2425 (accounts for 4-year leap cycle with 100 and 400 year exceptions)
- `Days in a month`: 30.436875 (average month length)
- `Days in a week`: 7

### Month & Weekday Constants

Following ISO standard with January 1, 0000 being a Sunday:

| Weekday     | Alias | Value |
| ----------- | ----- | ----- |
| `Monday`    | `Mon` | 1     |
| `Tuesday`   | `Tue` | 2     |
| `Wednesday` | `Wed` | 3     |
| `Thursday`  | `Thu` | 4     |
| `Friday`    | `Fri` | 5     |
| `Saturday`  | `Sat` | 6     |
| `Sunday`    | `Sun` | 0     |

| Month       | Alias | Value |
| ----------- | ----- | ----- |
| `January`   | `Jan` | 1     |
| `February`  | `Feb` | 2     |
| `March`     | `Mar` | 3     |
| `April`     | `Apr` | 4     |
| `May`       |       | 5     |
| `June`      | `Jun` | 6     |
| `July`      | `Jul` | 7     |
| `August`    | `Aug` | 8     |
| `September` | `Sep` | 9     |
| `October`   | `Oct` | 10    |
| `November`  | `Nov` | 11    |
| `December`  | `Dec` | 12    |

## Date Constructors

### `Date` AKA `Days to date` (from components)

Creates a DATE object from day, month, and year components.

- **Given**:
  - `day`: NUMBER (day of month)
  - `month`: NUMBER (month number 1-12)
  - `year`: NUMBER (4-digit year)
- **Giveth**: DATE object with day, month, and year properties

### `Date` AKA `Days to date` (from datestamp)

Converts a datestamp (NUMBER) into a DATE object.

- **Given**:
  - `days`: NUMBER (datestamp)
- **Giveth**: DATE object with day, month, and year properties

### `Date` AKA `Days to date` (from date)

Passes through a DATE object unchanged.

- **Given**:
  - `date`: DATE object
- **Giveth**: DATE object

## Datestamp Constructors

### `Day` AKA `Date to days` (from components)

Creates a datestamp from day, month, and year.

- **Given**:
  - `day`: NUMBER (day of month)
  - `month`: NUMBER (month number 1-12)
  - `year`: NUMBER (4-digit year)
- **Giveth**: NUMBER datestamp

### `Day` AKA `Date to days` (from date)

Converts a DATE object to a datestamp.

- **Given**:
  - `date`: DATE object
- **Giveth**: NUMBER datestamp

### `Day` AKA `Date to days` (from datestamp)

Passes through a datestamp (NUMBER) unchanged.

- **Given**:
  - `days`: NUMBER (datestamp)
- **Giveth**: NUMBER datestamp

### `Year` (from year)

Creates a datestamp for the first day of a given year.

- **Given**:
  - `year`: NUMBER (4-digit year)
- **Giveth**: DATE object

### `Year` (from date)

Creates a datestamp for the first day of a year from a DATE object.

- **Given**:
  - `date`: DATE object
- **Giveth**: DATE object

### `Month` (from components)

Creates a datestamp for the first day of a month.

- **Given**:
  - `month`: NUMBER (month number 1-12)
  - `year`: NUMBER (4-digit year)
- **Giveth**: DATE object

### `Month` (from datestamp)

Creates a DATE object for the first day of the month from a datestamp.

- **Given**:
  - `days`: NUMBER (datestamp)
- **Giveth**: DATE object

### `Month` (from date)

Creates a DATE object for the first day of the month from a DATE object.

- **Given**:
  - `date`: DATE object
- **Giveth**: DATE object

### `Week` (from components)

Creates a datestamp for the first day of a week a given week and year (Monday).

- **Given**:
  - `week`: NUMBER (week number)
  - `year`: NUMBER (4-digit year)
- **Giveth**: DATE object

### `Week` (from datestamp)

Creates a DATE object for the first day of a week (Monday) from a datestamp.

- **Given**:
  - `days`: NUMBER (datestamp)
- **Giveth**: DATE object

### `Week` (from date)

Creates a DATE object for the first day of a week (Monday) for a given date.

- **Given**:
  - `date`: DATE object
- **Giveth**: DATE object

## Month Helpers

The following functions create DATE objects within specific months. Each takes a day and year and returns a DATE object.
E.g. `January 1 2025 -> DATE WITH day IS 1, month IS 1, year IS 2025`

- `January` (AKA `Jan`)
- `February` (AKA `Feb`)
- `March` (AKA `Mar`)
- `April` (AKA `Apr`)
- `May`
- `June` (AKA `Jun`)
- `July` (AKA `Jul`)
- `August` (AKA `Aug`)
- `September` (AKA `Sep`)
- `October` (AKA `Oct`)
- `November` (AKA `Nov`)
- `December` (AKA `Dec`)

- **Given**:
  - `day`: NUMBER (day of month)
  - `year`: NUMBER (4-digit year)
- **Giveth**: DATE object

### `Month of the year` (from datestamp)

Gets the month number from a datestamp.

- **Given**:
  - `days`: NUMBER (datestamp)
- **Giveth**: NUMBER (month number, e.g. 2 for February)

### `Month of the year` (from date)

Gets the month number of a given date.

- **Given**:
  - `date`: DATE object
- **Giveth**: NUMBER (month number, e.g. 2 for February)

## Datestamp Math Helpers

### `Months since year start to days` (from components)

Calculates total days from start of year to beginning of a given month.

- **Given**:
  - `month`: NUMBER (month number 1-12)
  - `year`: NUMBER (4-digit year)
- **Giveth**: NUMBER (amount of days)

### `Months since year start to days` (from datestamp)

Calculates total number of days from start of year to a given month from a datestamp.

- **Given**:
  - `days`: NUMBER (datestamp)
- **Giveth**: NUMBER (number of days)

### `Months since year start to days` (from date)

Calculates total number of days from start of year to a given month from a DATE object.

- **Given**:
  - `date`: DATE object
- **Giveth**: NUMBER (number of days)

### `Days in month` (from components)

Gets the total number of days in a given month.

- **Given**:
  - `month`: NUMBER (month number 1-12)
  - `year`: NUMBER (4-digit year)
- **Giveth**: NUMBER (number of days)

### `Days in month` (from datestamp)

Gets the total number of days in a given month from a datestamp.

- **Given**:
  - `days`: NUMBER (datestamp)
- **Giveth**: NUMBER (number of days)

### `Days in month` (from date)

Gets the total number of days in a given month for a given date.

- **Given**:
  - `date`: DATE object
- **Giveth**: NUMBER (number of days)

### `Days in year` (from year)

Calculates number of days in a given year.

- **Given**:
  - `year`: NUMBER (4-digit year)
- **Giveth**: NUMBER (365 or 366)

### `Days in year` (from date)

Calculates number of days in a given year from a DATE object.

- **Given**:
  - `date`: DATE object
- **Giveth**: NUMBER (365 or 366)

### `Years to days` (from year)

Calculates the total number of days from year 0 to a given year.

- **Given**:
  - `year`: NUMBER (4-digit year)
- **Giveth**: NUMBER (number of days)

### `Years to days` (from date)

Calculates the total number of days from year 0 to a DATE objects year.

- **Given**:
  - `date`: DATE object
- **Giveth**: NUMBER (number of days)

## Internal Helper Functions

The library includes several internal helper functions used by the main functions:

### `Year of days`

Calculates the year from a datestamp using efficient division algorithms.

- **Given**:
  - `days`: NUMBER (datestamp)
- **Giveth**: NUMBER (year)

### `Month of days`

Internal recursive function to determine the month from day-of-year calculation.

- **Given**:
  - `i`: NUMBER (current month index)
  - `c`: NUMBER (comparison value)
- **Giveth**: NUMBER (month)

## Weekday Functions

### `Weekday of` (from datestamp)

Gets the weekday number (0-6) for a given datestamp.

- **Given**:
  - `days`: NUMBER (datestamp)
- **Giveth**: NUMBER (0-6)

### `Weekday of` (from date)

Gets the weekday number (0-6) for a given date.

- **Given**:
  - `date`: DATE object
- **Giveth**: NUMBER (0-6)

### `Weekday of 1st day of month` (from components)

Gets the weekday of the first day of a month.

- **Given**:
  - `month`: NUMBER (month number 1-12)
  - `year`: NUMBER (4-digit year)
- **Giveth**: NUMBER (0-6)

### `Weekday of 1st day of month` (from datestamp)

Gets the weekday of the first day of a month from a datestamp.

- **Given**:
  - `days`: NUMBER (datestamp)
- **Giveth**: NUMBER (0-6)

### `Weekday of 1st day of month` (from date)

Gets the weekday of the first day of a month for a given date.

- **Given**:
  - `date`: DATE object
- **Giveth**: NUMBER (0-6)

### `Weekday of 1st day of year` (from year)

Gets the weekday of January 1st of a given year.
E.g. `2025 -> Wednesday`

- **Given**:
  - `year`: NUMBER (4-digit year)
- **Giveth**: NUMBER (0-6)

### `Weekday of 1st day of year` (from date)

Gets the weekday of January 1st from a DATE objects year.

- **Given**:
  - `date`: DATE object
- **Giveth**: NUMBER (0-6)

## Week Functions

### `Week of the year` (from datestamp)

Gets the week of the year number from a datestamp.

- **Given**:
  - `days`: NUMBER (datestamp)
- **Giveth**: NUMBER (week number)

### `Week of the year` (from date)

Gets the week of the year number for a given date.

- **Given**:
  - `date`: DATE object
- **Giveth**: NUMBER (week number)

### `Weeks in year` (from year)

Calculates number of weeks in a given year.
E.g. `2004 -> 53`

- **Given**:
  - `year`: NUMBER (4-digit year)
- **Giveth**: NUMBER (52 or 53)

### `Weeks in year` (from date)

Calculates number of weeks in a given year from a DATE object.

- **Given**:
  - `date`: DATE object
- **Giveth**: NUMBER (52 or 53)

## Attribute Checkers

### `is weekend` (from datestamp)

Checks if a given datestamp falls on a weekend.

- **Given**:
  - `days`: NUMBER (datestamp)
- **Giveth**: BOOLEAN

### `is weekend` (from date)

Checks if a given date falls on a weekend.

- **Given**:
  - `date`: DATE object
- **Giveth**: BOOLEAN

### `is weekday` (from datestamp)

Checks if a given datestamp falls on a weekday.

- **Given**:
  - `days`: NUMBER (datestamp)
- **Giveth**: BOOLEAN

### `is weekday` (from date)

Checks if a given date falls on a weekday.

- **Given**:
  - `date`: DATE object
- **Giveth**: BOOLEAN

### `is leap year` (from year)

Checks if a given year is a leap year.
E.g. `2004 -> TRUE`

- **Given**:
  - `year`: NUMBER (4-digit year)
- **Giveth**: BOOLEAN

### `is leap year` (from date)

Checks if a given date's year is a leap year.

- **Given**:
  - `date`: DATE object
- **Giveth**: BOOLEAN

## Relative Time Phrases

### `on day`

Gets the date for the nth day relative to a given date (1-based).
e.g. `on day` 1 (Jan 1 2025) returns Jan 1 2025, `on day` 2 returns Jan 2 2025

- **Given**:
  - `day`: NUMBER (nth day, 1 being the same day)
  - `date`: DATE object
- **Giveth**: DATE object (based on Given)

### `the day after`

Gets the date for the day AFTER a given date.

- **Given**:
  - `date`: DATE object
- **Giveth**: DATE object (based on Given)

### `the day before`

Gets the date for the day BEFORE a given date.

- **Given**:
  - `date`: DATE object
- **Giveth**: DATE object (based on Given)

### `the week after`

Gets the date for Monday of NEXT week from a given date.

- **Given**:
  - `date`: DATE object
- **Giveth**: DATE object (based on Given)

### `the week before`

Gets the date for Monday of the PREVIOUS week from a given date.

- **Given**:
  - `date`: DATE object
- **Giveth**: DATE object (based on Given)

### `the month after`

Gets the date for the first day of NEXT month.

- **Given**:
  - `date`: DATE object
- **Giveth**: DATE object (based on Given)

### `the month before`

Gets the date for the first day of the PREVIOUS month.

- **Given**:
  - `date`: DATE object
- **Giveth**: DATE object (based on Given)

### `the year after`

Gets the date for the first day of NEXT year.

- **Given**:
  - `date`: DATE object
- **Giveth**: DATE object (based on Given)

### `the year before`

Gets the date for the first day of the PREVIOUS year.

- **Given**:
  - `date`: DATE object
- **Giveth**: DATE object (based on Given)

## Comparators

### `the earlier of`

Returns the earlier of two dates.

- **Given**:
  - `date1`: DATE object
  - `date2`: DATE object
- **Giveth**: DATE object (based on Given)

### `the later of`

Returns the later of two dates.

- **Given**:
  - `date1`: DATE object
  - `date2`: DATE object
- **Giveth**: DATE object (based on Given)

## Native Overloads

The library includes native operator overloads for DATE objects:

### Comparison Operators

- `__GEQ__` (>=): Compares if first date is greater than or equal to second
- `__LEQ__` (<=): Compares if first date is less than or equal to second
- `__GT__` AKA `is after`: Compares if first date is after second
- `__LT__` AKA `is before`: Compares if first date is before second

### Arithmetic Operators

- `__PLUS__`: Adds days to a DATE (DATE + NUMBER) or adds datestamps (DATE + DATE)
- `__MINUS__`: Subtracts days from a DATE (DATE - NUMBER) or subtracts datestamps (DATE - DATE)
- `__PLUS__`: Adds days to a datestamp (NUMBER + DATE)
- `__MINUS__`: Subtracts a DATE from a number (NUMBER - DATE)

These allow natural operations like:

```l4
Jan 1 2025 PLUS 30        -- Adds 30 days
Feb 1 2025 MINUS Jan 1 2025 -- Returns difference in days
Jan 1 2025 GREATER THAN Dec 31 2024 -- Returns TRUE
```

## Stringify Functions

### `Name of month` (from date)

Converts a DATE object to month name. (e.g. January)

- **Given**:
  - `date`: DATE object
- **Giveth**: STRING (month name)

### `Name of month` (from datestamp)

Converts a datestamp to month name. (e.g. January)

- **Given**:
  - `days`: NUMBER (datestamp)
- **Giveth**: STRING (month name)

### `Name of weekday` (from datestamp)

Converts a datestamp to weekday name. (e.g. Monday)

- **Given**:
  - `days`: NUMBER (datestamp)
- **Giveth**: STRING (weekday name)

### `Name of weekday` (from date)

Converts a DATE object to weekday name. (e.g. Monday)

- **Given**:
  - `date`: DATE object
- **Giveth**: STRING (weekday name)
