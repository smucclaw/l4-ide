# Day & Date Functions Reference

Daydate is a library with some basic functions to enable math with dates and days following ISO 8601.
Functions return datestamps (Number of days since 1st January 0000).
A week begins on Monday.

Check the test/daydate.l4 file to see examples or play around with it in our [online-editor](https://jl4.legalese.com/?id=70d47963-10c2-4237-81e5-dd683c1ef08b).

Here are some example on how to use it:

```l4
IMPORT daydate

#EVAL `is leap year` 2400                                 -- TRUE - Century divisible by 400
#EVAL Feb 29 2000 EQUALS Day (Date 29 2 2000)             -- TRUE - leap day
#EVAL Week 53 2009 EQUALS Dec 28 2009                     -- TRUE - Last week of 53-week year
#EVAL Jan 1 2000 MINUS 1 EQUALS Dec 31 1999               -- TRUE - Subtraction across millennium
#EVAL `Weekday of 1st day of month` 3 2025 EQUALS Saturday-- TRUE - First day of month
#EVAL `the earlier of` (Feb 29 2024) (Feb 28 2024)        -- 739308 - Earlier in leap month
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

### `Date` AKA `Days to date`

Converts a datestamp into a DATE object.

- **Given**:
  - `days`: NUMBER datestamp or DATE object
- **Giveth**: DATE object with day, month, and year properties

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
  - `date`: NUMBER datestamp or DATE object
- **Giveth**: NUMBER datestamp

### `Year` (from year)

Creates a datestamp for the first day of a given year.

- **Given**:
  - `year`: NUMBER (4-digit year)
- **Giveth**: NUMBER datestamp

### `Year` (from date)

Creates a datestamp for the first day of a year from a DATE object.

- **Given**:
  - `date`: DATE object
- **Giveth**: NUMBER datestamp

### `Month` (from components)

Creates a datestamp for the first day of a month.

- **Given**:
  - `month`: NUMBER (month number 1-12)
  - `year`: NUMBER (4-digit year)
- **Giveth**: NUMBER datestamp

### `Month` (from date)

Creates a datestamp for the first day of a month from a DATE object.

- **Given**:
  - `date`: NUMBER datestamp or DATE object
- **Giveth**: NUMBER datestamp

### `Week` (from components)

Creates a datestamp for the first day of a week a given week and year (Monday).

- **Given**:
  - `week`: NUMBER (week number)
  - `year`: NUMBER (4-digit year)
- **Giveth**: NUMBER datestamp

### `Week` (from date)

Creates a datestamp for the first day of a week (Monday) for a given date.

- **Given**:
  - `date`: NUMBER datestamp or DATE object
- **Giveth**: NUMBER datestamp

## Month Helpers

The following functions create datestamps within specific months. Each takes a day and year and returns a DATE object.
E.g. `January 1 2025 -> DATE OF 1, 1, 2025`

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

### `Month of the year`

Gets the month number of a given date.

- **Given**:
  - `date`: NUMBER datestamp or DATE object
- **Giveth**: NUMBER (month number, e.g. 2 for February)

## Datestamp Math Helpers

### `Months since year start to days` (from components)

Calculates total days from start of year to beginning of a given month.

- **Given**:
  - `month`: NUMBER (month number 1-12)
  - `year`: NUMBER (4-digit year)
- **Giveth**: NUMBER (amount of days)

### `Months since year start to days` (from date)

Calculates total number of days from start of year to a given month from a DATE object.

- **Given**:
  - `date`: NUMBER datestamp or DATE object
- **Giveth**: NUMBER (number of days)

### `Days in month` (from components)

Gets the total number of days in a given month.

- **Given**:
  - `month`: NUMBER (month number 1-12)
  - `year`: NUMBER (4-digit year)
- **Giveth**: NUMBER (number of days)

### `Days in month` (from date)

Gets the total number of days in a given month for a given date.

- **Given**:
  - `date`: NUMBER datestamp or DATE object
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

## Weekday Functions

### `Weekday of` (from days)

Gets the weekday number (0-6) for a given date.

- **Given**:
  - `date`: NUMBER datestamp or DATE object
- **Giveth**: NUMBER (0-6)

### `Weekday of 1st day of month` (from components)

Gets the weekday of the first day of a month.

- **Given**:
  - `month`: NUMBER (month number 1-12)
  - `year`: NUMBER (4-digit year)
- **Giveth**: NUMBER (0-6)

### `Weekday of 1st day of month` (from date)

Gets the weekday of the first day of a month for a given date.

- **Given**:
  - `date`: NUMBER datestamp or DATE object
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

### `Week of the year`

Gets the week of the year number for a given date.

- **Given**:
  - `date`: NUMBER datestamp or DATE object
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

### `is weekend`

Checks if a given date falls on a weekend.

- **Given**:
  - `date`: NUMBER datestamp or DATE object
- **Giveth**: BOOLEAN

### `is weekday`

Checks if a given date falls on a weekday.

- **Given**:
  - `date`: NUMBER datestamp or DATE object
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

Gets the date for the nth day of a given date.
e.g. ``#EVAL Date (`on day` 2 (`the week after` (Jan 2 2026))) -> DATE OF 6, 1, 2026``

- **Given**:
  - `day`: NUMBER (nth day, 1 being the same day)
  - `date`: NUMBER datestamp or DATE object
- **Giveth**: NUMBER datestamp or DATE object (based on Given)

### `the day after`

Gets the date for the day AFTER a given date.

- **Given**:
  - `date`: NUMBER datestamp or DATE object
- **Giveth**: NUMBER datestamp or DATE object (based on Given)

### `the day before`

Gets the date for the day BEFORE a given date.

- **Given**:
  - `days`: NUMBER datestamp or DATE object
- **Giveth**: NUMBER datestamp or DATE object (based on Given)

### `the week after`

Gets the date for Monday of NEXT week from a given date.

- **Given**:
  - `date`: NUMBER datestamp or DATE object
- **Giveth**: NUMBER datestamp or DATE object (based on Given)

### `the week before`

Gets the date for Monday of the PREVIOUS week from a given date.

- **Given**:
  - `date`: NUMBER datestamp or DATE object
- **Giveth**: NUMBER datestamp or DATE object (based on Given)

### `the month after`

Gets the date for the first day of NEXT month.

- **Given**:
  - `date`: NUMBER datestamp or DATE object
- **Giveth**: NUMBER datestamp or DATE object (based on Given)

### `the month before`

Gets the date for the first day of the PREVIOUS month.

- **Given**:
  - `date`: NUMBER datestamp or DATE object
- **Giveth**: NUMBER datestamp or DATE object (based on Given)

### `the year after`

Gets the date for the first day of NEXT year.

- **Given**:
  - `date`: NUMBER datestamp or DATE object
- **Giveth**: NUMBER datestamp or DATE object (based on Given)

### `the year before`

Gets the date for the first day of the PREVIOUS year.

- **Given**:
  - `date`: NUMBER datestamp or DATE object
- **Giveth**: NUMBER datestamp or DATE object (based on Given)

## Comparators

### `the earlier of`

Returns the earlier of two dates.

- **Given**:
  - `date1`: NUMBER datestamp or DATE object
  - `date2`: NUMBER datestamp or DATE object
- **Giveth**: NUMBER datestamp or DATE object (based on Given)

### `the later of`

Returns the later of two dates.

- **Given**:
  - `date1`: NUMBER datestamp or DATE object
  - `date2`: NUMBER datestamp or DATE object
- **Giveth**: NUMBER datestamp or DATE object (based on Given)

## Stringify Functions

### `Name of month`

Converts a datestamp to month name. (e.g. January)

- **Given**:
  - `days`: NUMBER datestamp or DATE object
- **Giveth**: STRING (month name)

### `Name of weekday`

Converts a datestamp to weekday name. (e.g. Monday)

- **Given**:
  - `days`: NUMBER datestamp or DATE object
- **Giveth**: STRING (weekday name)
