# ACTUS Library

Algorithmic Contract Types Unified Standards ([actusfrf.org](https://www.actusfrf.org)) implementation in L4. Provides types and functions for modelling standardised financial contracts.

Import with `IMPORT actus` to get all modules, or import individual modules selectively.

### Location

| File                                                                                                   | Purpose                                   |
| ------------------------------------------------------------------------------------------------------ | ----------------------------------------- |
| [actus.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/actus.l4)                   | Main entry point (re-exports all modules) |
| [actus-core.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/actus-core.l4)         | Core types (contract types, roles, enums) |
| [actus-terms.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/actus-terms.l4)       | Contract term definitions                 |
| [actus-state.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/actus-state.l4)       | Contract state variables                  |
| [actus-events.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/actus-events.l4)     | Event types and operations                |
| [actus-daycount.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/actus-daycount.l4) | Day count conventions and year fractions  |
| [actus-schedule.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/actus-schedule.l4) | Schedule generation                       |

### Core Types (`actus-core`)

**Contract Type Taxonomy** (~32 ACTUS contract types organised into families):

- `Basic Contract Type` — PAM, LAM, NAM, ANN, CLM, CSH, STK, UMP, COM
- `Exotic Basic Contract Type` — LAX, NAX, ANX
- `Combined Contract Type` — SWAPS, SWPPV, OPTNS, FUTUR, FXOUT, CAPFL, BCS
- `Credit Enhancement Contract Type` — CEG, CEC, MAR

**Contract Role** — RPA, RPL, CLO, CNO, COL, LG, ST, BUY, SEL, RFL, PFL, RF, PF

- `` `role sign` `` — sign multiplier for payoff calculations (+1 or -1)

**Contract Performance** — PF (performant), DL (delayed), DQ (delinquent), DF (default), MA (matured), TE (terminated)

- `` `is non-performing` ``, `` `is active` `` — performance predicates

**Day Count Convention** — AA, A360, A365, `30E360`, `30E360ISDA`, BUS252

**Other Enums:**

- `End of Month Convention` — EOM, SD, EOMP
- `Business Day Convention` — SCR, SCRF, SCRP, SCRMF, SCRMP
- `Calendar Type` — NoCalendar, MondayToFriday, Calendar
- `Period Unit` — Days, Weeks, Months, Quarters, Years
- `Stub Convention` — Long/Short Initial/Final
- `Interest Payment Convention` — PayInterest, CapitalizeInterest
- `Penalty Type` — NoPenalty, AbsolutePenalty, RelativePenalty, InterestPenalty
- `Option Type` / `Option Exercise Type` — Call, Put, Straddle / European, American, Bermudan
- `Scaling Effect` — NoScaling, InterestScaling, PrincipalScaling, BothScaling
- `ACTUS Currency` — USD, EUR, GBP, JPY, CHF, CAD, AUD, CNY, HKD, SGD, or custom via `ACTUS Other`

**Cycle** — record with `period`, `periodUnit`, `stub`; convenience constructors: `` `Monthly Cycle` ``, `` `Quarterly Cycle` ``, `` `Semi-Annual Cycle` ``, `` `Annual Cycle` ``

### Contract Terms (`actus-terms`)

- `ACTUS Contract Terms` — core contract parameters (ID, type, role, dates, principal, rate, day count convention)
- `FXOUT Terms` — FX outright transaction parameters
- `` `implied exchange rate` `` — derive rate from FX term amounts

### Contract State (`actus-state`)

- `Contract State` — runtime state (statusDate, contractPerformance, notionalPrincipal, accruedInterest, nominalInterestRate, feeAccrued)
- `` `is fully paid` ``, `` `is in default` ``, `` `outstanding balance` `` — state predicates
- `` `empty state` `` — default initial state

### Contract Events (`actus-events`)

**Event Types:** IED, PR, PP, MD, IP, IPCI, RR, RRF, FP, PY, TD, XD, STD, DV, CE, AD

- `Contract Event` — record with eventType, eventDate, payoff, currency, notionalPrincipal, nominalInterestRate, accruedInterest
- `` `make event` `` — event constructor
- `` `event sequence` ``, `` `event name` ``, `` `event acronym` `` — event metadata
- `` `is principal event` ``, `` `is interest event` ``, `` `has cash flow` `` — event predicates
- `` `total payoff` `` — sum payoffs across a list of events

### Day Count Conventions (`actus-daycount`)

- `` `year fraction` `` — Y(S,E) calculation dispatching on convention
- `` `year fraction A365` ``, `` `year fraction A360` `` — specific convention implementations
- `` `days between` `` — raw day count between two dates
- `` `accrued interest for period` `` — interest accrual given principal, rate, dates, and convention

### Schedule Generation (`actus-schedule`)

- `` `add months` ``, `` `add days` `` — date arithmetic with month-end clamping
- `` `monthly schedule` `` — generate a list of monthly dates from a start date
- `` `schedule length` `` — count dates in a schedule

### Evaluation Engine (`actus`)

**Payoff Functions (POF):**

- `` `POF_IED` `` — Initial Exchange Date payoff
- `` `POF_IP` `` — Interest Payment payoff
- `` `POF_MD` `` — Maturity payoff
- `` `POF_PR` `` — Principal Redemption payoff

**State Transition Functions (STF):**

- `` `STF_IED` `` — initialise state after initial exchange
- `` `STF_IP` `` — reset accrued interest after payment
- `` `STF_MD` `` — contract reaches maturity
- `` `STF_PR` `` — reduce notional by redemption amount

**Interest Accrual:**

- `` `accrue interest to` `` — accrue interest from current state date to a target date

**FX Transaction (FXOUT):**

- `` `FXOUT pay event` ``, `` `FXOUT receive event` `` — generate FX cash flow events

**Cash Flow Projection:**

- `` `net cash flow` `` — sum all cash flows from a list of events

**See the [actus.l4](https://github.com/smucclaw/l4-ide/blob/main/jl4-core/libraries/actus.l4) source and sub-modules for full details.**
