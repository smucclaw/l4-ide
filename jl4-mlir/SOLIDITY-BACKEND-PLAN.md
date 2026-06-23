# jl4-mlir → Solidity/EVM backend plan

Goal: extend the L4 compiler so the same `.l4` source that today produces a
WASM module can also produce **EVM bytecode** suitable for deploying as an
ERC-20 (or ERC-20-shaped) smart contract. The L4 → MLIR pipeline stays;
WASM and EVM become two backends behind the same front-end.

The motivating use case: tokenisation of legal instruments — letting a
contract drafter write the rules once in L4 and get both a verifiable
off-chain evaluator (WASM) and an on-chain enforcer (EVM) from the same
source.

## What "ERC-20 support" means here

A minimum viable ERC-20 token contract must:

- Hold `balances` and `allowances` in persistent storage.
- Expose six external functions: `totalSupply`, `balanceOf`, `transfer`,
  `approve`, `transferFrom`, `allowance` — with the exact signatures and
  return shapes the standard requires.
- Emit two events with their canonical topic-0 hashes:
  `Transfer(address indexed, address indexed, uint256)` and
  `Approval(address indexed, address indexed, uint256)`.
- Survive Solidity's gas model (no unbounded loops over user state, no
  unnecessary storage writes, no reentrancy on receiver hooks).

Anything we cannot encode under those constraints we either (a) detect at
compile time and refuse, or (b) leave to a hand-written wrapper.

## Guiding principles

1. **Two backends, one front-end.** Parsing, typechecking, MLIR lowering up
   to the abstract op stays shared. The fork point is one MLIR pass that
   either lowers to `arith`/`func`/`scf`/`llvm` (current WASM path) or to
   the new Yul/EVM target.
2. **Fail loud at compile time.** The WASM backend got into trouble by
   silently emitting `0.0` for unsupported constructs. The EVM backend must
   refuse anything it cannot encode safely — undefined floats, host calls,
   unbounded loops over user-controlled inputs.
3. **Storage is precious, calldata is constrained.** Every L4 declaration
   needs an explicit storage vs. transient classification. Default to
   transient; promote to storage only on opt-in.
4. **The standard's bytes are non-negotiable.** Event topic-0 hashes,
   function selectors, ABI encoding rules are all spec'd. Match them
   byte-for-byte the way the WASM backend matches `jl4-service` byte-for-byte.

## The honest shape of the work

This is not a port of the WASM backend; it's a parallel one. Three things
in the current pipeline don't translate at all:

| Current (WASM)                      | EVM equivalent                    | Effort               |
| ----------------------------------- | --------------------------------- | -------------------- |
| `f64`-boxed rationals (JS BigInt)   | uint256 fixed-point, no float     | rewrite numeric path |
| JS host imports for math/strings    | none (or precompiled contracts)   | drop most builtins   |
| Linear memory + manual allocation   | storage slots + memory + calldata | new memory model     |
| Deontic = JS event-log interpreter  | Deontic = eager state mutation    | rewrite semantics    |
| `__l4_today_serial` from `Date.now` | `block.timestamp`                 | replace builtin      |
| JSON wire envelope                  | ABI-encoded calldata              | new marshaler        |

Estimated effort to a working "deploy + transfer + emit Transfer event":
**~6–8 weeks** of focused work, on top of the current MLIR foundation.

---

## Phase 0 — Locked decisions (one-time, gates everything else)

### 0.1 Target IR: Yul, not raw EVM bytecode

Yul is solc's structured intermediate language. It has functions, `switch`,
typed values, and runs through solc's optimiser. We emit Yul; `solc
--strict-assembly --bin` produces bytecode.

Rejected alternatives:

- **LLVM → EVM** — there is no production-grade EVM backend for LLVM. The
  `evm-lvm` project is dormant.
- **Raw bytecode emission** — loses solc's optimiser passes, loses
  source-map support for debuggers, doubles the surface area.
- **A new MLIR dialect (`evm` / `solidity`)** — interesting long-term but
  unjustified for first delivery. We can still route through MLIR for the
  shared front-end work and lower to Yul at the bottom.

### 0.2 Numeric representation: fixed-point uint256 at 1e18 scale

Solidity has no floats. The DeFi convention is "everything is uint256
scaled by 1e18 (or by the token's `decimals`)." We adopt that.

- `NUMBER` lowers to `uint256` in storage and calldata.
- Integer-typed bindings (declared `IS A NUMBER` with `@scale 0` or used
  only in integer contexts) lower without the 1e18 factor — saves a
  multiplication.
- Decimal literals are normalised at compile time: `0.1` becomes
  `100000000000000000`. The current rational-pool runtime doesn't survive.
- Negative numbers: use `int256` (two's complement) where the typechecker
  proves signedness is needed; default to uint256.
- **Out of scope:** trigonometry, logarithms, square roots — Solidity has
  no built-in math library, and the userland libs (ABDKMath, PRBMath) add
  ~5KB to the contract. Mark `__l4_sin/_cos/_tan/_ln/_log10/_sqrt/_pow` as
  **EVM-unsupported** and refuse to compile any function that calls them.

### 0.3 Numeric range is the user's problem

uint256's max is 2^256 - 1. With 1e18 scaling that's ~1.16 × 10^59 in the
"whole-number" part. If an L4 function multiplies two unbounded quantities,
overflow is possible. We don't try to prevent this statically; we rely on
Solidity 0.8+'s built-in overflow checks (which `revert` on wrap), and
document the boundary.

---

## Phase 1 — Type system extensions

### 1.1 Add `ADDRESS` as a primitive L4 type

20-byte EVM addresses. Distinct from `STRING` (which currently means
arbitrary text) and distinct from Party records.

```l4
DECLARE Holder HAS
    `wallet`     IS A ADDRESS
    `balance`    IS A NUMBER
```

Lowers to a Solidity `address` field in the generated struct. The wasm
backend can treat it as a 20-byte string for parity testing.

### 1.2 Add `BYTES32` for hashes and content addresses

Useful for token IDs, commit-reveal hashes, merkle roots. Lowers to
Solidity `bytes32`. Optional in v1; required if we want NFT or
permit-style support later.

### 1.3 `@storage` / `@transient` annotations on DECLAREs

```l4
@storage
DECLARE Balances HAS
    `holder`   IS A ADDRESS
    `amount`   IS A NUMBER

@storage
DECLARE Allowances HAS
    `owner`    IS A ADDRESS
    `spender`  IS A ADDRESS
    `amount`   IS A NUMBER
```

Records marked `@storage` get a storage slot assignment (Phase 2.6).
Everything else is transient — passed in calldata or computed in memory,
discarded at end of call.

The typechecker enforces: state-changing functions can write storage;
`view`/`pure` functions can read but not write; pure functions can't even
read.

### 1.4 Promote certain L4 records to Solidity `mapping`

A `@storage` record whose first field is `ADDRESS` and that's looked up
exclusively by that address lowers to `mapping(address => …)` instead of a
flat array. The compiler detects this by analysing access patterns. ERC-20
balances and allowances both fit this shape.

### 1.5 Function classification (`view` / `pure` / state-changing)

The compiler infers this from the function body:

- No storage reads, no external calls → `pure`.
- Reads storage, no writes → `view`.
- Writes storage or emits events → state-changing.

Each L4 `@export` gets the right Solidity attribute in the generated
external function declaration. Mismatches between L4 source and Solidity
expectation (e.g. an `@export view` that secretly writes) become compile
errors.

### 1.6 Built-in `caller` party

EVM transactions have an implicit `msg.sender`. Add a magic L4 name:

```l4
PARTY caller MUST transfer amount TO recipient
```

`caller` is not a parameter — the compiler resolves it to `msg.sender` at
the call site. On the WASM backend it remains an explicit parameter (for
parity testing, pass `msg.sender` in the request).

### 1.7 Built-in time

`__l4_today_serial`, `__l4_now_serial`, `__l4_current_time` already exist
on WASM as JS host calls. On EVM these all lower to `block.timestamp`
(seconds since epoch, the same scale L4 uses internally for `DATE`).

`block.number` becomes available as an additional builtin (`block`?
`current block`?). Useful for deadlines expressed in blocks rather than
seconds.

---

## Phase 2 — Core lowering: L4 → Yul

### 2.1 Records → Solidity structs

The existing `RetSchema` (record kind, fieldOrder, fields map) carries
enough information. Emit a Yul `struct` per record type with fields in
declaration order. For storage records, additionally emit the slot
assignment from Phase 2.6.

### 2.2 Enums → uint8

L4 enums are already tag-indexed in the WASM backend; reuse the same
indices. Solidity enums are backed by `uint8` by default.

### 2.3 `IF/THEN/ELSE` → Yul `switch`

Yul has no `if/else`, only `if cond { … }` (true-only) and `switch`. Lower
`IF a THEN b ELSE c` to:

```yul
switch a
case 1 { result := b }
default { result := c }
```

### 2.4 Function selectors and ABI encoding

Each external function gets a 4-byte selector computed as
`keccak256("name(types)")[:4]`. Type names follow Solidity ABI conventions
(`uint256`, `address`, `bool`, struct member types listed inline). The
selector goes in a top-level dispatch switch:

```yul
let selector := shr(224, calldataload(0))
switch selector
case 0xa9059cbb { transfer() }      // transfer(address,uint256)
case 0x095ea7b3 { approve() }       // approve(address,uint256)
case 0x70a08231 { balanceOf() }     // balanceOf(address)
…
default { revert(0, 0) }
```

Internal call sites in L4 stay positional (no selector overhead).

### 2.5 Decimal literal → uint256 fixed-point at compile time

Currently `__l4_rat_parse("0.1")` is a host call. For EVM the compiler
parses the decimal text into `(numerator, denominator)` BigInt at compile
time, then emits `mul(numerator, exp(10, 18 - denominatorPower))` as a
constant. Result: a single push of a precomputed uint256 constant.

### 2.6 Storage layout assignment

Walk every `@storage`-marked DECLARE in topological order:

- A struct gets one slot per field (32 bytes each in Solidity's
  unpacked layout). Optional optimisation: pack `bool`/`uint8`/`uint16`
  fields into a single slot in declaration order.
- A `mapping(K => V)` declaration uses a single slot for the mapping's
  base; the actual storage location of `m[k]` is `keccak256(k . slot)`.
  Emit Yul `sload`/`sstore` against that computed slot.
- Top-level scalars (e.g., `totalSupply IS A NUMBER`) get one slot each.

The layout becomes a build-time constant table; the lowering pass reads it
when emitting any storage access.

### 2.7 Memory model

Yul has three memory regions:

- **Storage** (`sload`/`sstore`) — persistent, ~5,000–20,000 gas per write.
- **Memory** (`mload`/`mstore`) — transient, allocated linearly via a free
  pointer at slot `0x40`. Costs ~3 gas per word, plus quadratic expansion
  cost. Used for return data, event payload assembly.
- **Calldata** — read-only, cheapest to access. Function arguments live
  here on entry.

L4's current "everything is a pointer in linear memory" model maps to
memory for transient computation. Storage records use the Phase 2.6 layout
directly; we never copy them into memory unless reading multiple fields.

### 2.8 Local variable allocation

Yul has function-scoped `let` bindings. Lower L4 WHERE/LET bindings to
`let` directly. The current closure-conversion path on WASH (recursive
zero-arg bindings get lambda-lifted to functions) translates: Yul has
internal functions with explicit free-variable params.

---

## Phase 3 — Deontic semantics for EVM

### 3.1 The mental model shift

On WASM, deontic L4 is interpreted: an event log gets replayed against the
contract structure and produces `FULFILLED` / `BREACH` / residual
obligation. The contract itself doesn't _do_ anything — the simulator
_observes_ whether it would have been fulfilled.

On EVM, the contract _is_ the enforcer. There is no observer. `PARTY x
MUST transfer amount TO y` means: the function that calls `transfer`
actually moves balance from `x` to `y`. The event log isn't replayed; it's
_emitted as it happens_.

This is a fundamental reorientation. The JS deontic interpreter doesn't
get ported — it's irrelevant on EVM. The contract structure gets compiled
directly into state-mutation code.

### 3.2 Built-in `Transfer` action

`PARTY from MUST transfer amount TO to` lowers to:

```yul
function l4_transfer(from, to, amount) {
    let bal := sload(balances_slot(from))
    if lt(bal, amount) { revert(0, 0) }   // "insufficient balance"
    sstore(balances_slot(from), sub(bal, amount))
    sstore(balances_slot(to), add(sload(balances_slot(to)), amount))
    // emit Transfer(from, to, amount)
    mstore(0, amount)
    log3(0, 32, TRANSFER_TOPIC, from, to)
}
```

`balances_slot(addr)` is the `keccak256(addr . balances_base_slot)`
computation from Phase 2.6.

### 3.3 Built-in `Approval` action

`PARTY owner MAY approve amount TO spender` →
`sstore(allowance_slot(owner, spender), amount); emit Approval(…)`.

### 3.4 Built-in `transferFrom` pattern

`PARTY spender MUST transferFrom (FROM owner) (AMOUNT amount) TO recipient` —
the three-party action ERC-20 uses for delegated transfers. Decrements
allowance, moves balance, emits `Transfer`.

### 3.5 `BREACH BY p BECAUSE "reason"` → `revert "reason"`

EVM has `revert` with optional reason bytes. The reason string gets
encoded as `Error(string)` ABI per Solidity convention (selector
`0x08c379a0`, then ABI-encoded string).

### 3.6 `WITHIN deadline` → `require(block.timestamp <= deadline)`

Maps L4's deadline semantics to EVM's block-timestamp checks. Time
arithmetic is integer arithmetic on seconds. The `__l4_today_serial`
intrinsic also points here.

### 3.7 `HENCE` / `LEST` as sequential / failure paths

`HENCE` is "the obligation was met; here's the next step" — translates to
sequential Yul code after the action.

`LEST` is "the obligation was not met; here's the fallback obligation" —
translates to a check after the action that runs the fallback if the
condition failed. In Solidity terms it's a `try` block, but Yul doesn't
have `try`, so we lower to an explicit conditional.

### 3.8 No event replay, no `EVALTRACE`, no trace=full

The deontic interpreter's "given this event log, what's the residual
obligation?" question doesn't apply on EVM. The state itself is the
residual. Reasoning trees (M5/M6) stay WASM-only.

---

## Phase 4 — ERC-20 surface area

### 4.1 Standard interface scaffold

Whether or not the L4 source explicitly declares them, every ERC-20
contract emits these six functions:

- `function totalSupply() external view returns (uint256)`
- `function balanceOf(address) external view returns (uint256)`
- `function transfer(address, uint256) external returns (bool)`
- `function approve(address, uint256) external returns (bool)`
- `function transferFrom(address, address, uint256) external returns (bool)`
- `function allowance(address, address) external view returns (uint256)`

The compiler generates these from the `@storage` decls and the deontic
actions in Phase 3. The user writes L4 like:

```l4
@erc20
@name "MyToken"
@symbol "MYT"
@decimals 18

@storage
DECLARE Balances HAS
    `holder` IS A ADDRESS
    `amount` IS A NUMBER

@storage
DECLARE Allowances HAS
    `owner`   IS A ADDRESS
    `spender` IS A ADDRESS
    `amount`  IS A NUMBER

@storage
DECLARE Supply HAS
    `total` IS A NUMBER

@export Initial mint
@init
GIVEN initialSupply IS A NUMBER
PARTY caller MUST mint initialSupply
```

And gets a working ERC-20 contract out the other end.

### 4.2 Required events

`Transfer(address indexed, address indexed, uint256)` — topic-0 hash:
`0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef`.

`Approval(address indexed, address indexed, uint256)` — topic-0 hash:
`0x8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925`.

Both are computed once at compile time and baked in as constants.

### 4.3 Token metadata (`name`, `symbol`, `decimals`)

Compile-time constants from `@name`, `@symbol`, `@decimals` annotations.
Emitted as three additional `view` external functions returning string,
string, uint8 respectively.

### 4.4 Constructor + initial state

EVM contracts have a one-shot constructor. Map a designated `@init` L4
function to it. Typical ERC-20: mint the initial supply to the deployer
(`msg.sender`).

The constructor takes calldata, runs the L4 `@init` function once,
deploys the runtime bytecode (everything else) as the contract code.

### 4.5 Reentrancy posture

The minimum ERC-20 doesn't make external calls, so reentrancy isn't a
worry yet. **Defer** the receiver-hook variants (ERC-777, ERC-1363) until
explicitly requested — they add cross-contract call complexity, which
needs a coherent story for L4's deontic model in the presence of failed
external calls.

---

## Phase 5 — What does NOT make the cut (and why)

Explicitly out of scope for v1. Listed here so they don't accidentally
become someone's task.

| Construct                                                    | Status | Why                                     |
| ------------------------------------------------------------ | ------ | --------------------------------------- |
| `__l4_sqrt/_sin/_cos/_tan/_ln/_log10/_pow`                   | refuse | No EVM math lib; userland adds 5KB+     |
| `__l4_json_encode/_decode`                                   | refuse | EVM doesn't speak JSON; calldata is ABI |
| `__l4_to_upper/_lower/_trim/_replace`                        | refuse | String mutation is prohibitive on EVM   |
| `__l4_contains/_starts_with/_ends_with/_index_of/_substring` | warn   | Possible but rarely worth gas           |
| JS deontic event-replay                                      | drop   | EVM state is the log's effect           |
| `trace=full` reasoning tree                                  | drop   | No observer on-chain                    |
| `prettyL4Value` / Aeson envelope                             | drop   | Wire format is ABI return data          |
| Floating-point math anywhere                                 | refuse | No f64 on EVM                           |
| Receiver-hook ERC-20 variants (ERC-777, ERC-1363)            | defer  | Reentrancy story unclear                |

Refusal means: the compiler detects the construct in a function targeted
at the EVM backend and emits a fail-loud compile error explaining the
incompatibility. The same `.l4` source can still compile to WASM via the
existing backend — only the EVM target rejects.

---

## Phase 6 — Testing and parity

### 6.1 Pipeline: L4 → Yul → solc → bytecode

Add `jl4-mlir solidity <file>` analogous to `jl4-mlir wasm <file>`. Output
is a `.yul` file + a `.json` ABI descriptor. Optionally chain through
`solc --strict-assembly --bin` to produce the runtime bytecode.

### 6.2 Local EVM execution harness

Choose one:

- **Foundry's `forge`** — a Rust EVM, fast, has cheatcodes. The DeFi
  industry standard. Recommended.
- **`revm` directly** — Rust EVM crate, more bare-metal.
- **Hardhat / ganache** — JS-based, slower, more dependencies.

For each fixture, deploy the compiled bytecode and run a scripted
sequence of calls (`transfer 100`, `approve 50`, `transferFrom`, etc.),
asserting balances and event topic-0/data after each step.

### 6.3 Reference parity

For each ERC-20-shaped L4 fixture, hand-write the equivalent Solidity
contract (or use OpenZeppelin's `ERC20`). Deploy both, run the same
sequence, assert:

- All `Transfer` and `Approval` event logs match exactly (topics + data).
- Final storage state (balances, allowances, totalSupply) matches.
- Reverts happen at the same call sites with the same reason strings.

This is the EVM equivalent of the current 56/56 WASM parity matrix
against `jl4-service`.

### 6.4 Gas regression tracking

Each fixture records gas used per call. Track these across compiler
changes — a 2× gas regression on `transfer` is a serious issue, even if
correctness is preserved.

### 6.5 Audit trail

Smart contracts are immutable once deployed. Before any "ship to mainnet"
claim:

- Reference implementations cross-checked against multiple ERC-20
  implementations (OpenZeppelin, Solady, the EIP-20 examples).
- Reentrancy patterns reviewed even though v1 has no external calls.
- Storage layout collision testing (running the same `.l4` through
  twice should produce identical bytecode and storage slots).

---

## Critical-path summary

The unblocking sequence:

```
0.1 (Yul target)
  └─ 0.2 (uint256 numerics)
       └─ 1.1, 1.2 (ADDRESS, BYTES32)
            └─ 1.3 (storage annotations)
                 └─ 2.6 (storage layout)
                      └─ 3.2 (Transfer action)
                           └─ 4.1 (ERC-20 scaffold)
                                └─ 6.2 (forge harness)
                                     └─ 6.3 (parity vs OZ)
```

Everything else can land in parallel once 2.6 (storage layout) is settled.

## Quarter-scale milestones

- **Q1 (weeks 1–4):** Phases 0, 1, 2. Output: L4 file with a single
  storage-mapping decl + one pure function compiles to deployable Yul.
- **Q2 (weeks 5–8):** Phase 3. Output: first working `transfer` action
  with Transfer event emission; balances move; reverts on insufficient
  funds.
- **Q3 (weeks 9–12):** Phase 4. Output: full ERC-20 scaffold from
  annotated L4 source; passes OpenZeppelin reference parity (6.3).
- **Q4 (weeks 13–16):** Phase 5 hardening + audit prep. Output: a
  fixture corpus, gas baselines, and a documented "what we refuse and
  why" surface area.

## Open questions to settle before starting

1. **Naming.** What does the user-facing CLI look like — `jl4-mlir
solidity`, `jl4-mlir evm`, a separate `jl4-erc20`? Implications for
   build matrix.
2. **Decimal scale flexibility.** ERC-20 lets each token pick its own
   `decimals` (most use 18, some use 6 for USDC-style). Do we fix the
   compile-time scale at 1e18, or make it `@decimals`-driven?
3. **Multi-contract source files.** ERC-20 has one main contract. Does
   L4 source compile to one contract per file, or one per `@erc20`
   annotation?
4. **Upgradability.** Proxy patterns (UUPS, Beacon, Transparent) are out
   of v1 scope, but the storage layout decisions made now will lock or
   unlock those later.
5. **Verification story.** How do users verify the deployed bytecode
   matches the L4 source? Probably: publish the `.l4` source + the
   compiler version + the Yul intermediate, let Etherscan-style verifiers
   reproduce the bytecode.

These are decisions, not detail — surface them before Phase 1 lands.
