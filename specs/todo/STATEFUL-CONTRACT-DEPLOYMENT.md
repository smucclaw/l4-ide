# Stateful Contract Deployment for jl4-service

Architecture spec for turning jl4-service from a stateless DEONTIC calculator into a stateful contract execution platform with auto-deployed projection APIs, webhooks, and live wall-clock semantics.

**Audience:** an implementer with a working understanding of L4 (the `DEONTIC` type, `MUST`/`MAY`/`SHANT`, `WITHIN`, `HENCE`/`LEST`, the residual obligation model) and standard backend concepts (HTTP APIs, Postgres, webhooks).

**Background reading:** [doc/concepts/legal-modeling/regulative-rules.md](../../doc/concepts/legal-modeling/regulative-rules.md) for the conceptual model. The mental model section is the load-bearing prerequisite.

---

## 1. Problem statement

The current jl4-service exposes every L4 export — including those returning `DEONTIC` — as a stateless RPC. Calling a regulative rule means POSTing a `startTime` plus the _entire_ event history; the server replays everything from scratch and returns the residual as raw JSON.

Three concrete failure modes:

1. **Callers must know the L4 actor/action vocabulary** before they can construct a request. There's no schema endpoint that enumerates valid parties or action shapes, so AI tools and third-party integrations need a preflight call and a parser for L4's typed declarations.
2. **Callers must persist their own history.** Every call replays from `t=0`. Long-running contracts grow expensive linearly, and clients become the system of record for legal state.
3. **Responses are L4 residuals.** Consumers either have to learn to read L4 or implement their own walker over the residual tree to figure out "what does Alice currently owe?".

This spec defines a service that takes responsibility for state, exposes a small set of auto-generated projection APIs, and behaves like a real-time wall-clock system when the contract declares a timezone.

---

## 2. Core architectural shift

The deployment becomes a **per-actor contract state machine**:

```
Deployment (an L4 bundle)
  └── one or more DEONTIC exports (rules)
        └── many auto-created contract instances (one per scoping key)
              ├── current residual obligation   ← the canonical state
              ├── append-only event log         ← for audit & history
              └── derived projections           ← obligations, permissions, etc.
```

Two semantic anchors that drive the whole design:

- **The residual `DEONTIC` value is the state.** L4 already returns a residual on every evaluation. Persist it, advance it event by event, and you have an event-sourced engine with O(1) resumption — no replay needed for routine reads.
- **Events are the input alphabet.** A `(party, action, timestamp)` triple is the only thing that moves a contract forward. The deployment exposes one endpoint that accepts events and routes them to the matching instance(s).

Everything else — projections, what-if simulation, deadline timers, webhooks — is derived from those two anchors.

---

## 3. Language prerequisites

These items must land in L4 before (or concurrently with) the deployment work. Skip them and the deployment layer either can't be built cleanly or will leak abstractions to its users.

### 3.1 Resumable residual (blocker)

`EVALTRACE` (or an equivalent runtime entry point) must accept a _residual_ `DEONTIC` value as input and continue from it, taking only the new events. Verify two properties end-to-end:

- Feeding a residual + new events produces the same result as a full replay from the original rule + all events combined.
- Residuals round-trip through serialization: `serialize(residual) → store → deserialize → resume` yields a value the evaluator can drive forward correctly.

The residual currently carries a captured environment of references. If those references prevent serialization, fix one of:

- prune the environment to variables actually reachable from the live residual sub-tree, or
- use a stable binary serialization (CBOR) that preserves the references intact across processes.

If neither is feasible, the deployment falls back to event-sourced replay with periodic snapshots — workable but caps deployment scale.

### 3.2 Duration units (blocker under wall-clock mode)

When a bundle declares `TIMEZONE IS …` the numeric timeline is reinterpreted as Unix seconds (see §5). Authors should be able to write durations in human units; the compiler resolves them to seconds:

```
WITHIN 14 days
WITHIN 5 hours
WITHIN 30 minutes
WITHIN 90 seconds
```

**A bare numeric `WITHIN 14` under wall-clock mode is interpreted as 14 _days_.** Legal text almost always means days; that is the humane default and matches what an author reading a contract would expect. Treating a bare number as seconds (because the underlying timeline is Unix seconds) would be a footgun. Treating it as a type error would be unnecessarily strict given how strong the "days" convention is in legal drafting.

So:

- `WITHIN n` (no unit) → n days
- `WITHIN n days|hours|minutes|seconds` → explicit unit
- `WITHIN n` in **abstract mode** (no `TIMEZONE IS`) → n abstract ticks, exactly as today

DATE / TIME values combined under `TIMEZONE IS` resolve to DATETIME (a UTC instant, i.e., a Unix timestamp). This resolution happens at compile/elaboration time, not at every evaluation, so the runtime always sees a number.

### 3.3 Instance identity

No new annotation is needed. **An instance _is_ a residual; its identity is the tuple of GIVEN values used to invoke the rule.** Every PARTY appearing in the residual — whether hardcoded in the L4 source or filled in from a GIVEN — is an actor the deployment tracks for event routing.

How instances arise, derived directly from the rule's signature:

| Rule signature                                       | Instance creation                                                                                                 | Actors tracked                                                                                        |
| ---------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------- |
| 0 GIVENs                                             | Singleton; instantiated at deployment load.                                                                       | Every PARTY appearing in the residual (all hardcoded).                                                |
| Exactly 1 GIVEN of an actor type                     | Auto-created on the first event whose `party` matches that type. The GIVEN is filled in from the event.           | Every PARTY in the residual (the GIVEN-bound actor plus any others introduced via HENCE/LEST chains). |
| Anything else (multiple GIVENs, or non-actor GIVENs) | Created by an explicit `POST /v1/instances` call that supplies all GIVEN values. Events alone cannot auto-create. | Every PARTY in the residual.                                                                          |

Event routing is the same in every case: an incoming event with `party = X` is applied to every existing instance where `X` is bound to a currently-live PARTY clause. If `X` doesn't appear in any existing instance, the auto-create rule above decides whether one is created or the event is reported as unmatched.

The third row is the only place an explicit step survives, and it survives because it's semantically required — a multi-party agreement cannot be deduced from a single event. That step is an API call (`POST /v1/instances` with `{rule, arguments}`), not a language annotation.

### 3.4 Deployment trigger

No `@contract` annotation is introduced. **An `@export`ed function whose return type is `DEONTIC …` is automatically a stateful contract** and is surfaced through the auto-deployed rule set in §6. Other `@export`ed functions remain stateless RPCs, exactly as today. The service already detects DEONTIC return types in `McpServer.hs`; that detection is the deployment trigger.

### 3.5 Other items (important but not blocking)

- **`@on_redeploy <policy>` annotation.** A new bundle/per-rule annotation declaring how live instances should be treated when the rule is redeployed. Two values cover the cases that matter at MVP: `prospective` (default; grandfather live instances) and `migrate_now` (replay live instances against the new version). Semantics in §4.
- **Action → JSON Schema derivation.** Schema generation for `DECLARE Action IS ONE OF …` should produce a discriminated union (`oneOf` with a discriminator) directly. For `PROVIDED` guards in a recognized shape (`>=`, `==`, `IN`), emit the corresponding JSON-Schema constraint; otherwise document the guard as opaque.
- **Structured breach/fulfilment reasons in serialized residual.** `DeadlineMissed` and `ExplicitBreach` already carry forensic detail; ensure it survives JSON serialization so webhooks can report _why_ a transition happened.

---

## 4. Versioning and amendments

### 4.1 Core principle

A live contract instance is bound to the version of the rule that created it. **Redeploying a bundle does not change live contracts.** New instances use the new version; existing instances continue under their pinned version until they terminate.

This is the legal default for amendments — statutes commenced after a contract is signed govern future contracts, not the one already in force — and the only safe automatic policy in a system where the deployment is also the legal artifact.

### 4.2 Rule identity

Every `@export`ed function has a stable identity derived from its compiled form and the transitive closure of types and helpers it depends on:

```
version_hash = hash( compiled(rule) ⊕ compiled(transitive deps) )
```

Reformatting source doesn't change the identity. Renaming a helper a rule actually uses does. This makes "did this rule actually change?" answerable without ambiguity, and lets most redeploys recognise that most rules are untouched.

### 4.3 Classification on every redeploy

For each `@export`ed rule, the redeploy is classified before commit:

| Class          | Trigger                                             | What happens to live instances                                                                                                                                      |
| -------------- | --------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **No-op**      | Hash unchanged                                      | Roll forward; nothing to do.                                                                                                                                        |
| **Compatible** | Hash changed, signature unchanged                   | Stay pinned to old version unless the rule declares `@on_redeploy migrate_now`. New instances use the new version.                                                  |
| **Breaking**   | Signature changed (GIVENs, actor type, action type) | **Deploy refuses unless `@on_redeploy migrate_now` is declared with a supplied migration function.** Old residuals are no longer well-typed against the new source. |
| **New**        | No previous version                                 | First version registered.                                                                                                                                           |
| **Removed**    | Previous version existed; no current export         | Old version retained while pinned instances exist. No new instances can be created for the rule.                                                                    |

The classifier produces a report the operator sees before commit:

```
saleContract       COMPATIBLE   v1 (142 live)   →  v2 (new instances)
ndaPolicy          NO-OP        v3
leaseAgreement     BREAKING     v1 (7 live)     →  REFUSED: needs @on_redeploy migrate_now
employmentOffer    NEW          v1
oldExperimentRule  REMOVED      v2 (3 live retained on v2)
```

### 4.4 Author intent: `@on_redeploy`

Migration policy lives in L4 source, per rule (or per bundle as a default):

```l4
@on_redeploy prospective    -- default; can be omitted
@on_redeploy migrate_now    -- replay live instances against the new version on commit
```

Two policies, intentionally. `prospective` is the legal default and applies if no annotation is present. `migrate_now` is what an author writes when a bug fix should reach running contracts — and is the only declaration that unblocks a Breaking-classified deploy.

When `migrate_now` is set on a Breaking change, the author must supply a migration function with a known signature (`OldDEONTIC → NewDEONTIC`) that translates persisted residuals to the new shape. Compatible changes need no migration function; the new rule consumes the old residual directly.

Additional policies (scheduled commencement, lazy migration, sunrise dates) can be added later if real use cases demand them. Two is enough at MVP.

### 4.5 Storage shape

Two additions to the schema in §8.2:

```sql
CREATE TABLE rule_versions (
  deployment_id   text NOT NULL,
  rule_name       text NOT NULL,
  version_hash    text NOT NULL,
  compiled_rule   bytea NOT NULL,
  policy          text NOT NULL DEFAULT 'prospective',
  registered_at   timestamptz NOT NULL DEFAULT now(),
  superseded_at   timestamptz,
  PRIMARY KEY (deployment_id, rule_name, version_hash)
);

ALTER TABLE instances
  ADD COLUMN version_hash text NOT NULL,
  ADD CONSTRAINT instances_version_fk
    FOREIGN KEY (deployment_id, rule_name, version_hash)
    REFERENCES rule_versions(deployment_id, rule_name, version_hash);
```

In steady state, one current row per rule. During transition, multiple versions coexist while older-pinned instances are still alive. A version row is garbage-collectable once no instance references it.

Migration audit entries are written into the existing `events` table as a synthetic event type — no separate table needed at MVP.

### 4.6 Why this is a good Stage 1

Versioning has independent value before any DEONTIC machinery exists. Even for stateless `@export`s:

- Every deployed rule gets a stable, content-addressed identity. Callers can detect signature drift programmatically.
- Operators get a real redeploy report instead of a black-box "we updated the bundle."
- Breaking-change refusal is a CI gate from day one, preventing silent signature breaks.
- The audit log starts accumulating immediately, so when stateful machinery arrives, legal-grade traceability is already in place.

The later stateful machinery just _uses_ what's already there: the `version_hash` to pin instances, the classifier to gate migrations, the audit log to record what changed.

---

## 5. Time semantics

The deployment runs in one of two modes, selected by whether the bundle declares `TIMEZONE IS …`.

### 4.1 Abstract mode (no `TIMEZONE IS`)

Today's behaviour. The numeric timeline is whatever the author says it is — ticks, days, abstract turns. All `at` parameters are required on event submission and ticks. Used for tests, simulation, and contracts whose semantics are unit-agnostic. The deployment layer does no auto-ticking; callers drive the clock explicitly.

### 4.2 Wall-clock mode (with `TIMEZONE IS`)

The numeric timeline is **Unix seconds (UTC)**. The declared timezone is for display and for resolving local-date references (`TODAY`, `BEFORE Friday`), not for the numeric timeline itself. Three things become implicit:

1. **`at` defaults to server's `now()`** on every endpoint that accepts it (`submit_event`, `tick`, `what_if`, etc.).
2. **Every read auto-ticks the actor's residual to `now()`** before computing the answer. A `MUST … WITHIN 14 days` transitions to BREACHED at the exact instant the deadline passes, the moment anyone reads.
3. **Webhooks fire on transitions detected during auto-tick**, not only on explicit `submit_event` calls. The "someone read the dashboard and that's when we noticed" path is the same path as "we got an event".

Auto-tick is monotone (the residual only moves forward) and idempotent (ticking past `now` twice produces the same residual). It can be done as part of the read transaction without violating GET-is-pure expectations at the consumer level.

For push-based consumers that need transitions delivered eagerly without a reader, see §8 (deadline scheduler).

**Authority:** server `now()` is always authoritative. Callers may pass an explicit historical `at` for backfill, but may not redefine "now" to backdate events.

---

## 6. The auto-deployed rule set

When the bundle contains at least one DEONTIC-returning `@export`, the deployment publishes the following endpoints. The same flat set is published once per deployment regardless of how many DEONTIC-returning `@export`s exist; each endpoint takes an optional `rule` parameter.

**Semantic rule (consistent across all endpoints):**

> If `rule` is omitted, the operation applies across every DEONTIC-returning `@export` in the deployment, silently skipping any where the supplied actor or event would not typecheck. Type-mismatches are filtered, not errored.

**Response envelope (consistent across all endpoints):**

```json
{
  "results": [{ "rule": "...", "...payload": "..." }],
  "skipped": [{ "rule": "...", "reason": "type-mismatch" }],
  "unmatched": false
}
```

With `rule` supplied, `results` has length 1.

### 5.1 Discovery (bundle-level, no actor)

| #   | Endpoint            | Inputs            | Returns                                                              |
| --- | ------------------- | ----------------- | -------------------------------------------------------------------- |
| 1   | `list_contracts`    | —                 | `[{rule, actorType, actionType, instanceKey, hasDefaultStartAt}]`    |
| 2   | `describe_contract` | `rule?`           | actor type, action union, default startAt, structural skeleton       |
| 3   | `list_actor_types`  | `rule?`           | grouped: enum members or record fields per actor type                |
| 4   | `list_action_types` | `rule?`           | grouped: discriminated union per action type                         |
| 5   | `contract_skeleton` | `rule?, startAt?` | the initial residual _before any events_ — same shape as `get_state` |

### 5.2 Event ingestion (the log sink)

| #   | Endpoint        | Inputs                                       | Behaviour                                                                                                                                                           |
| --- | --------------- | -------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 6   | `submit_event`  | `rule?, party, action, at?, idempotencyKey?` | Fan-out: applied to every rule whose actor/action types accept the payload. Auto-creates instance(s) keyed per §3.3 if unseen. Returns per-rule transition outcome. |
| 7   | `submit_events` | `rule?, events[]`                            | Each event independently applied as in (6).                                                                                                                         |
| 8   | `tick`          | `rule?, actor, at?`                          | Advances `actor`'s clock in every rule they have state in, via a synthetic `WAIT UNTIL`.                                                                            |
| 9   | `tick_all`      | `rule?, at?`                                 | Same for every tracked actor in every (matching) rule.                                                                                                              |

In wall-clock mode `at` defaults to `now()`. In abstract mode it is mandatory.

### 5.3 State inquiry

| #   | Endpoint         | Inputs           | Returns                                                                |
| --- | ---------------- | ---------------- | ---------------------------------------------------------------------- |
| 10  | `list_instances` | `rule?, status?` | `[{rule, actor, status, startedAt, lastEventAt}]`                      |
| 11  | `get_state`      | `rule?, actor`   | `{status, currentTime, residual, terminalReason?}` per rule            |
| 12  | `get_history`    | `rule?, actor`   | event log, each event tagged with its rule and residual snapshot after |
| 13  | `get_status`     | `rule?, actor`   | cheap version of (11) returning only `PENDING / FULFILLED / BREACHED`  |

### 5.4 Projections (per actor; pure walks of the residual)

| #   | Endpoint                              | Returns (per matching rule)                                             |
| --- | ------------------------------------- | ----------------------------------------------------------------------- |
| 14  | `obligations` (`rule?, actor`)        | live `MUST` clauses binding or bound by `actor`                         |
| 15  | `permissions` (`rule?, actor`)        | live `MAY` clauses                                                      |
| 16  | `prohibitions` (`rule?, actor`)       | live `SHANT` clauses                                                    |
| 17  | `next_deadline` (`rule?, actor`)      | soonest deadline anywhere in this actor's residuals                     |
| 18  | `allowed_events` (`rule?, actor`)     | every `(party, action)` shape that would advance the residual right now |
| 19  | `discharging_events` (`rule?, actor`) | subset of (18) that would lead to `FULFILLED`                           |
| 20  | `breaching_events` (`rule?, actor`)   | subset of (18) that would lead to `BREACHED`                            |
| 21  | `parties_in_scope` (`rule?, actor`)   | every party bound somewhere in this actor's residuals                   |

Projections are pure walks of the residual `Deonton` tree: gather leaves, bucket by modal, attach deadlines and continuations. No event replay; microsecond responses.

### 5.5 Simulation (pure; never mutates)

| #   | Endpoint                                      | Returns                                            |
| --- | --------------------------------------------- | -------------------------------------------------- |
| 22  | `what_if` (`rule?, actor, event`)             | resulting residual & transition; nothing persisted |
| 23  | `what_if_sequence` (`rule?, actor, events[]`) | same, multi-event                                  |
| 24  | `what_if_tick` (`rule?, actor, at`)           | what happens at time `at` with no further events   |

Implementation: load the actor's persisted history, run the evaluator with the hypothetical events appended, return the result, don't write.

### 5.6 Admin

| #   | Endpoint           | Inputs                          | Notes                                                                  |
| --- | ------------------ | ------------------------------- | ---------------------------------------------------------------------- |
| 25  | `reset_instance`   | `rule?, actor, allRules?: true` | Requires `allRules: true` when `rule` is omitted, as a footgun guard.  |
| 26  | `archive_instance` | `rule?, actor, allRules?: true` | Marks completed; keeps history readable.                               |
| 27  | `replay_instance`  | `rule?, actor, asOf?`           | Rebuild residual from stored history up to `asOf`. Recovery/migration. |

---

## 7. Webhook integration

### 6.1 Where config lives

In the deployment record, not the L4 source. Webhook URLs and secrets are environment-specific and rotate independently of code.

```json
{
  "deploymentId": "hr-policies-v3",
  "bundle": "…",
  "integrations": {
    "webhooks": [
      {
        "id": "wh_01",
        "url": "https://hr.example.com/hooks/l4",
        "events": ["state.changed"],
        "rules": null, // null = all
        "secret": "whsec_…",
        "active": true
      }
    ]
  }
}
```

Configure via PUT/PATCH on the deployment. No new top-level resource; surface a sub-routes group for ergonomics:

| Endpoint                                           | Purpose                                       |
| -------------------------------------------------- | --------------------------------------------- |
| `GET /deployments/{id}/webhooks`                   | list                                          |
| `POST /deployments/{id}/webhooks`                  | register                                      |
| `PATCH /deployments/{id}/webhooks/{whid}`          | edit                                          |
| `DELETE /deployments/{id}/webhooks/{whid}`         | remove                                        |
| `POST /deployments/{id}/webhooks/{whid}/test`      | synthetic event for integration testing       |
| `GET /deployments/{id}/webhooks/{whid}/deliveries` | recent attempts with status, response, timing |

### 6.2 Event taxonomy

Start with the smallest set that distinguishes the cases consumers actually react to:

| Event type           | Fires when                                                                                   |
| -------------------- | -------------------------------------------------------------------------------------------- |
| `state.changed`      | The firehose — any residual movement                                                         |
| `state.fulfilled`    | Terminal `FULFILLED`                                                                         |
| `state.breached`     | Terminal `BREACHED`                                                                          |
| `obligation.created` | A HENCE chain advanced and a fresh `MUST` is now live                                        |
| `deadline.passed`    | A `WITHIN` expired (usually coincides with `fulfilled` for SHANT/MAY or `breached` for MUST) |
| `actor.first_seen`   | An event introduced a new actor; instance auto-created                                       |
| `event.unmatched`    | An incoming event didn't typecheck against any rule                                          |

Webhooks may filter by event type, by rule, or both.

### 6.3 Payload

Self-contained — the consumer never needs a round-trip to fetch state:

```json
{
  "delivery":   "dlv_01H…",
  "deployment": "hr-policies-v3",
  "event":      "state.breached",
  "occurredAt": 1764064800,
  "rule":       "ndaContract",
  "actor":      "Alice",
  "previousStatus": "PENDING",
  "currentStatus":  "BREACHED",
  "trigger": {
    "kind": "deadline_passed",   // | "event_applied" | "tick"
    "event": { "party": "...", "action": { … }, "at": 1764064793 } | null,
    "deadlineAt": 1764064795 | null,
    "breachReason": "missed disclosure window"
  },
  "state": {
    "residual":     { … },
    "obligations":  [ … ],
    "permissions":  [ … ],
    "prohibitions": [ … ],
    "nextDeadline": { … } | null
  }
}
```

Sign the raw body with HMAC-SHA256 using the webhook's `secret`; put the result in `X-L4-Signature`. Standard pattern (Stripe/GitHub style).

### 6.4 Delivery guarantees

| Property    | Choice                                  | Notes                           |
| ----------- | --------------------------------------- | ------------------------------- |
| Delivery    | At-least-once                           | With exponential backoff        |
| Retries     | 5s, 30s, 5min, 30min, 1h, 6h, give up   | Configurable later              |
| Idempotency | `delivery` id stable across retries     | Consumer dedupes                |
| Signing     | HMAC-SHA256, mandatory                  | Consumer verifies before acting |
| Dead-letter | Failed deliveries queryable for 30 days | Operator can manually replay    |

Don't bother with exactly-once or ordered delivery. Consumers dedupe on `delivery` id.

---

## 8. State storage

### 7.1 Technology choice

**AWS RDS Postgres 16, Multi-AZ.** No reason to reach further. Workload is small-payload, transactional, query-shaped data — Postgres handles years before anything else is needed. Avoid bringing in DynamoDB, EventStoreDB, or Kafka for a workload that fits in Postgres.

### 7.2 Schema

```sql
CREATE TABLE deployments (
  id            text PRIMARY KEY,
  bundle_hash   text NOT NULL,
  integrations  jsonb NOT NULL DEFAULT '{}'::jsonb,
  created_at    timestamptz NOT NULL DEFAULT now(),
  updated_at    timestamptz NOT NULL DEFAULT now()
);

CREATE TABLE instances (
  id              uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  deployment_id   text NOT NULL REFERENCES deployments(id) ON DELETE CASCADE,
  rule_name       text NOT NULL,
  instance_key    jsonb NOT NULL,        -- the tuple of GIVEN argument values
  status          text NOT NULL,         -- 'PENDING' | 'FULFILLED' | 'BREACHED'
  current_time    bigint NOT NULL,       -- Unix seconds (wall-clock) or abstract tick
  next_deadline   bigint,                -- nullable; used by scheduler
  residual        jsonb NOT NULL,        -- the serialized DEONTIC residual
  created_at      timestamptz NOT NULL DEFAULT now(),
  updated_at      timestamptz NOT NULL DEFAULT now(),
  version         bigint NOT NULL DEFAULT 0  -- optimistic concurrency
);

CREATE UNIQUE INDEX instances_lookup
  ON instances (deployment_id, rule_name, instance_key);

CREATE INDEX instances_due
  ON instances (next_deadline)
  WHERE status = 'PENDING' AND next_deadline IS NOT NULL;

CREATE INDEX instances_by_status
  ON instances (deployment_id, rule_name, status);

CREATE TABLE events (
  id              uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  instance_id     uuid NOT NULL REFERENCES instances(id) ON DELETE CASCADE,
  idempotency_key text,
  party           jsonb NOT NULL,
  action          jsonb NOT NULL,
  at              bigint NOT NULL,
  received_at     timestamptz NOT NULL DEFAULT now(),
  transition      jsonb NOT NULL,        -- previous/current status, cause, deadline, reason
  residual_after  jsonb                  -- optional snapshot (audit / replay)
);

CREATE UNIQUE INDEX events_idempotency
  ON events (instance_id, idempotency_key)
  WHERE idempotency_key IS NOT NULL;

CREATE INDEX events_by_instance_time
  ON events (instance_id, at);

CREATE TABLE webhook_deliveries (
  id              uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  webhook_id      text NOT NULL,
  deployment_id   text NOT NULL,
  instance_id     uuid,
  event_id        uuid,
  payload         jsonb NOT NULL,
  status          text NOT NULL,         -- 'pending' | 'delivered' | 'failed' | 'dead'
  attempts        int  NOT NULL DEFAULT 0,
  next_attempt_at timestamptz,
  last_response   jsonb,
  created_at      timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX deliveries_pending
  ON webhook_deliveries (next_attempt_at)
  WHERE status = 'pending';
```

### 7.3 Hot path

`submit_event` is exactly one transaction:

```
BEGIN;

  -- 1. lock the instance row
  SELECT residual, current_time, version
    FROM instances
    WHERE deployment_id = $1 AND rule_name = $2 AND instance_key = $3
    FOR UPDATE;

  -- 2. evaluate (out of SQL): residual + event → new_residual + transition

  -- 3. append the event
  INSERT INTO events (instance_id, ...) VALUES (...);

  -- 4. update the snapshot with optimistic CAS
  UPDATE instances
     SET residual      = $new_residual,
         status        = $new_status,
         current_time  = $new_time,
         next_deadline = $new_deadline,
         updated_at    = now(),
         version       = version + 1
   WHERE id = $instance_id AND version = $loaded_version;

  -- 5. enqueue webhook deliveries for any transition
  INSERT INTO webhook_deliveries (...)
    SELECT ...
      FROM deployments
     WHERE id = $1;

COMMIT;
```

No replay. One round trip. Fully ACID.

### 7.4 Auto-tick on read

Reads should not block on `FOR UPDATE`. Pattern:

1. Read the instance row.
2. If `current_time < now()`, spawn a short follow-up transaction that runs `step(instance_id, tick=now())`, updates the row, enqueues any resulting webhooks.
3. Return the _fresh_ projection to the caller.

This keeps the read path simple, caps per-request latency, and lets the write happen exactly once per advance (the `FOR UPDATE` in step 4 of the write path serialises concurrent ticks naturally).

### 7.5 Scheduler & webhook delivery

Both are **Postgres-as-queue** workers using `FOR UPDATE SKIP LOCKED`. No SQS or Kafka.

Deadline scheduler (single worker loop):

```sql
SELECT id FROM instances
WHERE status = 'PENDING'
  AND next_deadline IS NOT NULL
  AND next_deadline <= EXTRACT(EPOCH FROM now())::bigint
ORDER BY next_deadline
LIMIT 100
FOR UPDATE SKIP LOCKED;
```

For each id: run `step(instance_id, tick=now())`, update, commit. Multiple workers can run in parallel safely.

Webhook delivery (same shape):

```sql
SELECT id FROM webhook_deliveries
WHERE status = 'pending'
  AND next_attempt_at <= now()
ORDER BY next_attempt_at
LIMIT 100
FOR UPDATE SKIP LOCKED;
```

Attempt the HTTP POST, update `status` / `attempts` / `next_attempt_at`, commit.

This pattern scales to thousands of operations per second on a single worker and remains correct under N parallel workers.

### 7.6 RDS configuration

| Setting        | Value                                        | Why                                                   |
| -------------- | -------------------------------------------- | ----------------------------------------------------- |
| Engine         | Postgres 16                                  | Best JSONB & concurrency                              |
| Instance class | `db.t4g.medium` or `db.r7g.large`            | Plenty for tens of thousands of instances             |
| Storage        | gp3, 100 GB+, 3000 IOPS baseline             | gp3 lets you raise IOPS independently of size         |
| Multi-AZ       | Yes (production)                             | This is your system of record for legal state         |
| Pooling        | RDS Proxy _or_ PgBouncer in transaction mode | Service should never open raw connections per request |
| Backups        | Automated, 30-day retention, PITR enabled    | Long retention for legal artifacts                    |
| Encryption     | At-rest (KMS) + TLS in-transit               | Standard for PII / legal data                         |
| Read replica   | One, when dashboards become a real read path | Writes stay on primary                                |

### 7.7 Performance principles, in order of impact

1. **Keep `instances.residual` bounded.** Prune the captured environment to variables actually reachable from the live sub-tree before each persist. Or switch to CBOR if JSONB grows noisy.
2. **Optimistic CAS, not pessimistic cross-call locks.** Locks live inside the write transaction only.
3. **Auto-tick is its own small transaction**, not bundled into the read handler.
4. **`FOR UPDATE SKIP LOCKED` for both scheduler and webhook workers.** Run N workers freely; never starve.
5. **JSONB GIN index on `instance_key`** only if you start needing "find all instances where `instance_key.id = 'Alice'`" lookups.
6. **Partition `events` by month** once it crosses ~10M rows. Detach old partitions to S3 for cold storage.

### 7.8 What you do NOT need

- A separate event store (EventStoreDB, Kafka). The `events` table _is_ it, with ACID guarantees.
- Redis. The hot row sits in Postgres's shared buffers.
- A separate queue (SQS, RabbitMQ). Postgres-as-queue handles this scale.
- Sharding. Single instance handles millions of contracts and tens of millions of events before this is a question.

---

## 9. End-to-end request lifecycles

### 8.1 `submit_event` (the canonical write)

```
client → POST /v1/events { party, action, at? }
  │
  ├─ validate party against actor types of every DEONTIC-returning rule
  ├─ validate action against action types of every DEONTIC-returning rule
  ├─ for each matching rule:
  │    BEGIN
  │      find-or-create instance: route to every existing instance where `party`
  │        is bound to a live PARTY clause; else auto-create per §3.3 if the rule
  │        has exactly one actor-typed GIVEN, else report event as unmatched
  │      load residual + current_time (FOR UPDATE)
  │      if wall-clock mode: tick to now() first
  │      apply event to residual via evaluator
  │      INSERT INTO events
  │      UPDATE instances ... version+1
  │      INSERT INTO webhook_deliveries (for each transition kind & matching webhook)
  │    COMMIT
  ├─ collect per-rule outcomes
  └─ respond { results, skipped, unmatched }
```

### 8.2 `obligations` (the canonical read)

```
client → GET /v1/projections/obligations?actor=Alice
  │
  ├─ for each DEONTIC-returning rule:
  │    look up instance(s) where Alice is bound
  │    if wall-clock mode and current_time < now():
  │      run advance-to-now in a short transaction (may emit webhooks)
  │    walk residual, collect MUST nodes binding Alice
  ├─ collect per-rule results
  └─ respond { results, skipped, unmatched }
```

### 8.3 Deadline expiry → webhook (no caller involved)

```
scheduler worker ticks:
  SELECT instance ids with next_deadline <= now() FOR UPDATE SKIP LOCKED
  for each:
    BEGIN
      load residual
      apply tick(now())
      UPDATE instances
      if transitioned: INSERT INTO webhook_deliveries
    COMMIT

webhook worker ticks:
  SELECT pending deliveries FOR UPDATE SKIP LOCKED
  for each:
    POST payload to webhook.url with X-L4-Signature
    UPDATE delivery row with status / attempts / next_attempt_at
```

---

## 10. Iteration plan

Ship in stages. Each stage is independently useful; you can pause at any boundary without leaving half-features in the codebase.

| Stage | Scope                               | Deliverable                                                                                                      | Effort                                    |
| ----- | ----------------------------------- | ---------------------------------------------------------------------------------------------------------------- | ----------------------------------------- |
| **0** | L4 prerequisites (§3.1, §3.2, §3.5) | `EVALTRACE` resumes from residual; duration units; `@on_redeploy` annotation; action schema generation           | language work                             |
| **1** | Versioning + redeploy report (§4)   | Content hash per `@export`; `rule_versions` table; classifier; deploy report; breaking-change refusal; audit log | huge value, even before DEONTIC machinery |
| **2** | Discovery + better output           | Endpoints 1–5, schema-derived MCP tool inputs; still stateless                                                   | huge value, small code                    |
| **3** | Projections (stateless)             | Endpoints 14–21 operating on a residual passed in by the caller (no storage yet)                                 | huge value, small code                    |
| **4** | Storage + contract instances        | Postgres schema; endpoints 6, 10–13, 27; instances pinned to `version_hash`                                      | medium                                    |
| **5** | Auto-tick + simulation              | Wall-clock mode (§5.2); endpoints 8, 9, 22–24                                                                    | small                                     |
| **6** | Webhooks                            | §7 in full                                                                                                       | medium                                    |
| **7** | Deadline scheduler                  | §8.5 worker                                                                                                      | small                                     |
| **8** | Admin                               | Endpoints 25, 26                                                                                                 | small                                     |

Stage 1 alone gives operators a real redeploy report and CI-grade breaking-change refusal for every `@export`, even before any contract is stateful. Stage 2 removes the "callers must know L4 vocabulary" complaint. Stages 2–3 together remove the "callers must read L4 to understand the response" complaint. Stages 4–5 remove the "callers must replay history" complaint. Stages 6–7 enable push integrations.

---

## 11. Open decisions

These are intentionally not pinned in this spec; the implementer should raise them and get a call before committing.

1. **Default behaviour when an action has a PROVIDED guard the schema generator can't statically describe.** Two reasonable choices: emit a permissive schema and let the runtime reject mismatches, or refuse to generate the tool and require the author to simplify the guard. Recommend the permissive default; flag generated tools when fallback was used.

2. **Identity collisions across rules.** If two DEONTIC-returning rules in the same bundle have overlapping actor types (both accept `Person`), is the same actor value `"Alice"` the same Alice in both? Recommend: yes within a deployment, no across deployments. Document explicitly so authors design around it.

3. **Retention of terminated instances.** How long do `FULFILLED` / `BREACHED` instances and their event histories remain query-able? Recommend: indefinitely by default for legal artifacts; offer a soft-delete / archive-to-S3 path.

4. **Authentication & authorization model.** Out of scope for this spec but blocking for production. Per-deployment API tokens at minimum; per-instance ACLs eventually.

---

## 12. References

- [doc/concepts/legal-modeling/regulative-rules.md](../../doc/concepts/legal-modeling/regulative-rules.md) — conceptual model for `DEONTIC`, events, residuals
- [doc/reference/regulative/README.md](../../doc/reference/regulative/README.md) — keyword-level reference (PARTY, MUST, MAY, SHANT, WITHIN, HENCE, LEST, BREACH, BECAUSE, RAND, ROR)
- [doc/reference/regulative/DEONTIC.md](../../doc/reference/regulative/DEONTIC.md) — the DEONTIC type and EVALTRACE signature
- [specs/done/DEONTIC-TRACE-API-SPEC.md](../done/DEONTIC-TRACE-API-SPEC.md) — current stateless trace API
- [specs/done/PROHIBITION-BREACH-SPEC.md](../done/PROHIBITION-BREACH-SPEC.md) — SHANT polarity and the truth table for HENCE/LEST
- [specs/done/JL4-SERVICE-SPEC.md](../done/JL4-SERVICE-SPEC.md) — current jl4-service architecture
- [specs/todo/BOUNDED-DEONTICS-SPEC.md](BOUNDED-DEONTICS-SPEC.md) — philosophical / theoretical foundation
