# Decision Service Backend

Exposes certain pre-compiled JL4 programs for use by an LLM AI, via the Tool Calling / Function Calling API.

The current MVP exposes a couple of example programs which are hardcoded.

In a future iteration the backend will

1. dynamically load the available programs from the `jl4/examples` directory
2. parse and run an L4 program `POST`ed in as part of the API call itself.

## Usage

Run the decision service backend locally from the project root:

```sh
> cabal run jl4-decision-service-exe -- --port 8081 --serverName http://localhost:8081/ --sourcePaths jl4/experiments/britishcitizen5.l4 --sourcePaths jl4/experiments/parking.l4
```

or run it from the sub-directory:

```sh
> cd jl4-decision-service
> cabal run jl4-decision-service-exe -- --port 8081 --serverName http://localhost:8081/ --sourcePaths ../jl4/experiments/britishcitizen5.l4 --sourcePaths ../jl4/experiments/parking.l4
```

Then try executing a few things [swagger-ui at http://localhost:8081/swagger-ui/](http://localhost:8081/swagger-ui/).
There is also a machine-readable [`swagger.json`](http://localhost:8081/swagger.json) representation of the API.

### Endpoints

#### Functions resource

The function resource represents functions in the `jl4-decision-service`.
These functions can be evaluated with user-given arguments.

- `GET    /functions`: Get the function signature of all stored functions
- `GET    /functions/<name>`: Get the full function signature of the function `<name>`
- `PUT    /functions/<name>`: Update the function `<name>`
- `POST   /functions/<name>`: Create a new function `<name>`
- `DELETE /functions/<name>`: Delete function `<name>`

### Evaluation

Function evaluation is provided via the endpoints:

- `POST  /functions/<name>/evaluation`: Evaluate the function `<name>` with given arguments.
- `POST  /functions/<name>/batch`: Evaluate the function `<name>` with an array of arguments.

### Visualizing Evaluation Traces

New to L4? You can now see how your logic flows by asking the decision service for a GraphViz trace. Start with `trace=full` so the engine records every lazy step, then toggle `graphviz=true` to include DOT output plus ready-made image links in the JSON response:

```bash
curl -s \
  'http://localhost:8081/functions/compute_qualifies/evaluation?trace=full&graphviz=true' \
  -H 'Content-Type: application/json' \
  -d '{"fnEvalBackend":"JL4","fnArguments":{"walks": true, "drinks": true, "eats": true}}' \
  | jq '.contents.graphviz'
```

`graphviz` is now an object with three helpful fields:

```jsonc
"graphviz": {
  "dot": "digraph evaluation_trace { ... }",
  "png": "/functions/compute_qualifies/evaluation/trace.png",
  "svg": "/functions/compute_qualifies/evaluation/trace.svg"
}
```

If you prefer pictures, the service can render the same trace as PNG or SVG when GraphViz’ `dot` binary is on your path (e.g. `brew install graphviz` or `apt-get install graphviz`). Use the dedicated image endpoints and pipe the response straight into a file:

```bash
# PNG
curl -s \
  'http://localhost:8081/functions/compute_qualifies/evaluation/trace.png?trace=full' \
  -H 'Content-Type: application/json' \
  -d '{"fnEvalBackend":"JL4","fnArguments":{"walks": true}}' > qualifies.png

# SVG (easy to open in a browser)
curl -s \
  'http://localhost:8081/functions/compute_qualifies/evaluation/trace.svg?trace=full' \
  -H 'Content-Type: application/json' \
  -d '{"fnEvalBackend":"JL4","fnArguments":{"walks": true}}' > qualifies.svg
```

The PNG/SVG endpoints simply rerun the same evaluation on demand, keeping the primary `/evaluation` response pure (no cached blobs to invalidate, no race conditions to juggle). If the image routes ever feel slow you can always fall back to the `dot` text and render locally.

Batch runs get the same treatment. When you add `graphviz=true` to `/functions/<name>/batch?trace=full`, each case in the response includes an `@graphviz` object alongside its usual outputs. That makes it easy to loop over results and stash every trace for later inspection:

```bash
curl -s \
  'http://localhost:8081/functions/compute_qualifies/batch?trace=full&graphviz=true' \
  -H 'Content-Type: application/json' \
  -d '{"outcomes":["result"],"cases":[{"@id":1,"walks":true,"drinks":true,"eats":true}]}' \
  | jq '.cases[0]["@graphviz"].dot'
```

Pro tip: feed the DOT text into `dot -Tsvg -o trace.svg` or tools like `xdot` whenever you want an interactive walkthrough of the evaluation tree.

#### What am I Looking At?

An evaluation trace is a play-by-play of the lazy evaluator—the closest thing we have to “explainable AI” for deterministic, rule-based logic. Each node represents an expression that actually ran, annotated with its final value (or error). Edges connect parents to the sub-expressions they forced, so you can read the graph top-down like a conversation: “we were evaluating `qualifies` → we needed `walks`, `drinks`, `eats` → that led to these conditionals…”. Because L4 is lazy, branches that never execute simply don’t appear—great for spotting short-circuit behavior or skipped `CONSIDER` branches. When the trace exporter adds metadata (expression text, timestamp, active file, result) you can match a dot file back to the exact REPL command or API call that produced it. If you’ve seen function-call graphs or data-flow diagrams before, think of this as a trimmed, execution-order version focused only on the choices the interpreter actually had to make. The resulting diagram is the receipt for every conclusion the engine reached.

#### Example Trace Visualization

Here's what a typical trace looks like—this one shows a scoring calculation with conditionals and pattern matching:

![Example trace visualization](../doc/images/trace-showcase.png)

More examples are available in `doc/images/trace-*.png` (short-circuit evaluation, recursion, CONSIDER branches, etc.).

## Loading L4 Functions

Two functions are hardcoded by default; see [src/Examples.hs](src/Examples.hs) for details.

Other functions can be loaded at start time using the `--sourcePaths` command line option.

The argument to the option is a directory or individual `.l4` files.

**Important**: By default, the decision service only loads L4 files that either:

1. Have a matching `.yaml` sidecar (being deprecated), OR
2. Have explicit `@export` directives in their source

Files without either will be ignored. This prevents implicitly exposing helper functions and ensures only intentionally exported functions are available via the API.

For each `.l4` file, if a matching `.yaml` sidecar exists, it is used as the function declaration. Otherwise, functions are exposed when their leading comment uses the `@export` (or `@export default`) syntax:

```l4
@export default Demo entry point for the API
GIVEN input IS A Number @desc Example numeric argument
GIVETH A Number
demo input MEANS input + 1
```

Parameter descriptions can still use inline `@desc` annotations as shown above. Files with neither a `.yaml` sidecar nor any `@export` decides are ignored (this avoids implicitly exposing helper functions).

Use `@export default …` when you want a module-level “main” function. The decision service exposes every `@export` decide clause at `/functions/{name}`, but when a client loads a module without naming a function (for example by UUID or when uploading raw `.l4` code), the service chooses the decide marked `default`. If no default export exists, it falls back to the client-provided name or the first remaining export.

The `.yaml` file describes the API of the `.l4` file. It mirrors the API of the `POST /functions/<name>`.

The format of the `.yaml` is an object with the following keys:

- `type`: Only allowed value is `function`
- `function`: The definition of the function.
  - `name`: Name of the function.
  - `description`: Description of the function. May be an empty string.
  - `parameters`: An object describing the parameters that can be passed to the function.
    - `type`: Must be `object`
    - `properties`: Description of the parameters of the function.
    - `required`: Lists which parameters are required by the function. Some backends may not support optional parameters.
  - `supportedBackends`: The evaluation backends that can be used for this function. Only allowed value is `jl4`

Multiple `--sourcePaths` can be given.

### Example

Annotation-based metadata is preferred, but the legacy `.yaml` format is still supported for back-compatibility. For the `bignums.l4` program:

```jl4
@export Determine whether the inputs are considered big
GIVEN   x IS A NUMBER
        y IS A NUMBER
DECIDE `numbers are big`
    IF     x GREATER THAN 1000
       AND y GREATER THAN   250
                          *   2
                            + 2
    OR x GREATER THAN 10000
    OR y ^       ^    20000
```

If you cannot yet annotate the source, continue to supply the accompanying `.yaml` file. The YAML describes
the name of the function, and the parameters. Optionally, we can provide a description as well:

```yaml
type: function
function:
  name: "numbers are big"
  description: Given two numbers, determine if the numbers are big. The numbers are considered big if they are both big. But they can also be considered big if either of the numbers is very big.
  parameters:
    type: object
    properties:
      x:
        type: "number"
        description: The first number
      y:
        type: "number"
        description: The second number
    required:
      - "x"
      - "y"
  supportedBackends:
    - "jl4"
```

## Integration with Web IDE and Session Management

The decision service integrates with `jl4-websessions` to support the browser-based playground for L4 programs.

### How Web Sessions Work

When users upload or edit L4 code through the web IDE:

1. **Session Creation**: The `jl4-websessions` service generates a random UUID for each session and stores the L4 code in SQLite.

2. **Auto-Registration**: The websessions service automatically registers the session with the decision service by calling `POST /functions/{uuid}:{functionName}` for each `@export` function in the user's L4 code.

3. **Function Naming**: Functions from web sessions are named using the pattern `{uuid}:{functionName}`, for example:

   ```
   b52992ed-39fd-4226-bad2-2deee2473881:compute_qualifies
   ```

4. **Access Control**: These UUID-based functions remain fully accessible through:
   - `GET /functions/{uuid}:{name}` - Get function details
   - `POST /functions/{uuid}:{name}/evaluation` - Evaluate the function
   - All other function-specific endpoints

However, UUID-named functions are **filtered from the public function listing** (`GET /functions`) to prevent exposing user sessions to public discovery. This keeps the function list clean while still allowing anyone with the UUID to interact with the function directly.

### Example Session Workflow

```bash
# User uploads L4 code through web IDE
# → websessions generates UUID: b52992ed-39fd-4226-bad2-2deee2473881
# → websessions registers function with decision service

# Public listing doesn't show UUID functions
curl http://localhost:8081/functions
# Returns only explicitly published functions (no UUIDs)

# But direct access still works with the UUID
curl http://localhost:8081/functions/b52992ed-39fd-4226-bad2-2deee2473881:myFunction

# And evaluation works
curl -X POST \
  'http://localhost:8081/functions/b52992ed-39fd-4226-bad2-2deee2473881:myFunction/evaluation' \
  -H 'Content-Type: application/json' \
  -d '{"fnEvalBackend":"JL4","fnArguments":{"x": 42}}'
```

This design allows web sessions to be shareable via permalink while keeping the public API focused on official, published functions.

## See Also

- [http://github.com/smucclaw/lag](http://github.com/smucclaw/lag)
- [https://jl4.legalese.com/decision/swagger-ui/](https://jl4.legalese.com/decision/swagger-ui/)
- [jl4-websessions/README.md](../jl4-websessions/README.md) - Web IDE session management
