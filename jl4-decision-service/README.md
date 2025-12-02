# Decision Service Backend

Exposes certain pre-compiled JL4 programs for use by an LLM AI, via the Tool Calling / Function Calling API.

The current MVP exposes a couple of example programs which are hardcoded.

In a future iteration the backend will

1. dynamically load the available programs from the `jl4/examples` directory
2. parse and run an L4 program `POST`ed in as part of the API call itself.

## Usage

Run the decision service backend locally from the project root:

```sh
> cabal run jl4-decision-service-exe -- --port 8081 --serverName http://localhost:8081/ --sourcePaths doc/tutorial-code/
```

or run it from the sub-directory:

```sh
> cd jl4-decision-service
> cabal run jl4-decision-service-exe -- --port 8081 --serverName http://localhost:8081/ --sourcePaths ../doc/tutorial-code/
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

## Loading L4 Functions

Two functions are hardcoded by default; see [src/Backend/Examples.hs](src/Backend/Examples.hs) for details.

Other functions can be loaded at start time using the `--sourcePaths` command line option.

The argument to the option is a directory or individual `.l4` files.

For each `.l4` file the loader first looks for in-source annotations. Functions are exposed when their leading comment uses the `@export` (or `@export default`) syntax:

```l4
@export default Demo entry point for the API
GIVEN input IS A Number @desc Example numeric argument
GIVETH A Number
demo input MEANS input + 1
```

Parameter descriptions can still use inline `@desc` annotations as shown above. If no `@export` annotations are found, the loader falls back to a matching `.yaml` metadata file.

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

## See Also

- [http://github.com/smucclaw/lag](http://github.com/smucclaw/lag)
- [https://jl4.legalese.com/decision/swagger-ui/](https://jl4.legalese.com/decision/swagger-ui/)
