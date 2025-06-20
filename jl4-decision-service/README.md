# Decision Service Backend

Exposes certain JL4 programs for use by an LLM AI, via the Tool Calling / Function Calling API.

## Endpoints

    https://jl4.legalese.com/decision/swagger-ui/

Where do the JL4 programs come from?

## Hardcoded

The current MVP exposes a couple of example programs which are hardcoded: `compute qualifies` and `rodents and vermin`.

You get these for free when you run

    cabal run jl4-decision-service-exe -- \
       --port 8081 \
       --serverName http://jl4.legalese.com:8081/

See [src/Examples.hs](Examples.hs) for details.

## Loaded at Runtime

The backend can be instructed to load pairs of `.l4` and `.yaml` files, some of which can be found in the `jl4/experiments` directory.

To tell it where to load, add one or more `sourcePaths` arguments,
which can be a directory or an L4 file.

    --sourcePaths doc/tutorial-code/
    --sourcePaths jl4/experiments/
    --sourcePaths jl4/experiments/something.l4

L4 files without `.yaml` metadata will be ignored.

## Uploaded via the API itself

There is primitive functionality accept a new L4 program via `POST` and subsequently offer it.

This functionality has not been tested.

# Actual Run Examples

Usually you would only run the decision service this way, by hand, in development.

    cabal run jl4-decision-service-exe --           \
        --port 8081                                 \
        --serverName http://localhost:8081/         \
        --sourcePaths jl4/experiments/parking.l4    \
        --sourcePaths doc/tutorial-code             \
        --sourcePaths jl4/experiments/britishcitizen5.l4

Then try executing a few things at http://localhost:8081/swagger-ui/

# In Production

The deployed decision service is configured under [nix/](../nix/jl4-decision-service/configuration.nix).

In the deployed environment, the service automatically loads L4 files from both:

- `jl4/experiments/` - Various experimental L4 programs
- `doc/tutorial-code/` - Tutorial examples

These files are vendored into the Nix package at build time using Cabal's data-files mechanism.

# See Also

http://github.com/smucclaw/lag

https://jl4.legalese.com/decision/swagger-ui/
