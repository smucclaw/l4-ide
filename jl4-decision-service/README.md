# Decision Service Backend

Exposes certain pre-compiled JL4 programs for use by an LLM AI, via the Tool Calling / Function Calling API.

The current MVP exposes a couple of example programs which are hardcoded.

In a future iteration the backend will

1. dynamically load the available programs from the `jl4/examples` directory
2. parse and run an L4 program `POST`ed in as part of the API call itself.

## Usage

Run the decision service backend locally:

    cabal run jl4-decision-service-exe -- --port 8081 --serverName http://localhost:8081/

Then try executing a few things at http://localhost:8081/swagger-ui/

# See Also

http://github.com/smucclaw/lag

https://jl4.well-typed.com/decision/swagger-ui/
