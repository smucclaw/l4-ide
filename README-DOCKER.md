# Docker Quick Start

One-command setup for running the entire JL4 system locally.

## TL;DR

```bash
# Start everything
./jl4-dev start

# View logs
./jl4-dev logs

# Open http://localhost:5173 in your browser
```

## What This Runs

- **jl4-web** - Web IDE at http://localhost:5173
- **jl4-lsp** - Language server at ws://localhost:8000
- **jl4-decision-service** - API at http://localhost:8001
- **jl4-websessions** - Session storage at http://localhost:8002

## Commands

```bash
./jl4-dev start       # Start all services
./jl4-dev start-dev   # Start in dev mode (hot reload)
./jl4-dev stop        # Stop all services
./jl4-dev logs        # View logs
./jl4-dev test        # Test all endpoints
./jl4-dev help        # Show all commands
```

## First Time Setup

**Prerequisites:**

- Docker and Docker Compose
- ~8GB RAM available for Docker
- Ports 5173, 8000-8002 available

**Initial build (takes 15-20 minutes):**

```bash
./jl4-dev build
./jl4-dev start
```

Subsequent starts take ~30 seconds.

## Documentation

See [doc/DOCKER.md](doc/DOCKER.md) for full documentation.

## Comparison to Other Methods

- **Docker (this)** - Best for: local dev, one-button start/stop, demos
- **Manual Cabal** - Best for: active Haskell development, fastest iteration
- **NixOS flake** - Best for: production deployment, declarative config

## Troubleshooting

```bash
# Services won't start
./jl4-dev clean
./jl4-dev rebuild

# View detailed logs
./jl4-dev logs jl4-decision-service

# Test connectivity
./jl4-dev test
```

## Contributing

When making changes:

1. Test locally with `./jl4-dev start-dev`
2. Verify with `./jl4-dev test`
3. Check logs for errors: `./jl4-dev logs`
4. Update relevant Dockerfile if changing dependencies
