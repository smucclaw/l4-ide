# Docker Development Environment

This document describes the Docker-based local development environment for the JL4 system.

## Overview

The JL4 system consists of four services that work together:

- **jl4-lsp** (port 8000) - Language Server Protocol websocket server
- **jl4-decision-service** (port 8001) - Decision API for evaluating L4 functions
- **jl4-websessions** (port 8002) - Session management and L4 program storage
- **jl4-web** (port 5173) - Web-based IDE frontend

## Quick Start

### Prerequisites

- Docker and Docker Compose installed
- At least 8GB of RAM available for Docker
- Ports 5173, 8000, 8001, 8002 available on localhost

### One-Command Start

```bash
# Production build (optimized binaries)
./jl4-dev start

# Development mode (hot reload, slower startup)
./jl4-dev start-dev

# View logs
./jl4-dev logs

# Test all services
./jl4-dev test

# Stop everything
./jl4-dev stop
```

Then open http://localhost:5173 in your browser.

## Commands

### Starting Services

```bash
# Start all services (production build)
./jl4-dev start

# Start all services in development mode (hot reload)
./jl4-dev start-dev

# Stop all services
./jl4-dev stop

# Restart all services
./jl4-dev restart
```

### Monitoring

```bash
# Show service status
./jl4-dev status

# View logs from all services
./jl4-dev logs

# View logs from a specific service
./jl4-dev logs jl4-web
./jl4-dev logs jl4-decision-service

# Show running containers
./jl4-dev ps
```

### Building

```bash
# Build all Docker images
./jl4-dev build

# Rebuild all images from scratch (no cache)
./jl4-dev rebuild
```

### Testing

```bash
# Run integration tests on all services
./jl4-dev test
```

### Maintenance

```bash
# Open a shell in a service container
./jl4-dev shell jl4-lsp
./jl4-dev shell jl4-websessions

# Clean up (stops services and removes volumes)
./jl4-dev clean
```

## Architecture

### Service Dependencies

```
jl4-web → depends on → jl4-lsp
                    → jl4-websessions
                    → jl4-decision-service

jl4-decision-service → depends on → jl4-websessions

jl4-websessions → depends on → jl4-decision-service (optional push)
```

### Data Flow

```
User → jl4-web (5173)
         ↓
         → jl4-lsp (8000) for language support
         → jl4-websessions (8002) for saving/loading programs
         → jl4-decision-service (8001) for evaluating functions

jl4-websessions → jl4-decision-service (pushes saved programs)
jl4-decision-service → jl4-websessions (pulls programs by UUID)
```

### Volumes

- **websessions-data** - Persistent SQLite database for saved L4 programs
- **cabal-cache** (dev mode only) - Cached Haskell dependencies

## Configuration

### Environment Variables

The `jl4-dev` script supports these optional environment variables:

- `COMPOSE_FILE` - Override the docker-compose file (default: `docker-compose.yml`)
- `DEV_COMPOSE_FILE` - Override the dev compose file (default: `docker-compose.dev.yml`)

### Port Mapping

All services bind to localhost by default. To change ports, edit `docker-compose.yml`:

```yaml
services:
  jl4-web:
    ports:
      - "5173:5173" # Change left side to bind to different host port
```

## Development Modes

### Production Mode (`./jl4-dev start`)

- Multi-stage Docker builds
- Optimized binaries
- Minimal container size
- Fast startup after initial build
- Best for: Testing the full stack, demo deployments

**Build time:** ~15-20 minutes (first time), ~2-5 minutes (incremental)

**Startup time:** ~30 seconds

### Development Mode (`./jl4-dev start-dev`)

- Source code mounted as volumes
- Changes reflected on rebuild
- Full Cabal cache preserved
- Easier debugging
- Best for: Active development, debugging

**Build time:** ~15-20 minutes (first time)

**Startup time:** ~2-3 minutes (compiles on each start)

## Troubleshooting

### Services won't start

```bash
# Check Docker is running
docker ps

# Check ports are available
lsof -i :5173
lsof -i :8000
lsof -i :8001
lsof -i :8002

# View detailed logs
./jl4-dev logs
```

### Build fails

```bash
# Clean and rebuild from scratch
./jl4-dev clean
./jl4-dev rebuild
```

### Service health check fails

Health checks verify:

- LSP: Port 8000 accepting connections
- Decision Service: `/functions` endpoint responding
- Websessions: Root endpoint responding
- Web: HTTP server responding

View health status:

```bash
docker compose ps
```

### Database issues

```bash
# Reset the database (WARNING: destroys all saved sessions)
./jl4-dev clean
./jl4-dev start
```

### Out of disk space

Docker images are large (~3-4GB total). Clean up:

```bash
# Remove old containers and images
docker system prune -a

# Remove volumes (including session database)
./jl4-dev clean
```

## Comparison with Other Deployment Methods

| Method             | Use Case           | Pros                      | Cons                      |
| ------------------ | ------------------ | ------------------------- | ------------------------- |
| **Docker (this)**  | Local dev, demos   | One command, reproducible | Large images, build time  |
| **Cabal directly** | Active Haskell dev | Fast iteration            | Manual setup, 3 terminals |
| **NixOS flake**    | Production deploy  | Declarative, reliable     | Requires NixOS            |
| **Dev script**     | Quick local test   | Lightweight               | No isolation, manual deps |

## Production Deployment

This Docker setup is designed for local development. For production deployment:

1. **Build optimized images:**

   ```bash
   docker compose build --no-cache
   ```

2. **Push to registry:**

   ```bash
   docker tag jl4-decision-service your-registry/jl4-decision-service:latest
   docker push your-registry/jl4-decision-service:latest
   ```

3. **Deploy to container platform:**
   - AWS ECS/Fargate
   - Kubernetes
   - Docker Swarm
   - Or continue using NixOS (current production method)

## AI Assistant Friendly

This Docker setup is designed to be easy for AI coding assistants to work with:

- ✅ Simple commands (`./jl4-dev start`, `./jl4-dev logs`)
- ✅ Clear service names and ports
- ✅ Comprehensive help text (`./jl4-dev help`)
- ✅ Built-in testing (`./jl4-dev test`)
- ✅ Self-documenting configuration (docker-compose.yml)
- ✅ Colorized output for easy parsing

When asking an AI to debug or modify the system, provide:

- Output from `./jl4-dev status`
- Relevant logs from `./jl4-dev logs [service]`
- The specific service and file you're modifying

## See Also

- [DEPLOYMENT.md](../DEPLOYMENT.md) - Production deployment guide
- [dev-config.md](../dev-config.md) - Manual local development guide
- [dev-start.sh](../dev-start.sh) - Legacy bash script for manual startup
