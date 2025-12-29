# Deployment Guide

Quick reference for deploying L4 services to different environments.

## Environments

| Environment    | URL                                | Deployment Method            | Use Case                      |
| -------------- | ---------------------------------- | ---------------------------- | ----------------------------- |
| **Local Dev**  | `localhost:8001`, `localhost:8002` | Docker (`./jl4-dev start`)   | Local development and testing |
| **Local VM**   | `localhost:8080` (forwarded)       | NixOS VM (`jl4-demo`)        | Test NixOS config locally     |
| **Dev Server** | `https://dev.jl4.legalese.com`     | NixOS flake (`jl4-dev`)      | Cloud-based testing, staging  |
| **Production** | `https://jl4.legalese.com`         | NixOS flake (`jl4-aws-2505`) | Live production environment   |

## Quick Deploy Commands

### Local Development

**Docker (Recommended - One Command Start):**

```bash
# See doc/DOCKER.md for detailed instructions
./jl4-dev start        # Production build
./jl4-dev start-dev    # Development mode with hot reload
./jl4-dev logs         # View logs
./jl4-dev test         # Test all services
./jl4-dev stop         # Stop everything
```

**Manual Cabal (Legacy):**

```bash
# See dev-config.md for detailed instructions
./dev-start.sh full
```

### Local VM (Test NixOS Configuration)

```bash
# See doc/LOCAL-VM-DEPLOYMENT.md for detailed instructions

# Build and run VM
nixos-rebuild build-vm --flake '.#jl4-demo'
QEMU_NET_OPTS="hostfwd=tcp::2222-:22,hostfwd=tcp::8080-:80" \
  QEMU_OPTS="-display none" \
  result/bin/run-jl4-demo-vm &

# Access services
curl http://localhost:8080/decision/functions
```

### Dev Server

```bash
# On dev server (dev.jl4.legalese.com)
cd /path/to/l4-ide
git pull
nixos-rebuild switch --flake .#jl4-dev
```

### Production

```bash
# On production server (jl4.legalese.com)
cd /path/to/l4-ide
git pull
nixos-rebuild switch --flake .#jl4-aws-2505
```

## Service Architecture

Both dev and production use the same configuration with different domains:

```
User → nginx (443) → /decision → decision-service (localhost:8001)
                  → /session  → websessions (localhost:8002)

Services communicate internally via localhost:
  websessions → decision-service: http://localhost:8001 (push on save)
  decision-service → websessions: http://localhost:8002 (pull on demand)
```

## Configuration Files

| File                                         | Purpose                                             |
| -------------------------------------------- | --------------------------------------------------- |
| `flake.nix`                                  | Defines environment targets (jl4-dev, jl4-aws-2505) |
| `nix/configuration.nix`                      | Base NixOS configuration                            |
| `nix/jl4-decision-service/configuration.nix` | Decision service systemd config                     |
| `nix/jl4-websessions/configuration.nix`      | Websessions systemd config                          |
| `dev-config.md`                              | Detailed development guide                          |
| `dev-start.sh`                               | Helper script for local development                 |

## Testing After Deployment

### Check Services Status

```bash
# On server
systemctl status jl4-decision-service
systemctl status jl4-websessions
```

### Check Service Logs

```bash
# On server
journalctl -u jl4-decision-service -f
journalctl -u jl4-websessions -f
```

### Test Endpoints

```bash
# Test decision service
curl https://jl4.legalese.com/decision/functions

# Test websessions
curl -X POST https://jl4.legalese.com/session \
  -H "Content-Type: application/json" \
  -d '"DECIDE test MEANS TRUE"'
```

### Test Integration

```bash
# Save a program via websessions
UUID=$(curl -s -X POST https://jl4.legalese.com/session \
  -H "Content-Type: application/json" \
  -d '"GIVEN x IS A NUMBER\nDECIDE x IS EVEN MEANS x MOD 2 = 0\n@export even"' | tr -d '"')

# Check it's available in decision service
curl https://jl4.legalese.com/decision/functions/${UUID}:even
```

## Troubleshooting

### Services not starting

```bash
# Check logs for errors
journalctl -u jl4-decision-service -n 100
journalctl -u jl4-websessions -n 100

# Check port conflicts
lsof -i :8001
lsof -i :8002

# Verify nginx config
nginx -t
systemctl status nginx
```

### Integration not working

```bash
# Test websessions can reach decision service
curl -v http://localhost:8001/functions

# Test decision service can reach websessions
curl -v http://localhost:8002

# Check startup order
systemctl list-dependencies jl4-websessions
# Should show jl4-decision-service.service as a dependency
```

## Rollback

If deployment fails:

```bash
# On server
nixos-rebuild switch --rollback

# Or switch to specific generation
nixos-rebuild switch --rollback --generation N
```

## Documentation

- **[dev-config.md](./dev-config.md)** - Detailed local development guide
- **[dev-start.sh](./dev-start.sh)** - Helper script for local dev
- **[doc/LOCAL-VM-DEPLOYMENT.md](./doc/LOCAL-VM-DEPLOYMENT.md)** - Running local VM for testing
- **[nix/README.md](./nix/README.md)** - NixOS configuration details
