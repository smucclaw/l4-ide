# Development Configuration Guide

## Running L4 Services Locally

This guide explains how to run the L4 services locally for development and testing.

### TL;DR - Quick Start

**Option 1: Use the helper script**
```bash
# Show all commands for full stack
./dev-start.sh full

# Or start individual services
./dev-start.sh decision-only           # Just decision service
./dev-start.sh websessions-with-push   # Websessions with push enabled
```

**Option 2: Manual startup (3 terminals)**
```bash
# Terminal 1: Decision service
cd jl4-decision-service
cabal run jl4-decision-service-exe -- --port 8001 --sourcePaths ../jl4/examples \
  --crudServerName localhost --crudServerPort 8002

# Terminal 2: Websessions (after decision service starts)
cd jl4-websessions
cabal run jl4-websessions -- 8002 /tmp/sessions.db http://localhost:8001

# Terminal 3: Web frontend
cd ts-apps/jl4-web
npm run dev
```

Then open http://localhost:5173 in your browser.

### Development vs Production Bootstrapping

| Environment | How to Start | Configuration | Domain |
|-------------|--------------|---------------|--------|
| **Local Dev** | `cabal run` commands | Command-line arguments | `localhost` |
| **Dev Server** | `nixos-rebuild switch --flake .#jl4-dev` | `flake.nix` + `nix/*.nix` | `dev.jl4.legalese.com` |
| **Production** | `nixos-rebuild switch --flake .#jl4-aws-2505` | `flake.nix` + `nix/*.nix` | `jl4.legalese.com` |

**Local Development** uses `cabal` to build and run services with explicit command-line arguments for configuration (ports, URLs, paths).

**Dev Server & Production** use NixOS to build from the flake and systemd to manage services. All configuration comes from `nix/*.nix` files which set the same command-line arguments automatically. The only difference is the domain name.

### Deploying to Environments

**Deploy to Dev Server (AWS EC2):**
```bash
# SSH to dev server
ssh root@dev.jl4.legalese.com

# Pull latest changes
cd /path/to/l4-ide
git pull origin mengwong/auto-update-decision-service-from-websessions

# Rebuild with dev flake target
nixos-rebuild switch --flake .#jl4-dev

# Services automatically restart with new config
```

**Deploy to Production (AWS EC2):**
```bash
# SSH to production server
ssh root@jl4.legalese.com

# Pull latest changes
cd /path/to/l4-ide
git pull origin main

# Rebuild with production flake target
nixos-rebuild switch --flake .#jl4-aws-2505

# Services automatically restart with new config
```

The flake configurations (`jl4-dev` vs `jl4-aws-2505`) set the appropriate domain, which propagates to:
- nginx server name
- ACME/Let's Encrypt certificate
- Swagger API URLs
- CORS settings (if applicable)

### Architecture Overview

```
Production (NixOS):
                                     Internet
                                        │
                                        ▼
                              ┌─────────────────┐
                              │  nginx (443)    │
                              │  Listens: 0.0.0.0│
                              └────────┬────────┘
                                       │
                    ┌──────────────────┼──────────────────┐
                    │ /session         │ /decision         │
                    ▼                  ▼                   │
         ┌──────────────────┐  ┌──────────────────┐      │
         │  websessions     │  │  decision-service│      │
         │  Listen: 0.0.0.0 │  │  Listen: 0.0.0.0 │      │
         │  Port: 8002      │  │  Port: 8001      │      │
         └──────────────────┘  └──────────────────┘      │
                │                       │                 │
                │  Service-to-Service   │                 │
                │  (localhost only)     │                 │
                │◄──────────────────────┤                 │
                │  http://localhost:8002│                 │
                │                       │                 │
                └───────────────────────►                 │
                   http://localhost:8001                  │
                                                          │
         ┌─────────────────────────────────────────────┐ │
         │ Swagger URLs (external):                   │ │
         │  https://jl4.legalese.com/decision/...    │◄┘
         └─────────────────────────────────────────────┘

Development:
    All services on localhost (no nginx)
    External URLs = http://localhost:PORT
```

**Key Points:**
- Services bind to **0.0.0.0** (all interfaces) so nginx can reach them
- Services communicate with each other via **localhost** (same machine, fast)
- nginx proxies public traffic from **/session** → localhost:8002 and **/decision** → localhost:8001
- Swagger uses **external domain** (https://jl4.legalese.com) for API documentation

### Port Configuration

**Default Development Ports:**
- `jl4-websessions`: `localhost:8002`
- `jl4-decision-service`: `localhost:8001`
- `jl4-web` (Svelte frontend): `localhost:5173`

**Production Ports (NixOS):**
- Both services communicate internally via localhost
- External access via nginx on ports 80/443

### Starting Services

#### 1. Start the Decision Service

```bash
cd jl4-decision-service

# Without websessions integration (basic mode)
cabal run jl4-decision-service-exe -- \
  --port 8001 \
  --sourcePaths ../jl4/examples

# With websessions integration (full mode)
cabal run jl4-decision-service-exe -- \
  --port 8001 \
  --sourcePaths ../jl4/examples \
  --crudServerName localhost \
  --crudServerPort 8002 \
  --crudServerPath ""
```

#### 2. Start the Websessions Service

```bash
cd jl4-websessions

# Without decision service push (standalone mode)
cabal run jl4-websessions -- 8002 /tmp/sessions.db

# With decision service push (integrated mode)
cabal run jl4-websessions -- 8002 /tmp/sessions.db http://localhost:8001
```

#### 3. Start the Web Frontend

```bash
cd ts-apps/jl4-web
npm run dev
```

### Testing the Integration

Once all services are running:

1. **Save a program in the Web IDE:**
   ```
   http://localhost:5173/?id=new
   ```

2. **The save will:**
   - Generate a UUID (e.g., `b52992ed-39fd-4226-bad2-2deee2473881`)
   - Store in SQLite (`/tmp/sessions.db`)
   - Push to decision service (if URL configured)

3. **Test retrieval via websessions:**
   ```bash
   curl "http://localhost:8002?id=b52992ed-39fd-4226-bad2-2deee2473881"
   ```

4. **Test retrieval via decision service:**
   ```bash
   # Get function metadata
   curl "http://localhost:8001/functions/b52992ed-39fd-4226-bad2-2deee2473881:functionName"

   # Call the function
   curl -X POST "http://localhost:8001/functions/b52992ed-39fd-4226-bad2-2deee2473881:functionName/evaluation" \
     -H "Content-Type: application/json" \
     -d '{"fnArguments": {"param1": "value1"}}'
   ```

### Common Development Scenarios

#### Scenario 1: Frontend Development Only
```bash
# Start decision service with preloaded examples
cd jl4-decision-service
cabal run jl4-decision-service-exe -- --port 8001 --sourcePaths ../jl4/examples

# Start web frontend
cd ts-apps/jl4-web
npm run dev
```

#### Scenario 2: Full Stack Testing
```bash
# Terminal 1: Decision service
cd jl4-decision-service
cabal run jl4-decision-service-exe -- \
  --port 8001 --sourcePaths ../jl4/examples \
  --crudServerName localhost --crudServerPort 8002

# Terminal 2: Websessions (wait for decision service to start)
cd jl4-websessions
cabal run jl4-websessions -- 8002 /tmp/sessions.db http://localhost:8001

# Terminal 3: Web frontend
cd ts-apps/jl4-web
npm run dev
```

#### Scenario 3: Testing Websessions Push
```bash
# Start decision service first
cd jl4-decision-service
cabal run jl4-decision-service-exe -- --port 8001

# Start websessions with push enabled
cd jl4-websessions
cabal run jl4-websessions -- 8002 /tmp/sessions.db http://localhost:8001

# Test push by creating a session
curl -X POST "http://localhost:8002" \
  -H "Content-Type: application/json" \
  -d '"GIVEN x IS A NUMBER\nDECIDE x IS EVEN MEANS x MOD 2 = 0"' | tee /tmp/uuid.txt

# Verify it's available in decision service
UUID=$(cat /tmp/uuid.txt | tr -d '"')
curl "http://localhost:8001/functions/${UUID}:IS%20EVEN"
```

### Environment Variables (Optional)

You can set these for convenience:

```bash
export JL4_DECISION_PORT=8001
export JL4_WEBSESSIONS_PORT=8002
export JL4_WEB_PORT=5173
export JL4_DB_PATH=/tmp/sessions.db
```

### Troubleshooting

**Problem: Decision service can't reach websessions**
- Check websessions is running: `curl http://localhost:8002`
- Check the port number in decision service config
- Ensure no firewall blocking localhost connections

**Problem: Websessions not pushing to decision service**
- Check decision service is running: `curl http://localhost:8001/functions`
- Verify the decision service URL parameter was passed to websessions
- Check websessions logs for HTTP errors

**Problem: Functions not appearing in decision service**
- Check `@export` annotations in your L4 code
- Use `@export default` for a default function
- Verify no #EVAL/#ASSERT directives (they're stripped)

### Production vs Development

|                          | Development              | Production (NixOS)              |
|--------------------------|--------------------------|---------------------------------|
| Service binding          | `0.0.0.0:PORT`          | `0.0.0.0:PORT`                 |
| websessions → decision   | `http://localhost:8001`  | `http://localhost:8001`         |
| decision → websessions   | `http://localhost:8002`  | `http://localhost:8002`         |
| Public access            | Direct to ports          | Via nginx → localhost           |
| Swagger server URL       | `http://localhost:8001`  | `https://jl4.legalese.com/...` |
| TLS                      | No                       | Yes (nginx terminates)          |
| Database                 | `/tmp/sessions.db`       | `/var/lib/private/...`          |

**Key Architecture Points:**
- Both environments: Services bind to **all interfaces** (0.0.0.0)
- Both environments: Service-to-service communication uses **localhost** (fast, no TLS overhead)
- Production only: nginx reverse proxy adds TLS and path routing (/session, /decision)
- Swagger URLs differ: dev uses localhost, prod uses external domain
