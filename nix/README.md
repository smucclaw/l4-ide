# nix support

For comprehensive provisioning and deployment instructions, see:
- **[PROVISIONING.md](../PROVISIONING.md)** - Full guide for setting up new servers
- **[DEPLOYMENT.md](../DEPLOYMENT.md)** - Quick reference for deploying to existing servers

## Quick Reference

### Available Flake Targets

| Target | Domain | Environment |
|--------|--------|-------------|
| `jl4-demo` | `jl4.well-typed.com` | Original Hetzner demo |
| `jl4-aws-2505` | `jl4.legalese.com` | Production AWS EC2 |
| `jl4-dev` | `dev.jl4.legalese.com` | Dev/staging AWS EC2 |

### Redeploying to Existing Server

```sh
# Remote deployment
nixos-rebuild switch --flake .#jl4-dev --target-host root@dev.jl4.legalese.com

# Or on the server
ssh root@dev.jl4.legalese.com
cd /path/to/l4-ide && git pull
nixos-rebuild switch --flake .#jl4-dev
```

### Provisioning a New Server

**For AWS EC2:**

```sh
# 1. Launch Ubuntu EC2 instance
# 2. Configure DNS: dev.jl4.legalese.com → [EC2 IP]
# 3. Enable root SSH on the Ubuntu instance
# 4. Run nixos-anywhere from your local machine:

nixos-anywhere --flake .#jl4-dev root@dev.jl4.legalese.com
```

See **[PROVISIONING.md](../PROVISIONING.md)** for detailed step-by-step instructions.

### State to Transfer When Migrating Servers

- `/var/lib/acme/` - Let's Encrypt certificates
- `/var/lib/private/jl4-websessions/` - SQLite database with saved sessions
