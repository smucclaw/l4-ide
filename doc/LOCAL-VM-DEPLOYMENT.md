# Local VM Deployment Guide

This guide explains how to build, run, and deploy to a local jl4-demo VM for testing the NixOS deployment configuration without needing access to remote servers.

## Overview

The jl4-demo VM provides a local environment that mirrors the production deployment structure, allowing you to:

- Test NixOS configuration changes locally before deploying to production
- Develop and test systemd service configurations
- Verify nginx routing and TLS setup (self-signed certificates in VM)
- Test the full stack (jl4-web, jl4-decision-service, jl4-websessions) in a production-like environment

## Quick Start

### 1. Build the VM

```bash
cd /path/to/l4-ide
nixos-rebuild build-vm --flake '.#jl4-demo'
```

This creates `result/bin/run-jl4-demo-vm` and will download ~160MB of packages on first build.

**Build output:**

- `result` → symlink to `/nix/store/.../nixos-vm`
- `result/bin/run-jl4-demo-vm` → VM launcher script
- Creates `jl4-demo.qcow2` disk image (1GB virtual, ~20MB actual) in the project directory on first run
  - This disk image is the persistent VM state (gitignored)
  - Located at `/path/to/l4-ide/jl4-demo.qcow2`
  - Can be reused with libvirt/virt-manager

### 2. Run the VM

**Method A: Direct QEMU (recommended, uses port forwarding)**

**With SSH access (recommended for development):**

```bash
# Forward ports: SSH (2222→22), HTTP (8080→80), HTTPS (8443→443)
QEMU_NET_OPTS="hostfwd=tcp::2222-:22,hostfwd=tcp::8080-:80,hostfwd=tcp::8443-:443" \
  QEMU_OPTS="-display none" \
  result/bin/run-jl4-demo-vm
```

**With serial console (for interactive debugging):**

```bash
result/bin/run-jl4-demo-vm -nographic
```

Press `Ctrl-A X` to exit QEMU serial console.

**In background (headless):**

```bash
QEMU_NET_OPTS="hostfwd=tcp::2222-:22,hostfwd=tcp::8080-:80,hostfwd=tcp::8443-:443" \
  QEMU_OPTS="-display none" \
  result/bin/run-jl4-demo-vm &

# Stop later with:
pkill -f "qemu-system.*jl4-demo"
```

**Method B: Using libvirt/virt-manager (for bridged networking)**

If you want the VM to get its own IP on your network (instead of port forwarding), you can import the qcow2 disk image into libvirt:

```bash
# Prerequisites: host must have a bridge configured (e.g., br0)
# Check virtualisation.libvirtd.allowedBridges in host's configuration.nix

# Import the disk image into libvirt
virt-install \
  --name jl4-demo \
  --memory 1024 \
  --vcpus 1 \
  --disk path=/home/mengwong/src/smucclaw/l4-ide/jl4-demo.qcow2,format=qcow2,bus=virtio \
  --boot kernel=/nix/store/.../kernel,initrd=/nix/store/.../initrd,kernel_args="..." \
  --network bridge=br0,model=virtio \
  --graphics none \
  --noautoconsole

# Manage with virsh
virsh list --all
virsh start jl4-demo
virsh shutdown jl4-demo
virsh destroy jl4-demo  # force stop

# Find VM IP
virsh domifaddr jl4-demo --source arp
# or check ARP table
arp -an | grep "$(virsh domiflist jl4-demo | grep -o '52:54:00:[0-9a-f:]*')"
```

**Note:** The libvirt method requires specifying the full kernel/initrd paths and boot arguments. See `result/bin/run-jl4-demo-vm` for the exact paths. For most use cases, Method A (port forwarding) is simpler and sufficient.

### 3. Access the VM

**Via SSH (requires SSH key configured):**

```bash
# If your key is in flake.nix jl4-demo.root-ssh-keys:
ssh root@localhost -p 2222

# Otherwise, use serial console to add your key first
```

**Via serial console:**

```bash
# Login credentials (console only - SSH uses keys):
# Username: admin
# Password: admin

# Or as root (with configured SSH key)
```

**Via HTTP:**

```bash
# Access services through forwarded ports:
curl http://localhost:8080/             # nginx (jl4-web)
curl http://localhost:8080/decision/functions   # decision service
curl http://localhost:8080/session/     # websessions service
```

### 4. Deploy Changes to Running VM

After modifying nix configuration:

**If using Method A (direct QEMU):**

```bash
# Rebuild the VM
nixos-rebuild build-vm --flake '.#jl4-demo'

# Restart the VM
pkill -f "qemu-system.*jl4-demo"
QEMU_NET_OPTS="hostfwd=tcp::2222-:22,hostfwd=tcp::8080-:80,hostfwd=tcp::8443-:443" \
  QEMU_OPTS="-display none" \
  result/bin/run-jl4-demo-vm &
```

**If using Method B (libvirt):**

```bash
# Rebuild the VM
nixos-rebuild build-vm --flake '.#jl4-demo'

# Restart the VM
virsh shutdown jl4-demo  # or: virsh destroy jl4-demo
virsh start jl4-demo
```

**Or, deploy to running VM via SSH:**

```bash
# First, ensure VM can access your l4-ide repository
# Option 1: Use virtfs (already mounted at /nix/store in VM)
# Option 2: Use git clone inside VM

ssh root@localhost -p 2222

# Inside VM:
cd /path/to/l4-ide
nixos-rebuild switch --flake '.#jl4-demo'
```

## Configuration Details

### VM Settings

The VM is configured via `nix/aws-vm.nix` which overrides production settings:

| Setting        | Production    | VM                                     |
| -------------- | ------------- | -------------------------------------- |
| **ACME/TLS**   | Let's Encrypt | Self-signed certificates               |
| **forceSSL**   | Enabled       | Disabled                               |
| **Memory**     | 4GB+          | 1GB                                    |
| **CPUs**       | 2+            | 1                                      |
| **Networking** | Public IP     | User-mode NAT (port forwarding)        |
| **Admin user** | No            | Yes (username: admin, password: admin) |

### Flake Configuration

The VM inherits configuration from `flake.nix`:

```nix
nixosConfigurations.jl4-demo = inputs.nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    {
      jl4-demo = {
        domain = "jl4.well-typed.com";
        acme-email = "magnus@well-typed.com";
        root-ssh-keys = [
          "ssh-ed25519 AAAAC3Nza... mangoiv@p14-nixos"
          "ssh-ed25519 AAAAC3Nza... hannes@well-typed.com"
          "ssh-ed25519 AAAAC3Nza... andres@well-typed.com"
        ];
      };
    }
    inputs.disko.nixosModules.default
    ./nix/configuration.nix
    ./nix/hetzner.nix  # Disk partitioning (ignored by VM)
  ];
};
```

### Adding Your SSH Key

To access the VM via SSH, add your public key to `flake.nix`:

```bash
# Get your public key
cat ~/.ssh/id_ed25519.pub
# or
cat ~/.ssh/id_rsa.pub
```

Add to `flake.nix` under `nixosConfigurations.jl4-demo`:

```nix
root-ssh-keys = [
  "ssh-ed25519 AAAAC3Nza... mangoiv@p14-nixos"
  "ssh-ed25519 AAAAC3Nza... hannes@well-typed.com"
  "ssh-ed25519 AAAAC3Nza... andres@well-typed.com"
  "ssh-ed25519 AAAAC3Nza... YOUR-KEY-HERE"  # Add this line
];
```

Then rebuild the VM:

```bash
nixos-rebuild build-vm --flake '.#jl4-demo'
```

## Port Forwarding

The `QEMU_NET_OPTS` environment variable controls port forwarding:

```bash
# Syntax:
# hostfwd=tcp::[host-port]-:[guest-port]

# Forward SSH (host:2222 → VM:22)
QEMU_NET_OPTS="hostfwd=tcp::2222-:22"

# Forward multiple ports
QEMU_NET_OPTS="hostfwd=tcp::2222-:22,hostfwd=tcp::8080-:80,hostfwd=tcp::8443-:443"

# Forward decision service directly (bypass nginx)
QEMU_NET_OPTS="hostfwd=tcp::8001-:8001,hostfwd=tcp::8002-:8002"
```

## Troubleshooting

### VM Won't Start

**Error: `Could not access KVM kernel module`**

```bash
# Check if KVM is available
ls -l /dev/kvm

# If not available, VM will use TCG (slower but works)
# KVM is not required but significantly improves performance
```

**Error: `qemu-bridge-helper not found`**

The VM falls back to user-mode networking (NAT with port forwarding). This is expected and works fine for local development.

### Can't SSH to VM

**Check SSH port forwarding:**

```bash
ss -tlnp | grep 2222
# Should show: LISTEN ... 0.0.0.0:2222 ... qemu-system-x86_64
```

**Check SSH key is configured:**

```bash
# Verify your key is in flake.nix jl4-demo.root-ssh-keys
grep "$(cat ~/.ssh/id_ed25519.pub | cut -d' ' -f2)" flake.nix
```

**Access via serial console to debug:**

```bash
# Stop VM and restart with serial console
pkill -f "qemu-system.*jl4-demo"
result/bin/run-jl4-demo-vm -nographic

# Login as admin/admin
# Check SSH status:
systemctl status sshd
journalctl -u sshd -n 50
```

### Services Not Running

**Check service status:**

```bash
ssh root@localhost -p 2222 'systemctl status jl4-decision-service jl4-websessions nginx'
```

**View logs:**

```bash
ssh root@localhost -p 2222 'journalctl -u jl4-decision-service -n 50'
ssh root@localhost -p 2222 'journalctl -u jl4-websessions -n 50'
ssh root@localhost -p 2222 'journalctl -u nginx -n 50'
```

**Test services directly:**

```bash
# Decision service
curl http://localhost:8080/decision/functions

# Websessions
curl http://localhost:8080/session/

# Direct to service (bypass nginx)
ssh root@localhost -p 2222 'curl http://localhost:8001/functions'
```

### VM Network Issues

**Can't reach services on forwarded ports:**

```bash
# Check VM is running
ps aux | grep qemu-system | grep jl4-demo

# Check ports are forwarded
ss -tlnp | grep -E '2222|8080|8443'

# Check nginx is listening inside VM
ssh root@localhost -p 2222 'ss -tlnp | grep -E ":80|:443"'
```

### Reset VM State

If the VM is in a bad state, delete the disk image and rebuild:

```bash
# Stop VM
pkill -f "qemu-system.*jl4-demo"

# Delete disk image
rm jl4-demo.qcow2

# Restart VM (will create fresh disk image)
QEMU_NET_OPTS="hostfwd=tcp::2222-:22,hostfwd=tcp::8080-:80,hostfwd=tcp::8443-:443" \
  QEMU_OPTS="-display none" \
  result/bin/run-jl4-demo-vm &
```

## Development Workflow

### Test Configuration Changes

1. **Edit nix configuration:**

   ```bash
   vim nix/jl4-decision-service/configuration.nix
   # or
   vim nix/configuration.nix
   ```

2. **Rebuild VM:**

   ```bash
   nixos-rebuild build-vm --flake '.#jl4-demo'
   ```

3. **Restart VM:**

   ```bash
   pkill -f "qemu-system.*jl4-demo"
   QEMU_NET_OPTS="hostfwd=tcp::2222-:22,hostfwd=tcp::8080-:80,hostfwd=tcp::8443-:443" \
     QEMU_OPTS="-display none" \
     result/bin/run-jl4-demo-vm &
   ```

4. **Verify changes:**
   ```bash
   curl http://localhost:8080/decision/functions
   ssh root@localhost -p 2222 'systemctl status jl4-decision-service'
   ```

### Deploy to Production After Testing

Once changes work in the VM:

```bash
# Commit changes
git add nix/
git commit -m "Update jl4-decision-service configuration"

# Deploy to production
nixos-rebuild switch --flake '.#jl4-aws-2505' --target-host nano
```

## Comparison: Local VM vs Production

| Aspect     | Local VM                                      | Production AWS                                                     |
| ---------- | --------------------------------------------- | ------------------------------------------------------------------ |
| **Build**  | `nixos-rebuild build-vm --flake '.#jl4-demo'` | N/A (deployed directly)                                            |
| **Deploy** | `result/bin/run-jl4-demo-vm`                  | `nixos-rebuild switch --flake '.#jl4-aws-2505' --target-host nano` |
| **Access** | `ssh root@localhost -p 2222`                  | `ssh root@jl4.legalese.com`                                        |
| **URL**    | `http://localhost:8080`                       | `https://jl4.legalese.com`                                         |
| **TLS**    | Self-signed                                   | Let's Encrypt                                                      |
| **Domain** | jl4.well-typed.com (config)                   | jl4.legalese.com                                                   |
| **State**  | Ephemeral (unless backed up)                  | Persistent                                                         |
| **DB**     | Fresh on each boot                            | `/var/lib/private/jl4-websessions/`                                |

## Files Created

| File                          | Purpose                              |
| ----------------------------- | ------------------------------------ |
| `result`                      | Symlink to `/nix/store/.../nixos-vm` |
| `result/bin/run-jl4-demo-vm`  | VM launcher script                   |
| `jl4-demo.qcow2`              | VM disk image (1GB virtual, ~20MB actual) in project root (gitignored) |
| `/tmp/nix-vm.*/xchg/`         | Shared directory between host and VM |
| `/tmp/nix-vm.*/xchg/ip-*.txt` | VM network info (written by VM)      |

## See Also

- **[DEPLOYMENT.md](../DEPLOYMENT.md)** - Production deployment guide
- **[PROVISIONING.md](../PROVISIONING.md)** - Setting up new servers
- **[dev-config.md](../dev-config.md)** - Local development without VM (cabal run)
- **[nix/aws-vm.nix](../nix/aws-vm.nix)** - VM-specific configuration overrides
- **[flake.nix](../flake.nix)** - NixOS system definitions
