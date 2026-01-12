# Server Provisioning Guide

> **Navigation:** [Documentation Index](./README.md) | Provisioning (you are here) | [Deployment](./deployment.md)

This guide explains how to provision a new server (dev or production) from scratch using `nixos-anywhere` to convert a standard Ubuntu EC2 instance to NixOS.

**Use this guide for:** One-time initial server setup
**For ongoing deployments, see:** [deployment.md](./deployment.md)

## Overview

The L4 infrastructure uses NixOS for reproducible server configuration. We use `nixos-anywhere` to install NixOS on cloud VMs that initially run Ubuntu.

**Existing Servers:**

- **Production:** `jl4.legalese.com` (AWS EC2, flake target: `jl4-aws-2505`)
- **Dev/Staging:** `dev.jl4.legalese.com` (to be provisioned, flake target: `jl4-dev`)

## Prerequisites

On your local machine, you need:

```bash
# Install nix (if not already installed)
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

# nixos-anywhere is available in nixpkgs
nix profile install nixpkgs#nixos-anywhere

# Or run directly from this repo's flake
nix run nixpkgs#nixos-anywhere -- --help
```

## Step 1: Provision AWS EC2 Instance

### Create the Instance

1. **Launch EC2 instance:**
   - **AMI:** Ubuntu Server 22.04 LTS or later (e.g., `ami-0c55b159cbfafe1f0`)
   - **Instance type:** `t3.medium` or larger (minimum 2 vCPU, 4GB RAM)
   - **Storage:** 20GB+ EBS volume (NVMe)
   - **Security group:** Open ports 22 (SSH), 80 (HTTP), 443 (HTTPS)

2. **Create or update DNS record:**

   ```
   dev.jl4.legalese.com → [EC2 Public IP]
   ```

3. **Test SSH access:**
   ```bash
   ssh -i ~/.ssh/your-key.pem ubuntu@dev.jl4.legalese.com
   ```

### Prepare for nixos-anywhere

The initial Ubuntu instance needs to accept root SSH connections:

```bash
# SSH as ubuntu user first
ssh -i ~/.ssh/your-key.pem ubuntu@dev.jl4.legalese.com

# Enable root SSH temporarily
sudo mkdir -p /root/.ssh
sudo cp ~/.ssh/authorized_keys /root/.ssh/
sudo chmod 700 /root/.ssh
sudo chmod 600 /root/.ssh/authorized_keys
sudo sed -i 's/^#PermitRootLogin.*/PermitRootLogin prohibit-password/' /etc/ssh/sshd_config
sudo systemctl restart sshd

# Exit
exit
```

Test root access:

```bash
ssh -i ~/.ssh/your-key.pem root@dev.jl4.legalese.com
```

## Step 2: Configure Your SSH Key in flake.nix

Before running `nixos-anywhere`, ensure your SSH public key is in the flake configuration:

```bash
# Get your SSH public key
cat ~/.ssh/id_ed25519.pub
# or
cat ~/.ssh/id_rsa.pub
```

Add to `flake.nix` in the appropriate `nixosConfigurations` section:

```nix
jl4-demo = {
  domain = "dev.jl4.legalese.com";
  acme-email = "mengwong@legalese.com";
  root-ssh-keys = [
    "ssh-ed25519 AAAAC3Nza... your-key-here"
  ];
};
```

## Step 3: Install NixOS with nixos-anywhere

From your local machine:

```bash
# For dev server
nixos-anywhere --flake .#jl4-dev root@dev.jl4.legalese.com

# For production server (if provisioning new)
nixos-anywhere --flake .#jl4-aws-2505 root@jl4.legalese.com
```

**What this does:**

1. Partitions the disk according to `nix/aws-ec2.nix` (disko configuration)
2. Installs NixOS
3. Applies the configuration from `flake.nix` and `nix/configuration.nix`
4. Reboots into NixOS
5. Sets up systemd services for jl4-decision-service and jl4-websessions

**This will take 10-20 minutes.** The server will reboot during the process.

### Troubleshooting nixos-anywhere

If it fails:

```bash
# Try with verbose logging
nixos-anywhere --flake .#jl4-dev --debug root@dev.jl4.legalese.com

# Common issues:
# - SSH keys not configured: Check flake.nix root-ssh-keys
# - Network issues: Ensure security group allows port 22
# - Disk issues: Verify /dev/nvme0n1 exists on the instance
```

## Step 4: Verify Installation

After nixos-anywhere completes and the server reboots:

```bash
# SSH to the new NixOS system
ssh root@dev.jl4.legalese.com

# Check NixOS version
nixos-version

# Check services are running
systemctl status jl4-decision-service
systemctl status jl4-websessions
systemctl status nginx

# Check endpoints
curl http://localhost:8001/functions
curl http://localhost:8002

# Check external access (after DNS propagates and ACME issues cert)
curl https://dev.jl4.legalese.com/decision/functions
```

## Step 5: Initial State Transfer (Optional)

If migrating from an existing server, transfer state:

```bash
# From old server
ssh root@old-server.com
tar czf /tmp/state-backup.tar.gz \
  /var/lib/acme/ \
  /var/lib/private/jl4-websessions/

# Copy to new server
scp /tmp/state-backup.tar.gz root@dev.jl4.legalese.com:/tmp/

# On new server
ssh root@dev.jl4.legalese.com
cd /
tar xzf /tmp/state-backup.tar.gz
systemctl restart jl4-websessions
systemctl restart nginx
```

## Step 6: DNS and ACME Setup

1. **Update DNS** to point to new server IP (if not done already)

   ```
   dev.jl4.legalese.com → [New EC2 IP]
   ```

2. **Wait for DNS propagation** (5-30 minutes)

   ```bash
   dig dev.jl4.legalese.com
   ```

3. **ACME will automatically request Let's Encrypt certificate**

   ```bash
   # Watch ACME certificate issuance
   journalctl -u acme-dev.jl4.legalese.com -f

   # Once complete, check
   ls -la /var/lib/acme/dev.jl4.legalese.com/
   ```

4. **Test HTTPS**
   ```bash
   curl https://dev.jl4.legalese.com/decision/swagger.json
   ```

## Step 7: Test the Full Stack

```bash
# Test websessions
UUID=$(curl -s -X POST https://dev.jl4.legalese.com/session \
  -H "Content-Type: application/json" \
  -d '"GIVEN x IS A NUMBER\nGIVETH A BOOLEAN\n@export\nisEven MEANS x % 2 = 0"' | tr -d '"')

echo "Created session: $UUID"

# Verify it's in decision service
curl https://dev.jl4.legalese.com/decision/functions/${UUID}:isEven

# Test evaluation
curl -X POST https://dev.jl4.legalese.com/decision/functions/${UUID}:isEven/evaluation \
  -H "Content-Type: application/json" \
  -d '{"fnArguments": {"x": 4}}'
```

## Subsequent Deployments

After initial provisioning is complete, see [deployment.md](./deployment.md) for ongoing deployment procedures.

## Configuration Reference

### Flake Targets

| Target         | Domain                 | Use Case                            |
| -------------- | ---------------------- | ----------------------------------- |
| `jl4-demo`     | `jl4.well-typed.com`   | Original Hetzner demo (deprecated?) |
| `jl4-aws-2505` | `jl4.legalese.com`     | Production AWS EC2                  |
| `jl4-dev`      | `dev.jl4.legalese.com` | Dev/staging AWS EC2                 |

### Key Files

| File                                         | Purpose                                                 |
| -------------------------------------------- | ------------------------------------------------------- |
| `flake.nix`                                  | Defines nixosConfigurations (jl4-dev, jl4-aws-2505)     |
| `nix/configuration.nix`                      | Base system configuration (nginx, services)             |
| `nix/aws-ec2.nix`                            | AWS-specific hardware config (disk partitions, network) |
| `nix/aws-vm.nix`                             | AWS VM-specific settings                                |
| `nix/jl4-decision-service/configuration.nix` | Decision service systemd config                         |
| `nix/jl4-websessions/configuration.nix`      | Websessions systemd config                              |

### Port Configuration

| Service              | Internal Port | External Access          |
| -------------------- | ------------- | ------------------------ |
| jl4-decision-service | 8001          | https://DOMAIN/decision/ |
| jl4-websessions      | 8002          | https://DOMAIN/session   |
| nginx                | 80, 443       | Direct                   |

## Security Considerations

1. **SSH Keys:** Only use SSH key authentication (password auth disabled)
2. **Firewall:** AWS security group should only allow 22, 80, 443
3. **Updates:** Regularly rebuild from main branch for security patches
4. **Secrets:** No secrets in flake.nix (it's public); use age/sops for secrets if needed
5. **ACME certificates:** Auto-renewed by NixOS

## Troubleshooting

### Services won't start

```bash
# Check logs
journalctl -u jl4-decision-service -n 100
journalctl -u jl4-websessions -n 100

# Check ports
lsof -i :8001
lsof -i :8002

# Manually test service
cd /nix/store/...-jl4-decision-service/bin
./jl4-decision-service-exe --help
```

### ACME certificate issues

```bash
# Check ACME logs
journalctl -u acme-dev.jl4.legalese.com

# Retry certificate
systemctl restart acme-dev.jl4.legalese.com

# Check DNS is correct
dig dev.jl4.legalese.com
curl -I http://dev.jl4.legalese.com/.well-known/acme-challenge/test
```

### Disk space issues

```bash
# Check disk usage
df -h

# Clean up old nix generations
nix-collect-garbage -d

# List generations
nix-env --list-generations

# Delete specific generation
nix-env --delete-generations 10
```

## Rollback

### If Provisioning Fails

If `nixos-anywhere` fails or you need to start completely over:

```bash
# 1. Terminate the EC2 instance in AWS console
# 2. Create a new Ubuntu instance
# 3. Run nixos-anywhere again from Step 1
```

### If NixOS is Installed But Broken

Once NixOS is installed, use standard rollback procedures documented in [deployment.md](./deployment.md#rollback).

## Reference Documentation

### Internal Documentation

- **[README.md](./README.md)** - Deployment documentation index
- **[deployment.md](./deployment.md)** - Ongoing deployment procedures

### External Documentation

- [nixos-anywhere documentation](https://github.com/nix-community/nixos-anywhere)
- [NixOS Manual](https://nixos.org/manual/nixos/stable/)
- [Disko (disk partitioning)](https://github.com/nix-community/disko)
