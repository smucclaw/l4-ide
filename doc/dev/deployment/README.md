# Deployment Documentation

This directory contains documentation for deploying and managing L4 services.

## Which Guide Do I Need?

### Setting up a brand new server? → [provisioning.md](./provisioning.md)

Use this for **one-time initial setup** of a new dev or production server:

- Converting an Ubuntu EC2 instance to NixOS using `nixos-anywhere`
- Configuring DNS, ACME certificates, SSH keys
- Initial state transfer from old servers

### Deploying code changes? → [deployment.md](./deployment.md)

Use this for **ongoing deployments** to existing servers:

- Quick reference for all deployment commands (local, dev, production)
- Service status checks and testing
- Troubleshooting common issues
- Rollback procedures

### Running locally? → [docker.md](./docker.md) or [local-vm.md](./local-vm.md)

- **docker.md**: Docker-based local development (recommended)
- **local-vm.md**: Testing NixOS configuration in a local VM

## Quick Start

**First time ever deploying a server:**

1. Read [provisioning.md](./provisioning.md)
2. Follow all steps to set up the server
3. Then use [deployment.md](./deployment.md) for future updates

**Already have servers running:**

- Go straight to [deployment.md](./deployment.md)

**Working on your laptop:**

- Start with [docker.md](./docker.md) for the easiest local setup

## Documentation Overview

| File                | Purpose                                   | When to Use                    |
| ------------------- | ----------------------------------------- | ------------------------------ |
| **README.md**       | This file - navigation guide              | Start here if unsure           |
| **provisioning.md** | One-time server setup with nixos-anywhere | New server, disaster recovery  |
| **deployment.md**   | Ongoing deployment procedures             | Code updates, daily operations |
| **docker.md**       | Docker-based local development            | Local dev work                 |
| **local-vm.md**     | Local NixOS VM for testing configs        | Testing NixOS changes locally  |

## Common Tasks

| Task                                      | Guide           | Section                  |
| ----------------------------------------- | --------------- | ------------------------ |
| Deploy code to dev server                 | deployment.md   | Dev Server               |
| Deploy code to production                 | deployment.md   | Production               |
| Check if services are running             | deployment.md   | Testing After Deployment |
| Rollback a bad deployment                 | deployment.md   | Rollback                 |
| Set up a new dev/prod server from scratch | provisioning.md | All sections             |
| Troubleshoot service startup issues       | deployment.md   | Troubleshooting          |
| Test NixOS config changes locally         | local-vm.md     | All sections             |
| Run services on your laptop               | docker.md       | All sections             |

## Server Inventory

| Server       | URL                          | Flake Target | Documentation |
| ------------ | ---------------------------- | ------------ | ------------- |
| Production   | https://jl4.legalese.com     | jl4-aws-2505 | deployment.md |
| Dev/Staging  | https://dev.jl4.legalese.com | jl4-dev      | deployment.md |
| Local Docker | localhost:8001, :8002        | N/A          | docker.md     |
| Local VM     | localhost:8080 (forwarded)   | jl4-demo     | local-vm.md   |
