# How to deploy to jl4.ymhan.com from a mac

SSH key forwarding

```bash
eval "$(ssh-agent -s)"
ssh-add <insert ...>
```

Run nixos-enabled x86_64 container with socket mounted

```bash
docker run --platform linux/amd64 -it \
  -v "$(pwd)":/workspace \
  -v /run/host-services/ssh-auth.sock:/ssh-agent \
  -e SSH_AUTH_SOCK=/ssh-agent \
  -w /workspace \
  nixos/nix
```

Check that SSH forwarding works with

```
ssh-add -l
```

or

```
ssh root@jl4.ymhan.com
```

Then

```
// disables seccomp filtering, allowing Nix operations that would otherwise fail under QEMU emulation.
echo "filter-syscalls = false" >> /etc/nix/nix.conf
echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

nix run github:nix-community/nixos-anywhere -- --flake '.#jl4-demo' --target-host root@jl4.ymhan.com

```
