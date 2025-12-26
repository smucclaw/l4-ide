# this is meant for local dev of the nix flake deployment; it omits ACME and SSL.
# this variant is automatically selected by Nix when we run the flake with
#    nixos-rebuild build-vm --flake '.#jl4-aws-2505'
#    result/bin/ruj-jl4-demo-vm
#
# you can also get it to run via libvirt, with virt-install

{ config, pkgs, lib, ... }:
{
  virtualisation.vmVariant = {
    # following configuration is added only when building VM with build-vm

    # we write the IP address of the new instance to /tmp/vm-xchg, so we can ssh into it
    # we also enable password login for admin.
    # in the future it would be nice to have the equivalent of virt-install --bridge br0

    virtualisation = {
      memorySize = 1024; # Use 2048MiB memory.
      cores = 2;
      graphics = false;
      sharedDirectories = {
        myshare = {
          source = "/tmp/vm-xchg";
          target = "/tmp/vm-xchg";
        };
      };

      qemu.networkingOptions = [
        "-netdev bridge,br=br0,id=net0,helper=/run/wrappers/bin/qemu-bridge-helper"
        "-device virtio-net-pci,netdev=net0"
      ];
    };
    networking = {
      interfaces.eth0 = {
        useDHCP = true; # or configure static IP
      };
    };

    services.nginx.virtualHosts.${config.networking.domain} = {
      enableACME = lib.mkForce false;
      forceSSL = lib.mkForce false;
    };

    # Override decision service to use localhost for Swagger UI
    systemd.services.jl4-decision-service.serviceConfig.ExecStart = lib.mkForce (
      pkgs.writeShellScript "jl4-decision-service-start" ''
        sourcePathsArgs="${lib.concatStringsSep " " (map (p: "--sourcePaths ${p}") config.services.jl4-decision-service.sourcePaths)}"
        exec ${pkgs.callPackage ./jl4-decision-service/package.nix { }}/bin/jl4-decision-service-exe \
          --port ${toString config.services.jl4-decision-service.port} \
          --serverName https://localhost:8443${config.services.jl4-decision-service.path} \
          $sourcePathsArgs \
          --crudServerName localhost \
          --crudServerPort ${toString config.services.jl4-websessions.port}
      ''
    );

    users.groups.admin = {};
    users.users = {
      admin = {
        isNormalUser = true;
        extraGroups = [ "wheel" ];
        password = "admin";
        group = "admin";
      };
    };

    systemd.services.write-ip = {
      wantedBy = [ "multi-user.target" ];
      script = ''
    /run/current-system/sw/bin/sleep 2;
    /run/current-system/sw/bin/ip a > /tmp/vm-xchg/ip-a.txt;
    /run/current-system/sw/bin/ip r > /tmp/vm-xchg/ip-r.txt;
    /run/current-system/sw/bin/ip l > /tmp/vm-xchg/ip-l.txt;
  '';
    };

  };

}
