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
    };

    services.nginx.virtualHosts.${config.networking.domain} = {
      enableACME = lib.mkForce false;
      forceSSL = lib.mkForce false;
    };

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
