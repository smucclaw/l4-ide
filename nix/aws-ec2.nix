# for aws ec2 originally ubuntu AMI 20250606
# reinstalled using nixos-anywhere
# subsequently updated using nixos-rebuild switch --target-host

{ modulesPath, lib, ... }:
{
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  # Use systemd-networkd instead of dhcpcd for more predictable DNS management.
  # This avoids resolvconf signature mismatches when resolv.conf is manually edited.
  networking.useDHCP = false;
  networking.useNetworkd = true;
  systemd.network.networks."10-ens5" = {
    matchConfig.Name = "ens5";
    networkConfig.DHCP = "yes";
    linkConfig.RequiredForOnline = "yes";
  };
  # AWS VPC DNS resolver — fallback in case DHCP doesn't populate resolv.conf.
  # Without this, ACME cert renewal fails (can't resolve letsencrypt.org).
  networking.nameservers = [ "172.31.0.2" "169.254.169.253" ];
  # Disable wait-online to avoid deployment timeouts
  systemd.services.systemd-networkd-wait-online.enable = lib.mkForce false;
  
  time.timeZone = "Asia/Singapore";

  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    efiInstallAsRemovable = true;
  };

  boot.initrd.availableKernelModules = [
    "nvme"
  ];

  boot.initrd.kernelModules = [ "dm-snapshot" ];

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";


  swapDevices = [
    { device = "/dev/disk/by-partlabel/swap"; }
  ];

  disko.devices = {
    disk = {
      main = {
        device = "/dev/nvme0n1";
        type = "disk";
        content = {
          type = "gpt";
          partitions = {
            boot = {
              name = "boot";
              size = "1M";
              type = "EF02";
            };
            esp = {
              name = "ESP";
              size = "500M";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
              };
            };
            plainSwap = {
              size = "1G";
              name = "swap";
              content = {
                type = "swap";
                discardPolicy = "both";
                resumeDevice = true; # resume from hiberation from this device
              };
            };
            root = {
              size = "100%";
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "/";
              };
            };
          };
        };
      };
    };
  };
}
