# for aws ec2 originally ubuntu AMI 20250606
# reinstalled using nixos-anywhere
# subsequently updated using nixos-rebuild switch --target-host

{ modulesPath, lib, ... }:
{
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  networking.useDHCP = lib.mkDefault true;
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
