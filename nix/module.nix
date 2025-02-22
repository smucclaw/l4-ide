{ lib, ... }:
{
  options.jl4-demo = {
    root-ssh-keys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      description = "The ssh keys that may log on to the demo machine";
    };
    domain = lib.mkOption {
      type = lib.types.str;
      description = "domain at which the demo is hosted";
    };
    acme-email = lib.mkOption {
      type = lib.types.str;
      description = "the email for accepting the terms for letsencrypt acme";
    };
  };
}
