{
  pkgs,
  lib,
  config,
  ...
}:
{
  options.services.jl4-websessions = {
    path = lib.mkOption {
      type = lib.types.str;
      default = "/session";
      description = "path relative to the domain the sessions server will answer on";
    };
    port = lib.mkOption {
      type = lib.types.int;
      default = 8002;
      description = "port of localhost to run the jl4-websessions server on";
    };
  };

  config.services.nginx.virtualHosts.${config.networking.domain}.locations = {
    ${config.services.jl4-websessions.path}.proxyPass =
      "http://localhost:${toString config.services.jl4-websessions.port}";
  };

  config.systemd.services.jl4-websessions = {
    enable = true;
    description = "jl4-websessions";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = ''
        ${pkgs.callPackage ./package.nix { }}/bin/jl4-websessions \
          ${toString config.services.jl4-websessions.port} \
          "sessions.db"
      '';
      Restart = "always";

      # Security
      DynamicUser = true;
      NoNewPrivileges = true;
      # Sandboxing
      ProtectSystem = "full";
      ProtectHome = true;
      PrivateTmp = true;
      PrivateDevices = true;
      PrivateUsers = true;
      ProtectHostname = true;
      ProtectClock = true;
      ProtectKernelTunables = true;
      ProtectKernelModules = true;
      ProtectKernelLogs = true;
      ProtectControlGroups = true;
      RestrictAddressFamilies = [ "AF_UNIX AF_INET AF_INET6" ];
      LockPersonality = true;
      MemoryDenyWriteExecute = true;
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      PrivateMounts = true;
    };
  };
}
