{
  pkgs,
  lib,
  config,
  ...
}:
{
  options.services.jl4-decision-service = {
    path = lib.mkOption {
      type = lib.types.str;
      default = "/decision/";
      description = "path relative to the domain the decision-service will answer on";
    };
    port = lib.mkOption {
      type = lib.types.int;
      default = 8001;
      description = "port of localhost to run the websocket server of the jl4-decision-service on";
    };
  };

  config.services.nginx.virtualHosts.${config.networking.domain}.locations = {
    ${config.services.jl4-decision-service.path}.proxyPass =
      "http://localhost:${toString config.services.jl4-decision-service.port}/";
  };

  config.systemd.services.jl4-decision-service = {
    enable = true;
    description = "jl4-decision-service";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = ''
        ${pkgs.callPackage ./package.nix { }}/bin/jl4-decision-service-exe \
          --port ${toString config.services.jl4-decision-service.port} \
          --serverName https://${config.networking.domain + config.services.jl4-decision-service.path} \
          --crudServerName ${config.networking.domain} \
          --crudServerPort ${toString config.services.jl4-websessions.port} \
          --crudServerPath ${config.services.jl4-websessions.path} \
          --crudServerSecure
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
