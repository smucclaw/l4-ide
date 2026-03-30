{
  pkgs,
  lib,
  config,
  ...
}:
{
  options.services.fibo-sparql = {
    path = lib.mkOption {
      type = lib.types.str;
      default = "/sparql/";
      description = "URL path prefix for the SPARQL endpoint on the domain";
    };
    port = lib.mkOption {
      type = lib.types.int;
      default = 7878;
      description = "Localhost port for the SPARQL server";
    };
  };

  config.services.nginx.virtualHosts.${config.networking.domain}.locations = {
    # Proxy /sparql/ to the Flask app
    ${config.services.fibo-sparql.path} = {
      proxyPass = "http://localhost:${toString config.services.fibo-sparql.port}/";
    };
    # Also handle /sparql without trailing slash
    "/sparql" = {
      return = "301 /sparql/";
    };
  };

  config.systemd.services.fibo-sparql = {
    enable = true;
    description = "FIBO SPARQL endpoint (rdflib + Flask)";
    after = [ "network.target" "nginx.service" ];
    requires = [ "nginx.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = pkgs.writeShellScript "fibo-sparql-start" ''
        exec ${pkgs.callPackage ./package.nix { }}/bin/fibo-sparql \
          --host 127.0.0.1 \
          --port ${toString config.services.fibo-sparql.port}
      '';
      Restart = "always";
      RestartSec = 5;

      # Security hardening
      DynamicUser = true;
      NoNewPrivileges = true;
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
      RestrictRealtime = true;
      RestrictSUIDSGID = true;
      PrivateMounts = true;
    };
  };
}
