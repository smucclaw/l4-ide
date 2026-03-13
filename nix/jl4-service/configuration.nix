{
  pkgs,
  lib,
  config,
  ...
}:
{
  options.services.jl4-service = {
    path = lib.mkOption {
      type = lib.types.str;
      default = "/service/";
      description = "path relative to the domain the jl4-service will answer on";
    };
    port = lib.mkOption {
      type = lib.types.int;
      default = 8003;
      description = "port of localhost to run jl4-service on";
    };
    storePath = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/jl4-service/store";
      description = "persistent filesystem path for the bundle store";
    };
    debug = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "enable verbose logging and detailed error responses";
    };
    bundles = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      default = {
        classic = ../../jl4/experiments/classic;
        thailand-cosmetics = ../../jl4/experiments/thailand-cosmetics;
      };
      description = ''
        Bundles to pre-seed into the store on startup.
        Keys are deployment IDs, values are directories containing .l4 files.
        Pre-seeded bundles are compiled on first boot; subsequent restarts use CBOR cache.
      '';
    };
  };

  config.services.nginx.virtualHosts.${config.networking.domain}.locations = {
    ${config.services.jl4-service.path}.proxyPass =
      "http://localhost:${toString config.services.jl4-service.port}/";
  };

  config.systemd.services.jl4-service = {
    enable = true;
    description = "jl4-service (multi-tenant L4 deployment service)";
    after = [ "network.target" "nginx.service" ];
    requires = [ "nginx.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStartPre = pkgs.writeShellScript "jl4-service-preseed" ''
        storePath="${config.services.jl4-service.storePath}"
        mkdir -p "$storePath"

        ${lib.concatStringsSep "\n" (lib.mapAttrsToList (id: srcDir: ''
          # Pre-seed bundle: ${id}
          bundleDir="$storePath/${id}"
          if [ ! -f "$bundleDir/metadata.json" ]; then
            echo "Pre-seeding bundle: ${id}"
            mkdir -p "$bundleDir/sources"
            cp -rL ${srcDir}/* "$bundleDir/sources/"
            echo '{"smFunctions":[],"smVersion":"pre-seeded","smCreatedAt":"2025-01-01T00:00:00Z"}' > "$bundleDir/metadata.json"
          else
            echo "Bundle already exists: ${id}"
          fi
        '') config.services.jl4-service.bundles)}
      '';
      ExecStart = pkgs.writeShellScript "jl4-service-start" ''
        exec ${pkgs.callPackage ./package.nix { }}/bin/jl4-service \
          --port ${toString config.services.jl4-service.port} \
          --store-path "${config.services.jl4-service.storePath}" \
          --server-name https://${config.networking.domain + config.services.jl4-service.path} \
          ${lib.optionalString config.services.jl4-service.debug "--debug"}
      '';
      Restart = "always";

      # Load ANTHROPIC_API_KEY from server-local env file (not in git).
      # Create on server: echo 'ANTHROPIC_API_KEY=sk-ant-...' > /var/lib/jl4-service/.env
      # The "-" prefix makes this optional - service starts even if file is missing.
      # TODO: Migrate to AWS Secrets Manager with ExecStartPre fetch script.
      EnvironmentFile = [ "-/var/lib/jl4-service/.env" ];

      StateDirectory = "jl4-service";

      # Security (relaxed vs decision-service: needs writable store)
      DynamicUser = true;
      NoNewPrivileges = true;
      ProtectSystem = "strict";
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
      # Note: ReadWritePaths is not needed - StateDirectory already provides
      # write access to /var/lib/jl4-service and all subdirectories
    };
  };
}
