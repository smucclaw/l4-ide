{ config, ... }:
{
  imports = [
    ./jl4-web/configuration.nix
    ./jl4-lsp/configuration.nix
    ./jl4-decision-service/configuration.nix
    ./jl4-websessions/configuration.nix
    ./module.nix
  ];

  system.stateVersion = "24.11";

  systemd.network.enable = true;

  # ---------------------------------------------
  # web
  # ---------------------------------------------

  networking = {
    inherit (config.jl4-demo) domain;
    hostName = "jl4-demo";
    networkmanager.enable = true;
    firewall.allowedTCPPorts = [
      80
      443
    ];
  };

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    virtualHosts.${config.networking.domain} = {
      enableACME = true;
      forceSSL = true;
    };
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = config.jl4-demo.acme-email;
  };

  # ---------------------------------------------
  # ssh
  # ---------------------------------------------

  services.fail2ban = {
    enable = true;
    maxretry = 5;
  };

  users.users.root.openssh.authorizedKeys.keys = config.jl4-demo.root-ssh-keys;

  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = false;
  };
}
