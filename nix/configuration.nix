{ config, pkgs, ... }:
{
  imports = [
    ./jl4-web/configuration.nix
    ./jl4-lsp/configuration.nix
    ./jl4-decision-service/configuration.nix
    ./jl4-websessions/configuration.nix
    ./module.nix
  ];

  # ---------------------------------------------
  # web
  # ---------------------------------------------

  networking = {
    inherit (config.jl4-demo) domain;
    hostName = "jl4-demo";
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

  security.sudo.wheelNeedsPassword = false;

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

  # we don't really expect anyone to log in, but if they do, it's nice to have some quality-of-life packages available
  environment.systemPackages = with pkgs; [
    vim
    wget
    emacs-nox
    btop
    zsh
    bat
    ripgrep
    cloud-utils
    lsof
    inetutils
    graphviz
    xdot
  ];
}
