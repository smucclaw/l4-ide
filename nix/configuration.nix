{ config, pkgs, ... }:
{
  imports = [
    ./jl4-web/configuration.nix
    ./jl4-lsp/configuration.nix
    ./jl4-decision-service/configuration.nix
    ./jl4-websessions/configuration.nix
    ./l4-wizard/configuration.nix
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

  # Fix deployment timeout: nginx-config-reload must wait for nginx to finish
  # starting/restarting before attempting a reload
  systemd.services.nginx-config-reload = {
    after = [ "nginx.service" ];
    # Prevent long hangs during deployment - if reload doesn't complete in 10s,
    # fail and let the next trigger retry. This is safe because nginx will still
    # be serving (just with potentially stale config until next successful reload).
    serviceConfig.TimeoutStartSec = 10;
  };

  security.sudo.wheelNeedsPassword = false;

  # ---------------------------------------------
  # nix garbage collection
  # ---------------------------------------------

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  # Also optimize the store periodically to save disk space
  nix.optimise.automatic = true;

  # ---------------------------------------------
  # ssh
  # ---------------------------------------------

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
