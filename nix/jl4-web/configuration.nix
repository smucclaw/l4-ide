{ pkgs, config, lib, ... }:
let
  # Remove trailing slash for the wizard path (consistent URL formatting)
  wizardPath = lib.removeSuffix "/" config.services.l4-wizard.path;
in
{
  services.nginx.virtualHosts.${config.networking.domain}.locations = {
    "/".root = pkgs.callPackage ./package.nix {
      socket-url = "${config.networking.domain}${config.services.jl4-lsp.path}";
      session-url = "${config.networking.domain}${config.services.jl4-websessions.path}";
      decision-service-url = "${config.networking.domain}${config.services.jl4-decision-service.path}";
      wizard-path = wizardPath;  # Relative path, same origin
      secure = true;
    };
    "/robots.txt".root = pkgs.writeTextDir "robots.txt" ''
      User-agent: *
      Disallow: /
    '';
  };
}
