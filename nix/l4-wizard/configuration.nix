{ pkgs, config, lib, ... }:
let
  # Remove trailing slash for the base path (SvelteKit expects no trailing slash)
  basePath = lib.removeSuffix "/" config.services.l4-wizard.path;
  wizardPackage = pkgs.callPackage ./package.nix {
    decision-service-url = "${config.networking.domain}${config.services.jl4-decision-service.path}";
    base-path = basePath;
    secure = true;
  };
in
{
  options.services.l4-wizard = {
    path = lib.mkOption {
      type = lib.types.str;
      default = "/wizard";
      description = "URL path where the wizard will be served (without trailing slash)";
    };
  };

  config.services.nginx.virtualHosts.${config.networking.domain}.locations = {
    # Serve the wizard app from the configured path
    "${config.services.l4-wizard.path}/" = {
      alias = "${wizardPackage}/";
      tryFiles = "$uri $uri/ ${config.services.l4-wizard.path}/index.html";
      extraConfig = ''
        # Handle SPA routing - serve index.html for unknown paths
        error_page 404 = ${config.services.l4-wizard.path}/index.html;
      '';
    };
  };
}
