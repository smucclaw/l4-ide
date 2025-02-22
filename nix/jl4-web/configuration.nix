{ pkgs, config, ... }:
{
  services.nginx.virtualHosts.${config.networking.domain}.locations = {
    "/".root = pkgs.callPackage ./package.nix {
      url = "${config.networking.domain}${config.services.jl4-lsp.path}";
      secure = true;
    };
    "/robots.txt".root = pkgs.writeTextDir "robots.txt" ''
      User-agent: *
      Disallow: /
    '';
  };
}
