{ pkgs, config, ... }:
{
  services.nginx.virtualHosts.${config.networking.domain}.locations = {
    "/".root = pkgs.callPackage ./package.nix {
      socket-url = "${config.networking.domain}${config.services.jl4-lsp.path}";
      session-url = "${config.networking.domain}${config.services.jl4-websessions.path}";
      secure = true;
    };
    "/robots.txt".root = pkgs.writeTextDir "robots.txt" ''
      User-agent: *
      Disallow: /
    '';
  };
}
