{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    xz.dev
    pcre.dev
    zlib
    haskellPackages.zlib
  ];
  
  shellHook = ''
    export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:${pkgs.xz.dev}/lib/pkgconfig"
    export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:${pkgs.pcre.dev}/lib/pkgconfig"
  '';
}
