{
  python3,
  lib,
  makeWrapper,
  ...
}:
let
  pythonEnv = python3.withPackages (ps: with ps; [
    flask
    rdflib
  ]);
in
python3.pkgs.stdenv.mkDerivation {
  pname = "fibo-sparql";
  version = "0.1.0";
  src = ./.;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin $out/share/fibo-sparql

    # Copy the FIBO RDF data (preserving directory structure)
    cp -r data $out/share/fibo-sparql/

    # Copy the server script
    cp fibo_sparql.py $out/share/fibo-sparql/

    # Create wrapper that sets Python path and default --fibo-dir
    makeWrapper ${pythonEnv}/bin/python3 $out/bin/fibo-sparql \
      --add-flags "$out/share/fibo-sparql/fibo_sparql.py" \
      --add-flags "--fibo-dir $out/share/fibo-sparql/data"
  '';

  meta = with lib; {
    description = "Lightweight SPARQL endpoint for FIBO regulatory data (SEC-215)";
    license = licenses.asl20;
  };
}
