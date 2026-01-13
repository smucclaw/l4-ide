{
  decision-service-url ? "localhost:8001",
  base-path ? "",
  secure ? false,
  buildNpmPackage,
  importNpmLock,
  pkg-config,
  libsecret,
  lib,
  ...
}:
buildNpmPackage rec {
  pname = "l4-wizard";
  version = "0-latest";
  src = lib.cleanSourceWith {
    src = ../../.;
    filter = path: type:
      let
        relPath = lib.removePrefix (toString ../../. + "/") (toString path);
        baseName = baseNameOf (toString path);
      in
        # Include all directories to maintain path structure, while only copying specific files
        type == "directory" ||
        lib.hasPrefix "ts-apps" relPath ||
        lib.hasPrefix "ts-shared" relPath ||
        baseName == "package.json" ||
        baseName == "package-lock.json";
  };
  npmDeps = importNpmLock { npmRoot = src; };
  npmWorkspace = src;
  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ libsecret.dev ];
  buildPhase = ''
    runHook preBuild
    set -x
    export VITE_DECISION_SERVICE_URL=${if secure then "https" else "http"}://${decision-service-url};
    export VITE_BASE_PATH=${base-path};

    pushd ./ts-shared

    pushd ./type-utils
    npm run build
    popd

    pushd ./decision-service-types
    npm run build
    popd

    pushd ./viz-expr
    npm run build
    popd

    pushd ./boolean-analysis
    npm run build
    popd

    pushd ./jl4-client-rpc
    npm run build
    popd

    pushd ./vscode-webview-rpc
    npm run build
    popd

    pushd ./layout-ir
    npm run build
    popd

    pushd ./l4-ladder-visualizer
    npm run build
    popd

    popd

    pushd ./ts-apps

    pushd ./webview
    npm run build
    popd

    # HACK: we need to build workspace dependencies by hand because npm
    # isn't clever enough to figure out workspace dependencies

    pushd ./l4-wizard
    npm run build
    set +x
    runHook postBuild
  '';
  installPhase = ''
    mv build $out
  '';
  npmConfigHook = importNpmLock.npmConfigHook;
}
