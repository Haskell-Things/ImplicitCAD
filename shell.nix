attrs@{...}:
let
  inherit (import ./. attrs) pkgs haskellPackages;
  hlib = pkgs.haskell.lib;

  packages = [
    "implicit"
    "implicit-interpreter"
  ];
  extract-external-inputs = p:
    builtins.filter
      (dep: !(builtins.elem dep packages))
      (map
        (x: x.pname)
        (hlib.getHaskellBuildInputs haskellPackages.${p}));
  external-inputs =
    map
      (x: haskellPackages.${x})
      (builtins.concatLists
        (map
          extract-external-inputs
          packages));
  metaPackage =
    haskellPackages.mkDerivation
      { pname = "implicit-shell";
        version = "0.0.0.0";
        libraryHaskellDepends = external-inputs;
        license = pkgs.stdenv.lib.licenses.asl20;};

  package-envs =
    builtins.listToAttrs
      (map
        (p:
          { name = p;
            value = haskellPackages.${p}.env;})
        packages);

in

metaPackage.env // package-envs
