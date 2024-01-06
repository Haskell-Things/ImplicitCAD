{ pkgs ? import <nixpkgs> { }
, compiler ? null
, withImplicitSnap ? false
}:
let
  overlay = import ./overlay.nix pkgs compiler withImplicitSnap;
  overrideHaskellPackages = orig: {
    buildHaskellPackages =
      orig.buildHaskellPackages.override overrideHaskellPackages;
    overrides = if orig ? overrides
      then pkgs.lib.composeExtensions orig.overrides overlay
      else overlay;
  };

  packageSet =
    if compiler == null
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  haskellPackages = packageSet.override overrideHaskellPackages;
in {
  inherit (haskellPackages)
    implicit
    implicit-interpreter;

  inherit haskellPackages;
  inherit pkgs;
}
