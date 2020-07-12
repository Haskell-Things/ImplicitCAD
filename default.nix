{ pkgs ? import <nixpkgs> {}, withImplicitSnap ? false }:
let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
in
  if withImplicitSnap
  then pkgs.haskellPackages.callCabal2nixWithOptions "implicit" src "-fimplicitsnap" { }
  else pkgs.haskellPackages.callCabal2nix "implicit" src { }
