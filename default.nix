{ rev ? "c542baa0c894796c92a8173dead027f3b952c22e"
, withImplicitSnap ? false
, pkgs ?
    if ((rev == "") || (rev == "default") || (rev == "local"))
      then import <nixpkgs> { }
      # Do not guard with hash, so the project is able to use current channels (rolling `rev`) of Nixpkgs
      else import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") { }
}:
let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
in
  if withImplicitSnap
  then pkgs.haskellPackages.callCabal2nixWithOptions "implicit" src "-fimplicitsnap" { }
  else pkgs.haskellPackages.callCabal2nix "implicit" src { }
