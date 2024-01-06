pkgs: compiler: withImplicitSnap: hself: hsuper:
let
  lib = pkgs.lib;
  haskellLib = pkgs.haskell.lib;
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
in
{
  implicit =
    lib.pipe
      (
      if withImplicitSnap
      then hself.callCabal2nixWithOptions "implicit" src "-fimplicitsnap" {}
      else hself.callCabal2nix "implicit" src {}
      )
      [
        haskellLib.compose.buildFromSdist
      ];
  implicit-interpreter =
    lib.pipe
      (hself.callCabal2nix "implicit-interpreter" "${src}/implicit-interpreter" {})
      [
        haskellLib.compose.buildFromSdist
      ];
}
