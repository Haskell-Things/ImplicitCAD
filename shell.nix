{ pkgs ? import <nixpkgs> {}, withImplicitSnap ? true }:
(import ./default.nix { inherit pkgs withImplicitSnap; }).env
