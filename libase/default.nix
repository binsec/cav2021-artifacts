let
  pkgs = import <nixpkgs> {};
  cp = pkgs.ocaml-ng.ocamlPackages_4_05.callPackage;
  cudd = cp ./cudd.nix {};
  codex = cp ./libase.nix { inherit cudd; };
in
  codex
