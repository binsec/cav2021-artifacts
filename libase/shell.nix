let
  pkgs = import <nixpkgs> {};
  libase = import ./default.nix;
in
  pkgs.mkShell {
    inputsFrom = [ libase ];
    nativeBuildInputs = [ pkgs.ocaml-ng.ocamlPackages_4_05.merlin ];
  }

