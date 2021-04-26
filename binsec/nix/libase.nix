{ ocaml
, nix-gitignore
, stdenv
, lib
, gmp
, findlib
, framac
, ocamlgraph
, zarith
, cudd
}: let
  repo = ../../libase;
  src = 
    nix-gitignore.gitignoreSourcePure
      [
        (repo + "/.gitignore")
        ''
          .git
          *.nix
          result
        ''
      ]
    repo;
in
stdenv.mkDerivation {
  pname = "libase";
  version = "1.0.0";
  inherit src;
  buildInputs = [
    gmp # for zarith
    ocaml
    findlib
    ocamlgraph
    zarith
    cudd
  ];
}
