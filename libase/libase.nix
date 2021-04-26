{ ocaml, nix-gitignore, stdenv, gmp, findlib, framac, ocamlgraph, zarith, cudd }:
stdenv.mkDerivation {
  pname = "libase";
  version = "1.0.0";
  src = nix-gitignore.gitignoreSourcePure
    [
      ./.gitignore
      ''*.nix
        result''
    ] ./.;
  buildInputs = [
    gmp # for zarith
    ocaml
    findlib
    ocamlgraph
    zarith
    cudd
  ];
}
