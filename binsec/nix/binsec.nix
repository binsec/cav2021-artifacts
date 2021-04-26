{ stdenv
, nix-gitignore
, zlib
, ncurses
, zeromq
, gmp
, ocaml
, findlib
, ocamlgraph
, piqi
, piqi-ocaml
, zarith
, zmq
, menhir
, llvm
, qtest
, ounit
, qcheck
, seq
, autoreconfHook
, protobuf
, core
, libase
, cudd
, z3
, lib
}:
stdenv.mkDerivation {
  pname = "binsec";
  version = builtins.readFile ../VERSION;

  src = nix-gitignore.gitignoreSource [
    ''
      # additionnal ignores
      /nix
    ''
  ]
    ./..;


  strictDeps = true;

  buildInputs = [
    zlib
    ncurses
    zeromq
    gmp # for zarith
    core
    ocamlgraph
    piqi
    piqi-ocaml
    zarith
    zmq
    menhir
    llvm
    qtest
    ounit
    qcheck
    seq
    libase
    cudd
  ];
  nativeBuildInputs = [
    autoreconfHook
    protobuf
    ocaml
    findlib
    menhir
    piqi
    piqi-ocaml
    (lib.getBin z3) # for unittests
  ];

  enableParallelBuilding = true;

  makeFlags = [ "VERBOSEMAKE=1" ];
}
