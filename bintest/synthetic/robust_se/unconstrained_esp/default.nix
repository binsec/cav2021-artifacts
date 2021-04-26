with import ../../../common/pkgs.nix {};
standardBuild1909 {
  src = ./.;
  buildPhase = ''
    $CC -O1 -g -c b.c -o b.o
    $CC -O1 -g -c a.c -o a.o
    $CC -g a.o b.o
    '';
}
