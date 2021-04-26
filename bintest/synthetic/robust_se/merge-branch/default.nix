with import ../../../common/pkgs.nix {};
standardBuild1909 {
  src = ./.;
  buildPhase = ''
    $CC -Os -g -c b.c -o b.o
    $CC -Os -g -c a.c -o a.o
    $CC -g a.o b.o
    '';
}
