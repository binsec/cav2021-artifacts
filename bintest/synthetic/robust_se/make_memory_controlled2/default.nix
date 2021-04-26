with import ../../../common/pkgs.nix {};
standardBuild1909 {
  src = ./.;
  buildPhase = ''
    $CC -O2 -g -c b.c
    $CC -O2 -g -c a.c
    $CC -g b.o a.o
    '';
}
