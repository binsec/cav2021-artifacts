with import ../../../common/pkgs.nix {};
standardBuild1909 {
  src = ./.;
  buildPhase = ''
    $CC -O2 -fstack-protector -g -c b.c -o b.o
    $CC -fstack-protector -g -c a.c -o a.o
    $CC -fstack-protector -g a.o b.o
    '';
}
