with import ../../../common/pkgs.nix {};
pkgs1909.stdenv.mkDerivation {
  name = "coding-game-16";
  hardeningDisable = [ "all" ];
  dontStrip = true;
  buildCommand = ''
    mkdir -p $out
    $CC -O2 -g ${./hidden.c} -c -o hidden.o
    $CC -O2 -g ${./instrumented.c} -c -o instrumented.o
    $CC -O2 -g hidden.o instrumented.o -o $out/a.out
  '';
}
