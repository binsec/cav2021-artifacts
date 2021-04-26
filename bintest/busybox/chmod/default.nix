with import ../../common/pkgs.nix { debug_symbols = true; };
standardBuild {
  src = ./.;
  buildInputs = [ musl libbb ];
  buildPhase = ''
    musl-gcc -O3 -g -c a.c
    musl-gcc -static -O3 -g a.o -lbb
    '';
}
