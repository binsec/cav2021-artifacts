with import ../../common/pkgs.nix {};
standardBuild {
  src = ./.;
  buildInputs = [ musl ];
  buildPhase = ''
    musl-gcc -static -O3 -g a.c
    '';
}
