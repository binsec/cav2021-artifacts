with import ../../common/pkgs.nix { debug_symbols = true; };
standardBuild {
  src = ./.;
  buildPhase = ''
    gcc ${uclibc_cflags} -static -O3 -g a.c
    '';
}
