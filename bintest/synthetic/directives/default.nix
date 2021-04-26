with import ../../common/pkgs.nix {};
standardBuild1909 {
  src = ./.;
  buildPhase = ''
    $CC -g -O3 a.c 
    '';
}
