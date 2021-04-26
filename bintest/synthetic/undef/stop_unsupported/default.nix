with import ../../../common/pkgs.nix {};
standardBuild1909 {
  src = ./a.S;
  unpackPhase = ":";
  buildPhase = ''
    $CC $src
    '';
}
