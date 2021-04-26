let
  sources = import ./nix/sources.nix {};
  pkgs = import sources.nixpkgs {};
  pkgs_binsec = import ../binsec/nix/pkgs.nix;
in
pkgs.mkShell {
  # llvm for FileCheck
  nativeBuildInputs = (with pkgs; [
    moreutils
    llvm_8
    (python3.withPackages (ps: with ps; [ python-prctl pandas numpy ]))
  ]) ++ (with pkgs_binsec; [
    binsec
    unisim
    z3
    boolector
  ]);

  shellHook = ''
    export PATH=$PATH:${toString ./.}
  '';
}
