let
  pkgs = import ./pkgs.nix;
in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    niv
    unisim
    boolector
    z3
    cvc4
    yices
    ocamlPackages_for_binsec.merlin
    ocamlPackages_for_binsec.ocaml-lsp
    appimage-run
  ];
  inputsFrom = [ pkgs.binsec ];
  strictDeps = true; 
}
