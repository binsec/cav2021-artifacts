let
  sources = import ./sources.nix {};
  overlay = self: super: {
    niv = (import sources.niv {}).niv;
    unisim = super.callPackage ./unisim.nix {};
    ocamlPackages_for_binsec = self.ocaml-ng.ocamlPackages_4_07.overrideScope'(oself: osuper: {
      cudd = oself.callPackage ./cudd.nix {};
      libase = oself.callPackage ./libase.nix {};
    });
    binsec = self.ocamlPackages_for_binsec.callPackage ./binsec.nix { inherit (self) z3; };
    binsec_appimage = super.callPackage ./bundle.nix {};
    dot-merlin-reader = super.dot-merlin-reader.override { ocamlPackages = self.ocamlPackages_for_binsec; };
    /* z3 has a large regression in 20.09 https://github.com/Z3Prover/z3/issues/4743 */
    inherit (import sources.nixpkgs_2003 {}) appimage-run z3;
  };
  pkgs = import sources.nixpkgs { overlays = [ overlay ]; };
in
  pkgs
