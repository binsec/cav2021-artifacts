let
  # use vanilla nixpkgs, at a pinned version for reproducibility
  sources = import ../../binsec/nix/sources.nix {};
  nixpkgs = import sources.nixpkgs {};
  # use 32bit linux static musl version
  pkgs = nixpkgs.pkgsi686Linux.pkgsStatic;
in
  pkgs.mkShell {
    # let's keep the assembly simple
    hardeningDisable = [ "all" ];
  }
