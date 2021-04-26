let
  sources = import nix/sources.nix {};
  pkgs = import sources.nixpkgs {};
  angrpkgs = import sources.angr { inherit pkgs; };
in
pkgs.stdenv.mkDerivation {
  name = "angr-env";
  buildInputs = [ (pkgs.python3.withPackages(ps: [ angrpkgs.python3Packages.angr ])) ];
}
