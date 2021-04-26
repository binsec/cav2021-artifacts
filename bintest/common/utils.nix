let impureNixpkgs = import <nixpkgs> {}; in
# for reproducibility let's use a pinned nixpkgs
let
  nixpkgs = impureNixpkgs.fetchFromGitHub {
              owner = "NixOS";
              repo = "nixpkgs";
              # get with nix-instantiate '<nixpkgs>' -A lib.nixpkgsVersion --eval | cut -d. -f3
              rev = "bbbe24f4695ba8313a2816889aa0d66a754a95b1";
              sha256 = "0r533fjsy0wj4w9pcinw2dgad71lvbsw5k8lcplyimi8rd4ijda8";
            };
  nixpkgs1909 = impureNixpkgs.fetchFromGitHub {
              owner = "NixOS";
              repo = "nixpkgs";
              # get with nix-instantiate '<nixpkgs>' -A lib.nixpkgsVersion --eval | cut -d. -f3
              rev = "129fb487c8678323cf769a942c750b65511dc06e";
              sha256 = "18vhadgzsxvifxwdbmh8lp2d84qlqcbzjfq5p02ms95j3mf4qrfx";
            };
  nixpkgs2003 = impureNixpkgs.fetchFromGitHub {
              owner = "NixOS";
              repo = "nixpkgs";
              # get with nix-instantiate '<nixpkgs>' -A lib.nixpkgsVersion --eval | cut -d. -f3
              rev = "2b6382c5c7b746ae677c33c2f4cd32cf74ca4773";
              sha256 = "0q44jwdx9zp7z0jzsk5m133cp8xfxfpi1g4kpp7y2vvqsmc3fjvr";
            };
in
rec {
  pkgs = import nixpkgs { system="i686-linux"; };
  pkgs1909 = (import nixpkgs1909 {
    crossSystem = (import (nixpkgs1909 +"/lib")).systems.examples.musl32;
  }).pkgsStatic;
  pkgs2003 = (import nixpkgs2003 {
    crossSystem = (import (nixpkgs2003 +"/lib")).systems.examples.musl32;
  }).pkgsStatic;
  onlyCFiles = directory: builtins.filterSource (name: type: (pkgs.lib.hasSuffix ".c" name)) directory;
  no_pie = pkg: (
  (builtins.getAttr "overrideAttrs" pkg) (
    attrs: attrs//{
      hardeningDisable = ["all"];
    }
  ));
  with_debug = pkg: (
  (builtins.getAttr "overrideAttrs" pkg) (
    attrs: attrs//{
      dontStrip = true;
      preUnpack = ''export CFLAGS="-g $CFLAGS";'';
    }
  ));
  standardBuildMaker = pkgs:
    { src, name ? "bintest", installPhase ? "mv a.out $out", ... }@attrs : (
    pkgs.stdenv.mkDerivation (attrs//{
      src = onlyCFiles src;
      hardeningDisable = ["all"];
      inherit installPhase name;
    })
  );
  standardBuild = standardBuildMaker pkgs;
  standardBuild1909 = standardBuildMaker pkgs1909;
}
