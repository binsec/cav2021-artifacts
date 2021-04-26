with import ../../common/pkgs.nix {};
standardBuild1909 {
  src = ./.;
  buildPhase = ''
    $CC -g -c -o a.o a.c 
    $CC -O3 -L${openssl_ct.out}/lib/ a.o -lcrypto
    '';
}
