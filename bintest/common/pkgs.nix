{ debug_symbols ? false }:
let utils = import ./utils.nix; in
with utils;
let f = x: no_pie (if debug_symbols then (with_debug x) else x); in
utils // rec {
  musl = f pkgs.musl;
  busybox = f (pkgs.busybox.override {musl = musl; useMusl = true;});
  libbb = busybox.overrideAttrs (attrs: attrs//{
  installPhase = ''
    mkdir -p $out/lib
    ar d libbb/lib.a appletlib.o
    cp libbb/lib.a $out/lib/libbb.a
    cp networking/libiproute/lib.a $out/lib/libiproute.a
    '';
  });
  uclibc = f (pkgs.uclibc.override { extraConfig = "HAS_NO_THREADS y"; });
  uclibc_cflags = ''-isystem ${uclibc}/include -L${uclibc}/lib -B${uclibc}/lib'';
  openssl_ct = (pkgs1909.openssl_1_1.override {static = true;}).overrideAttrs (old: {
    postPatch = (old.postPatch or "") + ''
      sed -e 's/static//' -e 's/OPENSSL_malloc(outlen)/*out/' -e '/OPENSSL_free/d' -i crypto/ct/ct_b64.c
      rm -rf test
    '';
    configureFlags = pkgs1909.lib.lists.filter (x: x!="--enable-static" && x!="--disable-shared") old.configureFlags;
    doCheck = false;
  });
  aspell = f pkgs.aspell;
}
