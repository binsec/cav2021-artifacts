a.out: default.nix instrumented.c hidden.c
	nix-build default.nix
	cp -L result/$@ $@
	chmod u+w $@
