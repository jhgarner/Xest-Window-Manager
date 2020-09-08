let
  # capability = pkgs.haskell.lib.doJailbreak (import (builtins.fetchTarball https://github.com/tweag/capability/archive/v0.3.0.0.tar.gz) {}).capability;
  capability = pkgs: pkgs.haskell.lib.doJailbreak (import (builtins.fetchTarball https://github.com/tweag/capability/archive/v0.3.0.0.tar.gz) {inherit pkgs;}).capability;
  capabilitySrc = builtins.fetchTarball https://hackage.haskell.org/package/capability-0.3.0.0/capability-0.3.0.0.tar.gz;
  overrides = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = hself: hsuper: {  
        # capability = hself.callPackage capability self);
        capability = self.haskell.lib.doJailbreak (hself.callCabal2nix "capability" capabilitySrc { });
      };
    };
  };
  pkgs = import <nixpkgs> {overlays = [overrides];};
  xest-package = pkgs.haskellPackages.callPackage (import (./release.nix)) {};
in xest-package
