let
  pkgs = import <nixpkgs> {};
  xest-package = pkgs.haskellPackages.callPackage (import (./release.nix)) {};
in xest-package
