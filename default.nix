{pkgs ? import <nixpkgs> {}}:

let
  gitignoreSrc = pkgs.fetchFromGitHub { 
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "c4662e662462e7bf3c2a968483478a665d00e717";
    sha256 = "sha256:1npnx0h6bd0d7ql93ka7azhj40zgjp815fw2r6smg8ch9p7mzdlx";
  };
  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;
  # If you don't pass in a pinned nixpkgs, this might fail. See xest.nix for an
  # example.
  xest-package = pkgs.pkgs.haskell.packages.ghc884.callPackage (import (./release.nix)) {inherit gitignoreSource;};
  with-config = xest-package.overrideAttrs (old: {
    postInstall = ''
      cp -r config/ $out
      '';
  });
in with-config
