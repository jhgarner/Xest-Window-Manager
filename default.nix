{pkgs ? import <nixpkgs> {}}:

let
  gitignoreSrc = pkgs.fetchFromGitHub { 
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "c4662e662462e7bf3c2a968483478a665d00e717";
    sha256 = "sha256:1npnx0h6bd0d7ql93ka7azhj40zgjp815fw2r6smg8ch9p7mzdlx";
  };
  sourceFilesBySuffices = pkgs.lib.sourceFilesBySuffices;
  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;
  # If you don't pass in a pinned nixpkgs, this might fail. See xest.nix for an
  # example.
  xest-package = pkgs.pkgs.haskell.packages.ghc884.callPackage (import (./release.nix)) {inherit gitignoreSource sourceFilesBySuffices;};
  with-config = xest-package.overrideAttrs (old: {
    postInstall = ''
      ls
      ls config
      cp -r config/ $out
      sed -i "s#/usr/share/fonts/TTF/DejaVuSans.ttf#${pkgs.dejavu_fonts}/share/fonts/truetype/DejaVuSans.ttf#" $out/config/config.dhall
      sed -i "s#/usr/share/fonts/TTF/DejaVuSans.ttf#${pkgs.dejavu_fonts}/share/fonts/truetype/DejaVuSans.ttf#" $out/config/config.99.dhall
      '';
  });
in with-config
