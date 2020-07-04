let
  pkgs = import <nixpkgs> {};
  dhall = import (fetchTarball {
    url = https://hydra.dhall-lang.org/jobset/dhall-haskell/master/channel/latest/nixexprs.tar.bz2;
    sha256 = "1zqs273k1iyjr4ldl99m6px27n3cn8fznihhknw4278bfpbsb8sd";
  }) {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.termite
    pkgs.stack
    pkgs.dhall
    dhall.linux-dhall-lsp-server
    pkgs.haskellPackages.ghcide
  ];
}
