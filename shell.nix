let
  pkgs = import <nixpkgs> {};
  dhall = import (fetchTarball {
    url = https://hydra.dhall-lang.org/jobset/dhall-haskell/master/channel/latest/nixexprs.tar.bz2;
    sha256 = "18qnlbq3ma1ipzvlsxg4r714aj6ckj1fc9mgxz9rhvqzpsmx74fv";
  }) {};
  nix-tools = import (fetchTarball {
    url = https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz;
    sha256 = "08sf4k9w6gn2scp86bcq0j27akjrxykf8dziy12dm61aqhwy948q";
  }) {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.termite
    pkgs.stack
    pkgs.dhall
    nix-tools.pkgs.haskell-nix.nix-tools.ghc884
    dhall.linux-dhall-lsp-server
    pkgs.haskellPackages.ghcide
  ];
}
