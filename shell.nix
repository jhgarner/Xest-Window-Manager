let
  pkgs = import <nixpkgs> {};
  dhall = import (fetchTarball {
    url = https://hydra.dhall-lang.org/jobset/dhall-haskell/master/channel/latest/nixexprs.tar.bz2;
    sha256 = "127r3nabcc4snqy1jap0yyas2cfjyiz5qxhz5dyjd56nznkfda5a";
  }) {};
  nix-tools = import (fetchTarball {
    url = https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz;
    sha256 = "08sf4k9w6gn2scp86bcq0j27akjrxykf8dziy12dm61aqhwy948q";
  }) {};
  hls = (import (builtins.fetchTarball "https://github.com/shajra/nix-hls/tarball/master") {ghcVersion = "ghc884";});
in
pkgs.mkShell {
  buildInputs = [
    pkgs.termite
    pkgs.stack
    pkgs.dhall
    pkgs.glibc
    hls.hls
    pkgs.haskellPackages.ghcide
    dhall.linux-dhall-lsp-server
  ];
}
