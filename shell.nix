let
  pkgs = import <nixpkgs> {};
  dhall = import (fetchTarball {
    url = https://hydra.dhall-lang.org/jobset/dhall-haskell/master/channel/latest/nixexprs.tar.bz2;
    sha256 = "01ii87xcfplg2ppz3pqqi61b9zvlynr3d6c9xjmlic49airr3vc7";
  }) {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.stack
    pkgs.dhall
    dhall.linux-dhall-lsp-server
    pkgs.haskellPackages.ghcide
  ];
}
