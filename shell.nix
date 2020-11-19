let
  pkgs = import <nixpkgs> {};
  dhall = import (fetchTarball {
    url = https://hydra.dhall-lang.org/jobset/dhall-haskell/master/channel/latest/nixexprs.tar.bz2;
    # sha256 = "0636c1lz1ndsdskgvqcbrfx2bav6hycdwlbsbyv0lvl9ykca1mk4";
  }) {};
  nix-tools = import (fetchTarball {
    url = https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz;
    sha256 = "08sf4k9w6gn2scp86bcq0j27akjrxykf8dziy12dm61aqhwy948q";
  }) {};
  # hls = (import (builtins.fetchTarball "https://github.com/shajra/nix-hls/tarball/master") {ghcVersion = "ghc884";});
in
pkgs.mkShell {
  buildInputs = [
    pkgs.stack
    pkgs.dhall
    pkgs.glibc
    (pkgs.haskell-language-server.override { supportedGhcVersions = [ "884" ]; })
    # pkgs.haskellPackages.ghcide
    dhall.linux-dhall-lsp-server
    pkgs.haskellPackages.ormolu
    pkgs.haskellPackages.floskell
  ];
  shellHook = ''
  export TEST="IN HERE!"
  export LD_LIBRARY_PATH=$LD_LIBRARY_PATH''${LD_LIBRARY_PATH:+:}${pkgs.glibc}/lib
  '';
}
