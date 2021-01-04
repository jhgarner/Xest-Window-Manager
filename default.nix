let
  gitignoreSrc = pkgs.fetchFromGitHub { 
    owner = "hercules-ci";
    repo = "gitignore";
    # put the latest commit sha of gitignore Nix library here:
    rev = "c4662e662462e7bf3c2a968483478a665d00e717";
    # use what nix suggests in the mismatch message here:
    sha256 = "sha256:1npnx0h6bd0d7ql93ka7azhj40zgjp815fw2r6smg8ch9p7mzdlx";
  };
  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;
  # pkgs = import <nixpkgs> {};
  # Use 20.09 since unstable has moved to 8.10 and something's wrong with ghc 8.10 and Xest
  pkgs = import (builtins.fetchGit {
    url = "https://github.com/nixos/nixpkgs/";
    ref = "refs/heads/nixos-20.09";
    rev = "c5c6009fb436efe5732e07cd0e5692f495321752";
  }) {};
  xest-package = pkgs.pkgs.haskell.packages.ghc884.callPackage (import (./release.nix)) {inherit gitignoreSource;};
in xest-package
