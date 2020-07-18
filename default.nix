let
  pkgs = import <nixpkgs> {};
  stackToNix = pkgs.callPackage (fetchTarball https://github.com/jhgarner/Xest-Window-Manager/archive/master.tar.gz) { };
in
stackToNix {
  # root: the path with stack.yaml. you may want to filter this. for example using nix-gitignore.
  root = ./.;
  # shell: get the .env instead of the nix-build derivation. recommended that you do this with shell.nix/default.nix.
  # see https://github.com/DisciplinaOU/disciplina/blob/master/shell.nix
  shell = false;
  # you shouldn't need overrides, but you can ;)
  # overrides = final: previous: with pkgs.haskell.lib; {
  #   qtah = overrideCabal previous.qtah (super: {
  #     libraryToolDepends = with pkgs.qt5; [ qtbase qttools ];
  #   });
  # };
}
