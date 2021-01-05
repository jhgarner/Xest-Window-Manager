{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.xest;

  oldPkgs = import (builtins.fetchGit {
    url = "https://github.com/nixos/nixpkgs/";
    ref = "refs/heads/nixos-20.09";
    rev = "c5c6009fb436efe5732e07cd0e5692f495321752";
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # README: This is set to true to make nixGLDefault happy and to handle Nvidia drivers. Remove this line if you don't want unfree software!
    # You might need to change nixGLDefault to nixGLIntel or something else if you're not using a Nvidia card and don't want unfree software.
    # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  }) {config.allowUnfree = true;};

  nixgl = import (builtins.fetchTarball https://github.com/guibou/nixGL/archive/master.tar.gz) {pkgs = oldPkgs;};

  xest-package = import (builtins.fetchGit {
    url = https://github.com/jhgarner/Xest-Window-Manager.git;
    ref = "master";
  }) {pkgs = oldPkgs;};
in {
  # options.services.xest = {
  #   enable = mkOption {
  #     type = types.bool;
  #     default = false;
  #     description = ''
  #       When enabled, adds Xest to the list of desktop environments in your
  #       display manager.
  #     '';
  #   };
  # };

  config = {
    services.xserver.windowManager.session = [{
        name  = "xest-git";
        start = ''
          ${nixgl.nixGLDefault}/bin/nixGLDefault ${xest-package}/bin/xest-exe &> /tmp/xest.error &
          waitPID=$!
        '';
      }];
  };
}
