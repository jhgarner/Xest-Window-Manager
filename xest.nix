{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.xest;
  xest-package = import (builtins.fetchTarball https://github.com/jhgarner/Xest-Window-Manager/archive/master.tar.gz);
in {
  # TODO add options
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
          ${xest-package}/bin/xest-exe &
          waitPID=$!
        '';
      }];
  };
}
