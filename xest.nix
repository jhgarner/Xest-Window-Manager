{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.xest;

  oldPkgs = import (builtins.fetchGit {
    url = "https://github.com/nixos/nixpkgs/";
    ref = "refs/heads/nixos-20.09";
    rev = "c5c6009fb436efe5732e07cd0e5692f495321752";
  }) {config.allowUnfree = config.nixpkgs.config.allowUnfree;};

  nixgl = import (builtins.fetchTarball https://github.com/guibou/nixGL/archive/master.tar.gz) {pkgs = oldPkgs;};

  xest-package = import (builtins.fetchGit {
    url = https://github.com/jhgarner/Xest-Window-Manager.git;
    ref = "master";
  }) {pkgs = oldPkgs;};
in {
  options.services.xest = {
    useIntel = mkOption {
      type = types.bool;
      default = false;
      description = ''
        When true, forces xest to use the mesa drives. Useful on Nvidia Optimus
        devices that break the heuristic.
      '';
    };
  };
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
          ${if cfg.useIntel then nixgl.nixGLCommon nixgl.nixGLIntel else nixgl.nixGLDefault}/bin/nixGL ${xest-package}/bin/xest-exe &
          waitPID=$!
        '';
      }];
    environment.etc.xest = {
      source = "${xest-package}/config";
    };
  };
}
