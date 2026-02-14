{
  description = "Emacs AI Development Environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        packages = {
          aide-modeline = pkgs.emacsPackages.trivialBuild {
            pname = "aide-modeline";
            version = "0.1.0";
            src = ./aide-modeline.el;
          };
          aide-session-status = pkgs.emacsPackages.trivialBuild {
            pname = "aide-session-status";
            version = "0.1.0";
            src = ./aide-session-status.el;
            packageRequires = [ pkgs.emacsPackages.perspective ];
          };
          aide-persp-sidebar = pkgs.emacsPackages.trivialBuild {
            pname = "aide-persp-sidebar";
            version = "0.1.0";
            src = ./aide-persp-sidebar.el;
            packageRequires = [ pkgs.emacsPackages.perspective ];
          };
        };
      }
    );
}
