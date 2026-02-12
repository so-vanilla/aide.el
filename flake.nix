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
          aide-persp-side-bar = pkgs.emacsPackages.trivialBuild {
            pname = "aide-persp-side-bar";
            version = "0.1.0";
            src = ./aide-persp-side-bar.el;
            packageRequires = [ pkgs.emacsPackages.perspective ];
          };
        };
      }
    );
}
