{
  description = "cl-environment-variables flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-23.11";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {

        packages.${system} = {
          inputs = [pkgs.lispPackages.quicklisp];
          default = pkgs.mkShell {
            name = "cl-environment-variables";
            buildInputs = [pkgs.sbcl];
          };
        };

        devShell = pkgs.mkShell {
          inputs = [pkgs.lispPackages.quicklisp];
            name = "cl-environment-variables";
            buildInputs = [pkgs.sbcl];
        };
      });
}
