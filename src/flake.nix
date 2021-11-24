{
  description = "R devShell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:

    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        R-packages = with pkgs.rPackages; [
          FSelector
          R_matlab
          RobustRankAggreg
          caret
          e1071
          furrr
          magrittr
          praznik
          tidyverse
        ];
        R-dev-packages = with pkgs.rPackages; [
          docopt
          git2r
          languageserver
        ];

        R-build = pkgs.rWrapper.override { packages = R-packages; };
        R-devShell = pkgs.rWrapper.override { packages = R-packages ++ R-dev-packages; };

        dev-packages = with pkgs; [
          pre-commit
        ];

      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            R-devShell
            dev-packages
          ];

          shellHook = ''
            mkdir -p "$(pwd)/_libs"
            export R_LIBS_USER="$(pwd)/_libs"
          '';
        };
      }
    );
}
