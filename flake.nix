{
  description = "Nix flake for modaled";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {
        packages = rec {
          modaled = pkgs.stdenv.mkDerivation {
            name = "modaled";
            src = pkgs.lib.sourceByRegex ./. [
              "^modaled\.el$"
            ];
            buildInputs = [
              (pkgs.emacsWithPackages (epkgs: []))
            ];
            # TODO: enable warning in next version
            buildPhase = ''
              emacs -L . --batch -f batch-byte-compile *.el 2> stderr.txt
              cat stderr.txt
              ! grep -q ': Warning:' stderr.txt
            '';
            installPhase = ''
              LISPDIR=$out/share/emacs/site-lisp
              install -d $LISPDIR
              install *.el *.elc $LISPDIR
            '';
          };
          default = modaled;
        };
      });
}
