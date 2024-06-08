{
  description = "Nix flake for modaled";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      perSystem = { pkgs, ... }: {
        packages.default = pkgs.stdenv.mkDerivation {
          name = "modaled";
          src = ./.;
          buildInputs = [
            (pkgs.emacsWithPackages (epkgs: with epkgs; [ compat ]))
          ];
          buildPhase = ''
            emacs -L . --batch -f batch-byte-compile *.el
          '';
          installPhase = ''
            LISPDIR=$out/share/emacs/site-lisp
            install -d $LISPDIR
            install *.el *.elc $LISPDIR
          '';
        };
      };
    };
}
