{
  description = "PSNR : Peak Signal-to-Noise Ratio";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        zlibDev = pkgs.zlib.dev;
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            zlib
            pkg-config

            ghc
            cabal-install
          ];

          shellHook = ''
		  	cabal update
		  	cabal build
          '';
        };
      });
}
