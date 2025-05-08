{
  description = "Create files from templates";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils
  }:
  flake-utils.lib.eachDefaultSystem (system:
  let
    pkgs = nixpkgs.legacyPackages.${system};

    haskellPackages = pkgs.haskellPackages;

    mk =
      haskellPackages.callCabal2nix "mk" self rec {
        # dependency overrides go here
      };
  in {

    packages = {
      inherit mk;
      default = mk;
    };

    devShells.default = pkgs.mkShell {

      buildInputs = [
        pkgs.zlib
      ];

      packages = [
        haskellPackages.haskell-language-server
        haskellPackages.ghcid
        haskellPackages.cabal-install
      ];

      inputsFrom = builtins.attrValues self.packages.${system};
    };

  });

}
