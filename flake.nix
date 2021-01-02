{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
  inputs.flake-utils.url = github:poscat0x04/flake-utils;

  outputs = { self, nixpkgs, flake-utils, ... }: with flake-utils;
    eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        in
          with pkgs;
          {
            devShell = hackage-api-dev.envFunc { withHoogle = true; };
            defaultPackage = hackage-api;
          }
    ) // {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages;
          hackage-api = hpkgs.callCabal2nix "hackage-api" ./. {};
        in
          with super; with haskell.lib;
          {
            inherit hackage-api;
            hackage-api-dev = addBuildTools hackage-api [
              haskell-language-server
              cabal-install
            ];
          };
    };
}
