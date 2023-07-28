{
  description = "A Waybar custom plugin to enable or disable adaptive sync in Sway";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system};
            haskellPackages = pkgs.haskell.packages.ghc928;
            packageName = "swankybar";
            jailbreakUnbreak = pkg: pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));
        in
        {
          packages.${packageName} = haskellPackages.callCabal2nix packageName self rec {};

          defaultPackage = self.packages.${system}.${packageName};
          devShell = pkgs.mkShell {
            buildInputs = with haskellPackages; [
              haskell-language-server
              cabal-install
              cabal2nix
            ] ++ [ pkgs.zlib ];
            inputsFrom = builtins.attrValues self.packages.${system};
          };
        }
      );
}
