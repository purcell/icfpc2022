{ pkgs ? import <nixpkgs> {
  # Force Rosetta on ARM Macs, to let haskell-language-server work
  localSystem = if builtins.currentSystem == "aarch64-darwin"
                then "x86_64-darwin"
                else builtins.currentSystem; } }:

let
  ghc = pkgs.ghc.withPackages (p: with p; [ JuicyPixels juicy-draw microlens ]);
in
pkgs.mkShell {

  buildInputs = [ ghc pkgs.cabal-install pkgs.haskell-language-server ];

}
