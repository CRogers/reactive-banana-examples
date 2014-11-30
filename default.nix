{ pkgs ? (import <nixpkgs> {})
, haskellPackages ? pkgs.haskellPackages_ghc783
}:

haskellPackages.cabal.mkDerivation (self: {
  pname = "reactive-banana-test";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = with haskellPackages; [ cabalInstall reactiveBanana ];
  hyperlinkSource = false;
  isLibrary = true;
  isExecutable = false;
})
