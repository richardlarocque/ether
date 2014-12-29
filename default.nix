{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:
let
  inherit (haskellPackages) cabal cabalInstall
    largeword vector cryptohash
    cryptoRandom cryptoPubkey byteable
    cereal mtl dataLens
    tasty tastyHunit tastyQuickcheck
    json;

    # array containers bytestring OddWord

in cabal.mkDerivation (self: {
  pname = "ether";
  version = "0.1.0";
  src = ./.;
  buildDepends = [
    # As imported above
    largeword vector cryptohash
    cryptoRandom cryptoPubkey byteable
    cereal mtl dataLens
    tasty tastyHunit tastyQuickcheck
    json
  ];
  buildTools = [ cabalInstall ];
  enableSplitObjs = false;
})
