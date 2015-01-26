{ pkgs ? (import ../nixpkgs {}) }:

let secp256k1 = pkgs.callPackage ../secp256k1_hs { inherit pkgs; };
in
pkgs.haskellngPackages.mkDerivation {
  pname = "ether";
  version = "0.1.20150102.0";
  src = ./.;
  buildDepends = with pkgs.haskellngPackages; [
    base
    array
    largeword
    containers
    cabal-install
    vector
    cryptohash
    crypto-random
    crypto-pubkey
    crypto-pubkey-types
    byteable
    bytestring
    cereal
    mtl
    data-lens
    secp256k1
  ];
  testDepends = with pkgs.haskellngPackages; [
    tasty
    tasty-hunit
    tasty-quickcheck
    json
  ];
  isLibrary = true;
  isExecutable = false;

  # extraLibraries = [ secp256k1 ];
  # doCheck = false;

  description = "Ethereum libraries written in Haskell";
  license = pkgs.stdenv.lib.licenses.gpl2Plus;
}
