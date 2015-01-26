{ pkgs ? (import ../nixpkgs {}) }:

let
  secp256k1 = pkgs.callPackage ../secp256k1_hs { inherit pkgs; };
  env = pkgs.haskellngPackages.ghcWithPackages (p: with p; [
    base
    array
    largeword
    containers
    vector
    cryptohash
    crypto-random
    crypto-pubkey
    crypto-pubkey-types
    byteable
    bytestring
    cereal
    mtl
    mtl_2_2_1
    data-lens
    secp256k1

    tasty
    tasty-hunit
    tasty-quickcheck
    json

    cabal-install
  ]);
in
  pkgs.myEnvFun {
    name = "ether-dev";
    buildInputs = [ env ];
  }
