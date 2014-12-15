module Main where

import           Crypto.Secp256k1
import           Test.Tasty
import qualified Tests.HUnit
import qualified Tests.Upstream


main ::  IO ()
main = do

  withSecp256k1Initialized $ defaultMain tests
