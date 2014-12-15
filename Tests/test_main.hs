module Main where

import           Crypto.Secp256k1
import           Test.Tasty
import qualified Tests.HUnit
import qualified Tests.Upstream


main ::  IO ()
main = withSecp256k1Initialized $ do
         let normalTests = Tests.HUnit.group
         parsedResult <- Tests.Upstream.ioGroup
         case parsedResult of
           Left err -> print err
           Right parsedTests ->
               defaultMain $ testGroup "All" [normalTests, parsedTests]
