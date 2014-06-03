module Tests.HUnit.Block(tests) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Ethereum.State.Block
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.ByteString.Lazy as L

roundTripTest :: (Show a, Eq a) => (a -> Put) -> Get a -> a -> Test.Framework.Test
roundTripTest p g x = testCase (show x) $
        do let enc = runPut $ p x
           case runGetOrFail g enc of
                   Left (_, _, err) -> assertFailure err
                   Right (rest, _, _) | not (L.null rest) -> assertFailure "Some bytes remain"
                   Right (_, _, v) -> x @=? v

roundTripBlockHeader :: BlockHeader -> Test.Framework.Test
roundTripBlockHeader = roundTripTest putBlockHeader getBlockHeader

tests ::  [Test.Framework.Test]
tests = [
          testGroup "serializations" serializeTests
          ]

serializeTests ::  [Test.Framework.Test]
serializeTests =
        [  testGroup "GenesisBlockHeader" [
        roundTripBlockHeader genesisBlockHeader
        ]
        ]
