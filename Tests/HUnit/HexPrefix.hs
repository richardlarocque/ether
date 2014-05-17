module Tests.HUnit.HexPrefix(tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Binary.Get
import Data.Binary.Put
import Data.Word.Odd
import Ethereum.Encoding.HexPrefix

roundTripTest :: [Word4] -> Bool -> Test.Framework.Test
roundTripTest ns b = testCase (show (ns,b)) $ ns @=? d (e ns b)
        where e a1 a2 = runPut $ putHexPrefixBytes a1 a2
              d = (runGet $ getHexPrefixBytes b)

-- TODO: These should be augmented with quickcheck tests.
tests :: [Test.Framework.Test]
tests = [
        testGroup "RoundTrip True" [
                roundTripTest [0..15] True,
                roundTripTest (reverse [0..15]) True,
                roundTripTest [1..15] True,
                roundTripTest (reverse [1..15]) True
        ],
        testGroup "RoundTrip False" [
                roundTripTest [0..15] False,
                roundTripTest (reverse [0..15]) False,
                roundTripTest [1..15] False,
                roundTripTest (reverse [1..15]) False
        ],
        testGroup "RoundTrip Edges" [
                roundTripTest [] False,
                roundTripTest [0] False,
                roundTripTest [15] False
        ]
        ]
