module Tests.HUnit.HexPrefix(tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Ethereum.Encoding.HexPrefix

roundTripTest :: HPArray -> Test.Framework.Test
roundTripTest x = testCase (show x) $ x @=? (decodeHexPrefix.encodeHexPrefix) x

-- TODO: These should be augmented with quickcheck tests.
tests :: [Test.Framework.Test]
tests = [
        testGroup "RoundTrip True" [
                roundTripTest $ HPArray [0..15] True,
                roundTripTest $ HPArray (reverse [0..15]) True,
                roundTripTest $ HPArray [1..15] True,
                roundTripTest $ HPArray (reverse [1..15]) True
        ],
        testGroup "RoundTrip False" [
                roundTripTest $ HPArray [0..15] False,
                roundTripTest $ HPArray (reverse [0..15]) False,
                roundTripTest $ HPArray [1..15] False,
                roundTripTest $ HPArray (reverse [1..15]) False
        ],
        testGroup "RoundTrip Edges" [
                roundTripTest $ HPArray [] False,
                roundTripTest $ HPArray [0] False,
                roundTripTest $ HPArray [15] False
        ]
        ]
