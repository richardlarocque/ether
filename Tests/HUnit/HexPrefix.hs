module Tests.HUnit.HexPrefix(tests) where

import           Data.Word.Odd
import           Ethereum.Encoding.HexPrefix
import           Test.Tasty
import           Test.Tasty.HUnit

hexRoundTripTest :: [Word4] -> Bool -> TestTree
hexRoundTripTest ns b = testCase (show (ns, b)) $
        do let enc = asHexPrefix ns b
           case unHexPrefix enc of
                   Left err -> assertFailure err
                   Right (HPArray ns' b') -> (ns,b) @=? (ns',b')

-- TODO: These should be augmented with quickcheck tests.
tests :: TestTree
tests = testGroup "HexPrefix" [
        testGroup "RoundTrip True" [
                hexRoundTripTest [0..15] True,
                hexRoundTripTest (reverse [0..15]) True,
                hexRoundTripTest [1..15] True,
                hexRoundTripTest (reverse [1..15]) True
        ],
        testGroup "RoundTrip False" [
                hexRoundTripTest [0..15] False,
                hexRoundTripTest (reverse [0..15]) False,
                hexRoundTripTest [1..15] False,
                hexRoundTripTest (reverse [1..15]) False
        ],
        testGroup "RoundTrip Edges" [
                hexRoundTripTest [] False,
                hexRoundTripTest [0] False,
                hexRoundTripTest [15] False
        ]
        ]
