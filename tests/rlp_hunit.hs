module Main where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Either
import Data.LargeWord
import Ethereum.Encoding.RLP
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.ByteString.Lazy as L

roundTripTest :: (Show a, Eq a) => (a -> L.ByteString) -> (L.ByteString -> a) -> a -> Test.Framework.Test
roundTripTest e d x = testCase (show x) $ x @=? (d.e) x

roundTripTestArray :: [Word8] -> Test.Framework.Test
roundTripTestArray = roundTripTest (\x -> runPut $ putArray x) (runGet $ getArray)

roundTripTestScalar :: Integer -> Test.Framework.Test
roundTripTestScalar = roundTripTest (\x -> runPut $ putScalar x) (runGet $ getScalar)

roundTripTestBinary :: (Binary a, Show a, Eq a) => a -> Test.Framework.Test
roundTripTestBinary = roundTripTest encode decode

failToRead :: (Binary a) => Get a -> [Word8] -> Test.Framework.Test
failToRead g bs = testCase (show bs) $ assert $ isLeft $ runGetOrFail g (L.pack bs)

data Seq1 = Seq1 Word256 Word256 [Word8] Word256
        deriving (Show, Eq)

instance Binary Seq1 where
        put (Seq1 s1 s2 b3 s4) = putSequence $ do 
                putScalarI s1
                putScalarI s2
                putArray b3
                putScalarI s4
        get = getSequence $ do
                s1 <- getScalar
                s2 <- getScalar
                b3 <- getArray
                s4 <- getScalar
                return $ Seq1 (fromIntegral s1) (fromIntegral s2) b3 (fromIntegral s4)

-- TODO: These should be augmented with quickcheck tests.
tests :: [Test.Framework.Test]
tests = [
        testGroup "ByteArray" [
                roundTripTestArray [10],
                roundTripTestArray [128],
                roundTripTestArray [1, 2],
                roundTripTestArray [0..56]
        ],
        testGroup "Scalar" [
                roundTripTestScalar 0,
                roundTripTestScalar 1,
                roundTripTestScalar 128,
                roundTripTestScalar 256,
                roundTripTestScalar 1024,
                roundTripTestScalar 0x1000000000
        ],
        testGroup "Sequence" [
                roundTripTestBinary (Seq1 10 10 [1,2,3] 45),
                roundTripTestBinary (Seq1 10 10 [1..100] 45),
                roundTripTestBinary (Seq1 10000 0 [1..100] 1234)
        ],
        testGroup "ParseFailures" [
                failToRead (getScalar) [0, 1],          -- Zero prefix.
                failToRead (getArray) [181, 1],         -- Bytes missing.
                failToRead (getArray) [185, 1],         -- Length missing.
                failToRead (getArray) [185, 1, 2],      -- Bytes missing.
                failToRead (get :: Get Seq1) [1],       -- Invalid header.
                failToRead (get :: Get Seq1) [193],     -- Missing bytes.
                failToRead (get :: Get Seq1) [248],     -- Missing length.
                failToRead (get :: Get Seq1) [249,10],  -- Missing bytes.
                failToRead (get :: Get Seq1) [193,1]    -- Sub-parse failure.
        ]
        ]

main ::  IO ()
main = defaultMain tests
