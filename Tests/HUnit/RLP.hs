module Tests.HUnit.RLP(tests) where

import qualified Data.ByteString       as B
import           Data.Char
import           Data.Either
import           Data.LargeWord
import           Data.Serialize
import           Data.Word
import           Ethereum.Encoding.RLP
import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.Helpers

arrayRoundTripTest :: B.ByteString -> TestTree
arrayRoundTripTest = roundTripTest putArray getArray

scalarRoundTripTest :: Integer -> TestTree
scalarRoundTripTest = roundTripTest putScalar getScalar

failToReadRaw :: Get a -> B.ByteString -> TestTree
failToReadRaw g bs = testCase (show bs) $ assert $ isLeft $ runGet g bs

failToRead :: (Serialize a) => Get a -> [Word8] -> TestTree
failToRead g bs = testCase (show bs) $
                  assert $ isLeft $ runGet g (B.pack bs)

data Seq1 = Seq1 Word256 Word256 [Word8] Word256
        deriving (Show, Eq)

instance Serialize Seq1 where
        put (Seq1 s1 s2 b3 s4) = putSequence $ do
                putScalar256 s1
                putScalar256 s2
                putArray (B.pack b3)
                putScalar256 s4
        get = getSequence $ do
                s1 <- getScalar
                s2 <- getScalar
                b3 <- getArray
                s4 <- getScalar
                return $ Seq1 (fromIntegral s1) (fromIntegral s2)
                           (B.unpack b3) (fromIntegral s4)

-- TODO: These should be augmented with quickcheck tests.
tests :: TestTree
tests = testGroup "RLP" [
        testGroup "ByteArray" [
                arrayRoundTripTest $ B.pack [10],
                arrayRoundTripTest $ B.pack [128],
                arrayRoundTripTest $ B.pack [1, 2],
                arrayRoundTripTest $ B.pack [0..56]
        ],
        testGroup "Scalar" [
                scalarRoundTripTest 0,
                scalarRoundTripTest 1,
                scalarRoundTripTest 128,
                scalarRoundTripTest 256,
                scalarRoundTripTest 1024,
                scalarRoundTripTest 0x1000000000
        ],
        testGroup "Sequence" [
                genericRoundTripTest (Seq1 10 10 [1,2,3] 45),
                genericRoundTripTest (Seq1 10 10 [1..100] 45),
                genericRoundTripTest (Seq1 10000 0 [1..100] 1234)
        ],
        testGroup "ParseFailures" [
                failToRead getScalar [0, 1],          -- Zero prefix.
                failToRead getArray [181, 1],         -- Bytes missing.
                failToRead getArray [185, 1],         -- Length missing.
                failToRead getArray [185, 1, 2],      -- Bytes missing.
                failToRead (get :: Get Seq1) [1],       -- Invalid header.
                failToRead (get :: Get Seq1) [193],     -- Missing bytes.
                failToRead (get :: Get Seq1) [248],     -- Missing length.
                failToRead (get :: Get Seq1) [249,10],  -- Missing bytes.
                failToRead (get :: Get Seq1) [193,1]    -- Sub-parse failure.
        ],
        testGroup "HardCoded" [
            -- Sequences should not be successfully read as arrays.
            failToReadRaw getArray $ asByteString
                "\198\197\&2\131zed\128\128\128\128\128\128\128\128\128\131xyz"
        ]
        ]

asByteString :: String -> B.ByteString
asByteString = B.pack . map (fromIntegral.ord)
