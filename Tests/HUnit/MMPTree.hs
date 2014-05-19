module Tests.HUnit.MMPTree(tests) where

import Data.Array
import Data.Binary
import Data.Char
import Data.Word.Odd
import Ethereum.MMPTree.MMPTree
import qualified Data.ByteString.Lazy as L
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Ethereum.Common

roundTripTest :: (Show a, Eq a, Binary a) => a -> Test.Framework.Test
roundTripTest x = testCase (show x) $ x @=? (decode.encode) x

item :: [Word8] -> Item
item = Embedded . L.pack

key :: String -> [Word4]
key = nibbleize.(map (fromIntegral.ord))

nullBranchArray :: Array Word4 TreeRef
nullBranchArray = listArray (0,15) (replicate 16 (TreeHash 0))

fullBranchArray1 :: Array Word4 TreeRef
fullBranchArray1 = listArray (0,15) (map TreeHash [0..16])

fullBranchArray2 :: Array Word4 TreeRef
fullBranchArray2 = listArray (0,15) (map (Serialized . L.singleton) [0..16])

serialize_tests :: [Test.Framework.Test]
serialize_tests = testGroup "Serialization" [
        testGroup "Item" [
                roundTripTest $ item [],
                roundTripTest $ item [1],
                roundTripTest $ item [128],
                roundTripTest $ item [0..128]
                ],
        testGroup "TreeRef" [
                roundTripTest $ Serialized (L.pack []),
                roundTripTest $ Serialized (L.pack [1]),
                roundTripTest $ Serialized (L.pack [128]),
                roundTripTest $ Serialized (L.pack [0..128]),
                roundTripTest $ TreeHash 1234
                ],
        testGroup "Tree Leaf" [
                roundTripTest $ Leaf (key "abc") (item []),
                roundTripTest $ Leaf (key "de") (item [1]),
                roundTripTest $ Leaf (key "ff") (item [254]),
                roundTripTest $ Leaf (key "\0") (item [128])
                ],
        testGroup "Tree Extension" [
                roundTripTest $ Extension (key "abc") (TreeHash 10),
                roundTripTest $ Extension (key "\x0f") (TreeHash 456),
                roundTripTest $ Extension (key "\xff") (TreeHash 123),
                roundTripTest $ Extension (key "zxy") (Serialized $ L.pack [128])
                ],
        testGroup "Tree Branch" [
                roundTripTest $ Branch nullBranchArray Nothing,
                roundTripTest $ Branch nullBranchArray  $ Just (item []),
                roundTripTest $ Branch nullBranchArray  $ Just (item [1]),
                roundTripTest $ Branch fullBranchArray1 $ Just (item [1..128]),
                roundTripTest $ Branch fullBranchArray2 $ Just (item [0..10])
                ]
        ]

insert_tests = testGroup "Insertion" [
        testGroup "Insert" [
                putAndGetTest [("a", 1234)] "a"
                ]
        ]

tests :: [Test.Framework.Test]
tests = [ serialize_tests, insert_tests ]
