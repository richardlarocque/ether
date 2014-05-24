module Tests.HUnit.Trie(tests) where

import Data.Array
import Data.Binary
import Data.Char
import Data.Maybe
import Data.Word.Odd
import Ethereum.Storage.Trie
import Ethereum.Storage.HashMap
import Ethereum.Storage.Context
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Ethereum.Common

roundTripTest :: (Show a, Eq a, Binary a) => a -> Test.Framework.Test
roundTripTest x = testCase (show x) $ x @=? (decode.encode) x

item :: [Word8] -> B.ByteString
item = B.pack

unpackItem :: B.ByteString -> [Word8]
unpackItem = B.unpack

key :: String -> [Word4]
key = nibbleize.(map (fromIntegral.ord))

nullBranchArray :: Array Word4 TreeRef
nullBranchArray = listArray (0,15) (replicate 16 (TreeHash 0))

fullBranchArray1 :: Array Word4 TreeRef
fullBranchArray1 = listArray (0,15) (map TreeHash [0..16])

fullBranchArray2 :: Array Word4 TreeRef
fullBranchArray2 = listArray (0,15) (map (Serialized . B.singleton) [0..16])

serialize_tests :: Test.Framework.Test
serialize_tests = testGroup "Serialization" [
        testGroup "Item" [
                roundTripTest $ item [],
                roundTripTest $ item [1],
                roundTripTest $ item [128],
                roundTripTest $ item [0..128]
                ],
        testGroup "TreeRef" [
                roundTripTest $ Serialized (B.pack []),
                roundTripTest $ Serialized (B.pack [1]),
                roundTripTest $ Serialized (B.pack [128]),
                roundTripTest $ Serialized (B.pack [0..128]),
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
                roundTripTest $ Extension (key "zxy") (Serialized $ B.pack [128])
                ],
        testGroup "Tree Branch" [
                roundTripTest $ Branch nullBranchArray Nothing,
                roundTripTest $ Branch nullBranchArray  $ Just (item []),
                roundTripTest $ Branch nullBranchArray  $ Just (item [1]),
                roundTripTest $ Branch fullBranchArray1 $ Just (item [1..128]),
                roundTripTest $ Branch fullBranchArray2 $ Just (item [0..10])
                ]
        ]

toBS :: String -> B.ByteString
toBS = B.pack . map (fromIntegral . ord)

putAndGetTest :: [(String, String)] -> Test.Framework.Test
putAndGetTest pairs =
        let pairs' = map (\(a,b) -> (toBS a, toBS b)) pairs in
        testCase (show pairs) $ pairs' @=? (putAndGet pairs')

putAndGet :: [(B.ByteString, B.ByteString)] -> [(B.ByteString, B.ByteString)]
putAndGet ps = getMany (putMany initContext ps) (map fst ps)  
        
putMany :: Context -> [(B.ByteString, B.ByteString)] -> Context
putMany s ps = foldr (flip insertToTrie) s (reverse ps)

getMany :: Context -> [B.ByteString] -> [(B.ByteString, B.ByteString)]
getMany s ks = mapMaybe (\k -> lookupInTrie s k >>= \v -> return (k, v)) ks

-- TODO: Do this more exhaustively.
-- TODO: Verify correctness of resulting trees.
insert_tests ::  Test.Framework.Test
insert_tests = testGroup "Insertion" [
        testGroup "InsertBranches" [
                putAndGetTest [("a", "xyz")],
                putAndGetTest [("a", "xyz"), ("ab", "zed")],
                putAndGetTest [("a", "xyz"), ("ab", "zed"), ("abc", "bar")],
                putAndGetTest [("a", "xyz"), ("abc", "bar")]
                ],
        testGroup "InsertWithPrefix" [
                putAndGetTest [("pre.a", "xyz")],
                putAndGetTest [("pre.a", "xyz"), ("pre.ab", "zed")],
                putAndGetTest [("pre.a", "xyz"), ("pre.ab", "zed"), ("pre.abc", "bar")],
                putAndGetTest [("pre.a", "xyz"), ("pre.abc", "bar")]
        ],
        testGroup "SomeBigItems" [
                putAndGetTest [("a", replicate 40 'c')],
                putAndGetTest [("a", replicate 40 'c'), ("abcd", "x")],
                putAndGetTest [("a", replicate 40 'c'), ("b", "x")]
        ],
        testGroup "SplittingExtensions" [
                putAndGetTest [("abc1xx", "x"), ("abc2zz", "z")],
                putAndGetTest [("abc1xx", "x"), ("abc2zz", "z"), ("ax", "foo")],
                putAndGetTest [("abc1xx", "x"), ("abc2zz", "z"), ("axe", "foo")],
                putAndGetTest [("abc1xx", "x"), ("abc2zz", "z"), ("abc3", "foo")],
                putAndGetTest [("abc1xx", "x"), ("abc2zz", "z"), ("zy", "foo")]
        ]
        ]

tests :: [Test.Framework.Test]
tests = [ serialize_tests, insert_tests ]
