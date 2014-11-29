module Tests.HUnit.Trie(tests) where

import           Data.Array
import qualified Data.ByteString          as B
import           Data.Char
import           Data.Maybe
import           Data.Word
import           Data.Word.Odd
import           Ethereum.Common
import           Ethereum.Storage.Context
import           Ethereum.Storage.Trie
import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.Helpers

import           Data.Serialize
import           Data.Serialize.Get
import           Data.Serialize.Put

item :: [Word8] -> B.ByteString
item = B.pack

key :: String -> [Word4]
key = nibbleize . map (fromIntegral.ord)

nullBranchArray :: Array Word4 TreeRef
nullBranchArray = listArray (0,15) (replicate 16 (TreeHash 0))

fullBranchArray1 :: Array Word4 TreeRef
fullBranchArray1 = listArray (0,15) (map TreeHash [0..16])

fullBranchArray2 :: Array Word4 TreeRef
fullBranchArray2 = listArray (0,15) (map (Serialized . B.singleton) [0..16])

serializeTests :: TestTree
serializeTests = testGroup "Serialization" [
        testGroup "Item" [
                genericRoundTripTest $ item [],
                genericRoundTripTest $ item [1],
                genericRoundTripTest $ item [128],
                genericRoundTripTest $ item [0..128]
                ],
        testGroup "TreeRef" [
                genericRoundTripTest $ Serialized (B.pack []),
                genericRoundTripTest $ Serialized (B.pack [1]),
                genericRoundTripTest $ Serialized (B.pack [128]),
                genericRoundTripTest $ Serialized (B.pack [0..128]),
                genericRoundTripTest $ TreeHash 1234
                ],
        testGroup "Tree Leaf" [
                genericRoundTripTest $ Leaf (key "abc") (item []),
                genericRoundTripTest $ Leaf (key "de") (item [1]),
                genericRoundTripTest $ Leaf (key "ff") (item [254]),
                genericRoundTripTest $ Leaf (key "\0") (item [128])
                ],
        testGroup "Tree Extension" [
                genericRoundTripTest $ Extension (key "abc") (TreeHash 10),
                genericRoundTripTest $ Extension (key "\x0f") (TreeHash 456),
                genericRoundTripTest $ Extension (key "\xff") (TreeHash 123),
                genericRoundTripTest $ Extension (key "zxy") (Serialized $ B.pack [128])
                ],
        testGroup "Tree Branch" [
                genericRoundTripTest $ Branch nullBranchArray Nothing,
                genericRoundTripTest $ Branch nullBranchArray  $ Just (item []),
                genericRoundTripTest $ Branch nullBranchArray  $ Just (item [1]),
                genericRoundTripTest $ Branch fullBranchArray1 $ Just (item [1..128]),
                genericRoundTripTest $ Branch fullBranchArray2 $ Just (item [0..10])
                ]
        ]

toBS :: String -> B.ByteString
toBS = B.pack . map (fromIntegral . ord)

putAndGetTest :: [(String, String)] -> TestTree
putAndGetTest pairs =
        let pairs' = map (\(a,b) -> (toBS a, toBS b)) pairs in
        testCase (show pairs) $ pairs' @=? putAndGet pairs'

putAndGet :: [(B.ByteString, B.ByteString)] -> [(B.ByteString, B.ByteString)]
putAndGet ps = getMany (putMany initContext ps) (map fst ps)

putMany :: Context -> [(B.ByteString, B.ByteString)] -> Context
putMany s ps = foldr (flip insertToTrie) s (reverse ps)

getMany :: Context -> [B.ByteString] -> [(B.ByteString, B.ByteString)]
getMany s = mapMaybe (\k -> lookupInTrie s k >>= \v -> return (k, v))

-- TODO: Do this more exhaustively.
-- TODO: Verify correctness of resulting trees.
insertTests ::  TestTree
insertTests = testGroup "Insertion" [
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

tests :: TestTree
tests = testGroup "Trie" [ serializeTests, insertTests ]
