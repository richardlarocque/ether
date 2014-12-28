module Tests.Upstream.Trie where

import           Control.Monad
import qualified Data.ByteString          as B
import           Data.LargeWord
import           Ethereum.Common
import           Ethereum.Storage.Context
import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.Upstream.Common
import           Text.JSON

testPath :: String
testPath = "TrieTests/"

mkPath :: String -> String
mkPath = (++) (testDataRoot ++ testPath)

tests :: IO (Either String TestTree)
tests = liftM (groupDataTests "Trie") $ sequence [
         readTestsFromFile "TrieTest" (mkPath "trietest.json")
                               (liftM makeTest . parseTrieTest),
         readTestsFromFile "TrieAnyOrderTest" (mkPath "trieanyorder.json")
                               (liftM makeTest . parseTrieAnyOrderTest)
        ]

parseTrieTest :: JSValue -> Maybe (Word256, [(B.ByteString, B.ByteString)])
parseTrieTest val =
    do obj <- asObject val
       let assoc = fromJSObject obj
       inputObj <- lookup "in" assoc
       inputArray <- asArray inputObj
       inputPairs <- mapM parseArrayPair inputArray
       rootObj <- lookup "root" assoc
       expectedRoot <- liftM decode256be $ parseSimpleType rootObj
       return (expectedRoot, inputPairs)

parseTrieAnyOrderTest :: JSValue
                      -> Maybe (Word256, [(B.ByteString, B.ByteString)])
parseTrieAnyOrderTest val =
    do obj <- asObject val
       let assoc = fromJSObject obj
       inputObj <- lookup "in" assoc
       inputKVs <- asObject inputObj
       let inputKVs' = fromJSObject inputKVs
       inputPairs <- mapM parseObjPair inputKVs'
       rootObj <- lookup "root" assoc
       expectedRoot <- liftM decode256be $ parseSimpleType rootObj
       return (expectedRoot, inputPairs)

makeTest :: (Word256, [(B.ByteString, B.ByteString)]) -> Assertion
makeTest (r, ps) = r @=? rootHash (foldl insertToTrie initContext ps)

parseArrayPair :: JSValue -> Maybe (B.ByteString, B.ByteString)
parseArrayPair arr =
    do pairObj <- asArray arr
       when (2 /= length pairObj) Nothing
       k <- parseSimpleType $ pairObj !! 0
       v <- parseSimpleType $ pairObj !! 1
       return (k, v)

parseObjPair :: (String, JSValue) -> Maybe (B.ByteString, B.ByteString)
parseObjPair obj =
    do k <- parseBytes $ fst obj
       v <- parseSimpleType $ snd obj
       return (k, v)
