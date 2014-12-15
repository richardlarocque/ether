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

testFiles :: [String]
testFiles = [ "trietest.json" ]

testPaths :: [String]
testPaths = map (\x -> testDataRoot ++ testPath ++ x) testFiles

tests :: IO (Either String TestTree)
tests = filesAsTestGroup "Trie" testPaths toTrieTest

toTrieTest :: JSValue -> Maybe Assertion
toTrieTest obj =
    do (root, pairs) <- parseTrieTest obj
       return $ makeTrieTest root pairs

parseTrieTest :: JSValue -> Maybe (Word256, [(B.ByteString, B.ByteString)])
parseTrieTest val =
    do obj <- asObject val
       let assoc = fromJSObject obj
       inputObj <- lookup "in" assoc
       inputArray <- asArray inputObj
       inputPairs <- mapM parsePair inputArray
       rootObj <- lookup "root" assoc
       expectedRoot <- liftM decode256be $ parseSimpleType rootObj
       return (expectedRoot, inputPairs)

makeTrieTest :: Word256 -> [(B.ByteString, B.ByteString)] -> Assertion
makeTrieTest r ps = r @=? rootHash (foldl insertToTrie initContext ps)

parsePair :: JSValue -> Maybe (B.ByteString, B.ByteString)
parsePair obj =
    do pairObj <- asArray obj
       when (2 /= length pairObj) Nothing
       k <- parseSimpleType $ pairObj !! 0
       v <- parseSimpleType $ pairObj !! 1
       return (k, v)
