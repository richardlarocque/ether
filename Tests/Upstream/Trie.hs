module Tests.HUnit.Common where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Test.HUnit.Upstream.Common

testPath :: String
testPath = [ "TrieTests" ]

testFiles :: [String]
testFiles = [ "trietest.json" ]

testPaths :: [String]
testPaths = map (\x -> testDataRoot ++ testPath ++ x) testFiles

tests :: IO (Either String TestTree)
tests = filesAsTestGroup "Trie" testPaths

toTrieTest :: JSObject -> Assertion
toTrieTest obj =
    case parseTrieTest of
      Nothing -> assertFailure "Parse failure"
      Just (pairs, root) -> runTrieTest root pairs

parseTrieTest :: JSObject -> Maybe ()
parseTrieTest obj =
    do let assoc = fromJSObject
       inputObj <- lookup assoc "in"
       inputArray <- asArray inputObj
       inputPairs <- mapM parsePair inputArray
       expectedRoot <- lookup assoc "root"
       (inputPairs, expectedRoot)

runTrieTest :: [(B.ByteString, B.ByteString)]
runTrieTest r ps = r @=? rootHash (foldl insertToTrie initContext ps)

parsePair obj :: JSObject -> Maybe (B.ByteString, B.ByteString)
parsePair obj =
    do pairObj <- asArray obj
       when (2 /= length pairObj) Nothing
       k <- parseSimpleType $ pairObj !! 0
       v <- parseSimpleType $ pairObj !! 1
       return (pairObj !! 0, pairObj !! 1)

parseSimpleType :: JSObject -> Maybe B.ByteString
parseSimpleType obj =
    case obj of
      JSNull -> B.empty
      JSString jsstr | "0x" isPrefixOf (fromJSString jsstr) -> asHex jsstr
      JSString jsstr -> asString jsstr
    where asHex s = readHex $ stripPrefix "0x" $ fromJSString s
          asString s = B.pack $ fromJSString s

parseHex :: JSString -> Maybe B.ByteString
parseHex jsstr =
    liftM B.pack $ readHex $ stripPrefix "0x" $ fromJSString s
    where readHex (a:b:rest) =
              case readHex [a,b] of
                [(v,"")] -> Just $ v:(readHex rest)
                _ -> Nothing
          readHex [] -> []
          readHex _ -> Nothing
