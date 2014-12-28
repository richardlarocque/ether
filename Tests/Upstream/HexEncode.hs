module Tests.Upstream.HexEncode(tests) where

import           Control.Monad
import           Data.Bits
import qualified Data.ByteString as B
import           Data.Char
import           Data.Word
import           Ethereum.Encoding.HexPrefix
import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.Upstream.Common
import           Text.JSON

testPath :: String
testPath = "BasicTests/"

mkPath :: String -> String
mkPath = (++) (testDataRoot ++ testPath)

tests :: IO (Either String TestTree)
tests = liftM (groupDataTests "HexEncode") $ sequence [
  readTestsFromFile "HexEncode" (mkPath "hexencodetest.json")
  (liftM makeTest . parseHexEncodeTest)]

parseHexEncodeTest :: JSValue -> Maybe ([Word8], Bool, B.ByteString)
parseHexEncodeTest val =
  do obj <- asObject val
     let assoc = fromJSObject obj
     seqObj <- lookup "seq" assoc
     termObj <- lookup "term" assoc
     outObj <- lookup "out" assoc
     seq' <- parseByteSequence seqObj
     term' <- parseBool termObj
     out' <- parseNibbles outObj
     return (seq', term', out')

parseNibbles :: JSValue -> Maybe B.ByteString
parseNibbles (JSString s) =
  do let s' = fromJSString s
     unless (all isDigit s') Nothing
     let nibbles = map (fromIntegral . digitToInt) s'
     return $  B.pack $ squash nibbles
  where squash (h:l:rest) = ((h `shiftL` 4) .|. l) : (squash rest)
        squash [] = []
        squash _ = error "unpair list"
parseNibbles _ = Nothing

makeTest :: ([Word8], Bool, B.ByteString) -> Assertion
makeTest (seq', term, out) = out @=? asHexPrefix seq' term
