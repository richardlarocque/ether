module Tests.Upstream.RLP(tests) where

import           Control.Monad
import qualified Data.ByteString as B
import           Data.Char
import           Data.Serialize
import           Ethereum.Encoding.RLP
import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.Upstream.Common
import           Text.JSON

testPath :: String
testPath = "BasicTests/"

mkPath :: String -> String
mkPath = (++) (testDataRoot ++ testPath)

tests :: IO (Either String TestTree)
tests = liftM (groupDataTests "RLP") $ sequence [
  readTestsFromFile "RLP" (mkPath "rlptest.json")
  (liftM makeTest . parseRLPTest)]

parseRLPTest :: JSValue -> Maybe (RLP, B.ByteString)
parseRLPTest val =
  do obj <- asObject val
     let assoc = fromJSObject obj
     in' <- parseRLPValue =<< lookup "in" assoc
     out' <- parseHexString =<< lookup "out" assoc
     return (in', out')

parseRLPValue :: JSValue -> Maybe RLP
parseRLPValue (JSString s)
  | (not $ null $ fromJSString s) && (head $ fromJSString s) == '#' =
      return $ toRLP $ (read :: String -> Integer) $ tail $ fromJSString s
parseRLPValue (JSString s) =
  return $ toRLP $ B.pack $ map (fromIntegral . ord) $ fromJSString s
parseRLPValue r@(JSRational _ _) =
  liftM toRLP $ parseInteger r
parseRLPValue (JSArray vs) =
  liftM Group $ mapM parseRLPValue vs
parseRLPValue _ = Nothing

makeTest :: (RLP, B.ByteString) -> Assertion
makeTest (r, enc) = enc @=? (runPut . put) r
