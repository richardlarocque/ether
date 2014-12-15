module Tests.Upstream.Common where

import           Control.Monad
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC8
import           Data.Either
import           Data.List
import           Numeric
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.JSON

testDataRoot :: String
testDataRoot = "eth_tests/"

groupDataTests :: String -> [Either String TestTree] -> Either String TestTree
groupDataTests label ts =
    case lefts ts of
      [] -> Right $ testGroup label (rights ts)
      errs -> Left $ formatErr errs
    where formatErr errs =
              label ++ ":\n" ++ intercalate "," (map ("\t" ++) errs)

filesAsTestGroup :: String -> [String]
                    -> (JSValue -> Maybe Assertion)
                    -> IO (Either String TestTree)
filesAsTestGroup name paths interp =
    liftM (groupDataTests name) $ mapM (flip readTestsFromFile interp) paths

readTestsFromFile :: String -> (JSValue -> Maybe Assertion)
                  -> IO (Either String TestTree)
readTestsFromFile path interpreter =
    do json <- readJSONFile path
       return $ maybe errMsg grouper $ makeTestCases interpreter json
    where errMsg = Left $ "Error reading file: " ++ path
          grouper = Right . testGroup path

readJSONFile :: String -> IO JSValue
readJSONFile path =
    do contents <- readFile path
       case decode contents of
         Error err -> fail (path ++ ": " ++ err)
         Ok value -> return value

makeTestCases :: (JSValue -> Maybe Assertion) -> JSValue -> Maybe [TestTree]
makeTestCases f x =
    do obj <- asObject x
       let list = fromJSObject obj
       let labels = map fst list
       let testData = map snd list
       assertions <- mapM f testData
       return $ zipWith testCase labels assertions

--- Helpers

asArray :: JSValue -> Maybe [JSValue]
asArray v = case v of
              JSArray arr -> Just arr
              _ -> Nothing

asObject :: JSValue -> Maybe (JSObject JSValue)
asObject v = case v of
               JSObject obj -> Just obj
               _ -> Nothing

--- Parsers

parseSimpleType :: JSValue -> Maybe B.ByteString
parseSimpleType obj =
    case obj of
      JSNull -> Just B.empty
      JSString jsStr ->
          let jsStr' = fromJSString jsStr in
          case stripPrefix "0x" (fromJSString jsStr) of
            Just hexStr -> parseHex hexStr
            Nothing -> Just $ BC8.pack jsStr'
      _ -> Nothing

parseHex :: String -> Maybe B.ByteString
parseHex hexStr =
    liftM B.pack $ sequence $ readHexByte hexStr
    where readHexByte (a:b:rest) =
              case readHex [a,b] of
                [(v,"")] -> Just v : readHexByte rest
                _ -> [Nothing]
          readHexByte [] = []
          readHexByte _  = [Nothing]
