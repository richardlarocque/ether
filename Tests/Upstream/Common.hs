module Tests.Upstream.Common where

import           Control.Monad
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC8
import           Data.Either
import           Data.LargeWord
import           Data.List
import           Data.Ratio
import           Data.Word
import           Ethereum.Common
import           Numeric
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.JSON
import           Text.Read

testDataRoot :: String
testDataRoot = "eth_tests/"

groupDataTests :: String -> [Either String TestTree] -> Either String TestTree
groupDataTests label ts =
    case lefts ts of
      [] -> Right $ testGroup label (rights ts)
      errs -> Left $ formatErr errs
    where formatErr errs =
              label ++ ":\n" ++ intercalate "," (map ("\t" ++) errs)

readTestsFromFile :: String -> String -> (JSValue -> Maybe Assertion)
                  -> IO (Either String TestTree)
readTestsFromFile label path interpreter =
    do json <- readJSONFile path
       return $ maybe errMsg grouper $ makeTestCases interpreter json
    where errMsg = Left $ "Error reading file: " ++ path
          grouper = Right . testGroup label

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
      JSString jsStr -> parseBytes $ fromJSString jsStr
      _ -> Nothing

parseBytes :: String -> Maybe B.ByteString
parseBytes str =
    case stripPrefix "0x" str of
      Just x -> parseHex x
      Nothing -> Just $ BC8.pack str

asString :: JSValue -> Maybe String
asString (JSString s) = Just $ fromJSString s
asString _ = Nothing

parseWord256 :: JSValue -> Maybe Word256
parseWord256 = liftM decode256be . parseHexString

parseHexString :: JSValue -> Maybe B.ByteString
parseHexString x = asString x >>= parseHex

parseHex :: String -> Maybe B.ByteString
parseHex hexStr =
    liftM B.pack $ sequence $ readHexByte hexStr
    where readHexByte (a:b:rest) =
              case readHex [a,b] of
                [(v,"")] -> Just v : readHexByte rest
                _ -> [Nothing]
          readHexByte [] = []
          readHexByte _  = [Nothing]

mapJSArray :: (JSValue -> Maybe a) -> JSValue -> Maybe [a]
mapJSArray f (JSArray vs) = mapM f vs
mapJSArray _ _ = Nothing

parseByteSequence :: JSValue -> Maybe [Word8]
parseByteSequence = mapJSArray (liftM fromIntegral . parseInteger)

eitherToMaybe :: Either l r -> Maybe r
eitherToMaybe = either (const Nothing) Just

parseInteger :: JSValue -> Maybe Integer
parseInteger (JSRational _ x) | denominator x == 1 = Just $ numerator x
parseInteger (JSString x) = eitherToMaybe $ readEither $ fromJSString x
parseInteger _ = Nothing

parseBool :: JSValue -> Maybe Bool
parseBool (JSBool b) = Just b
parseBool _ = Nothing
