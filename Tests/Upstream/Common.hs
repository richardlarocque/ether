module Tests.HUnit.Upstream.Common where

import           Control.Monad
import           Data.Either
import           Data.List
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.JSON

testDataRoot :: String
testDataRoot = "eth_tests"

groupDataTests :: String -> [Either String TestTree] -> Either String TestTree
groupDataTests label ts =
    case lefts ts of
      [] -> Right $ testGroup label (rights ts)
      errs -> Left $ formatErr errs
    where formatErr errs =
              label ++ ":\n" ++ intercalate "," (map ("\t" ++) errs)

fileAsTestGroup :: String -> [String]
                    -> (JSValue -> Maybe Assertion)
                    -> IO (Either String TestTree)
fileAsTestGroup name paths interp =
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
    do arr <- asArray x
       mapM (makeTestCase f) arr

makeTestCase :: (JSValue -> Maybe Assertion) -> JSValue -> Maybe TestTree
makeTestCase f x =
    do obj <- asObject x
       let assoc = fromJSObject obj
       when (length assoc /= 1) Nothing
       let label = (fst . head) assoc
       let testData = (snd . last) assoc
       assertion <- f testData
       return $ testCase label assertion

asArray :: JSValue -> Maybe [JSValue]
asArray v = case v of
              JSArray arr -> Just arr
              _ -> Nothing

asObject :: JSValue -> Maybe (JSObject JSValue)
asObject v = case v of
               JSObject obj -> Just obj
               _ -> Nothing
