module Tests.Helpers where

import           Data.Serialize
import           Test.Tasty
import           Test.Tasty.HUnit

roundTripTest :: (Show a, Eq a) => (a -> Put) -> Get a -> a -> TestTree
roundTripTest p g x = testCase (show x) $
        do let enc = runPut $ p x
           case runGet g enc of
                   Left err -> assertFailure err
                   Right x' -> x @=? x'

genericRoundTripTest :: (Show a, Eq a, Serialize a) => a -> TestTree
genericRoundTripTest = roundTripTest put get
