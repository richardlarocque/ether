module Tests.HUnit.Interop(tests) where

import           Control.Monad
import           Data.Either
import           Data.Serialize
import           Ethereum.State.Block
import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.TestData             as D

import           Ethereum.BlockVerification

numberedBlocks :: [(Integer, Either String Block)]
numberedBlocks = zip [1..] $ map (runGet getBlock) D.blocks

singleBlockTest :: String -> (Either String Block -> Assertion) -> TestTree
singleBlockTest name test = testGroup name $ map f numberedBlocks
    where f (n, b) = testCase ("Block " ++ show n) (test b)

checkParse :: Either String Block -> Assertion
checkParse b = case b of
                 Left err -> assertFailure err
                 Right _ -> return ()

singleBlockTest' :: String -> (Block -> Assertion) -> TestTree
singleBlockTest' name test = singleBlockTest name test'
    where test' (Left _ ) = assertFailure "bad parse"
          test' (Right b) = test b

checkNonce :: Block -> Assertion
checkNonce b = unless (isBlockNonceValid $ header b) (assertFailure "bad nonce")

tests :: TestTree
tests = testGroup "Interop" [
         singleBlockTest "Parse" checkParse,
         singleBlockTest' "Nonce" checkNonce
        ]
