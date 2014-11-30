module Tests.HUnit.Interop(tests) where

import           Control.Monad
import qualified Data.ByteString            as B
import           Data.Either
import           Data.Serialize
import           Ethereum.State.Block
import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.TestData             as D

import           Ethereum.BlockVerification

singleBlockTest :: String -> ((B.ByteString, Either String Block) -> Assertion) -> TestTree
singleBlockTest name test = testGroup name $ map f (zip [1..] D.blocks)
    where f (n, b) = testCase ("Block " ++ show n) (test (b, runGet getBlock b))

checkParse :: (B.ByteString, Either String Block) -> Assertion
checkParse (_, b) = case b of
                      Left err -> assertFailure err
                      Right _ -> return ()

checkReserialize :: (B.ByteString, Either String Block) -> Assertion
checkReserialize (orig, p) =
    case p of
      Left _   -> assertFailure "bad parse"
      Right p' -> orig @=? runPut (putBlock p')

singleBlockTest' :: String -> (Block -> Assertion) -> TestTree
singleBlockTest' name test = singleBlockTest name test'
    where test' (_, Left _ ) = assertFailure "bad parse"
          test' (_, Right b) = test b

checkNonce :: Block -> Assertion
checkNonce b = unless (isBlockNonceValid $ header b) (assertFailure "bad nonce")

tests :: TestTree
tests = testGroup "Interop" [
         singleBlockTest "Parse" checkParse,
         singleBlockTest "Re-Serialize" checkReserialize,
         singleBlockTest' "Nonce" checkNonce
        ]
