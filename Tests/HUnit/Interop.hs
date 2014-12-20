module Tests.HUnit.Interop(tests) where

import           Control.Monad
import qualified Data.ByteString            as B
import           Data.Serialize
import           Ethereum.Encoding.RLP
import           Ethereum.State.Block
import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.TestData             as D

import           Ethereum.BlockVerification

singleBlockTest :: String -> ((B.ByteString, Either String Block) -> Assertion) -> TestTree
singleBlockTest name test = testGroup name $ map f (zip ([1..]::[Integer]) D.blocks)
    where f (n, b) = testCase ("Block " ++ show n) (test (b, runGet getRLP b))

checkParse :: (B.ByteString, Either String Block) -> Assertion
checkParse (_, b) = case b of
                      Left err -> assertFailure err
                      Right _ -> return ()

checkReserialize :: (B.ByteString, Either String Block) -> Assertion
checkReserialize (orig, p) =
    case p of
      Left _   -> assertFailure "bad parse"
      Right p' -> orig @=? runPut (putRLP p')

singleBlockTest' :: String -> (Block -> Bool) -> TestTree
singleBlockTest' name test = singleBlockTest name test'
    where test' (_, Left _ ) = assertFailure "bad parse"
          test' (_, Right b) = unless (test b) (assertFailure "test failed")

tests :: TestTree
tests = testGroup "Interop" [
              singleBlockTest "Parse" checkParse,
              singleBlockTest "Re-Serialize" checkReserialize,
              singleBlockTest' "Nonce" (isBlockNonceValid . header),
              singleBlockTest' "Uncles" isUnclesHashValid,
              singleBlockTest' "Receipts" isReceiptHashValid,
              singleBlockTest' "TransactionSignatures" areSignaturesValid
        ]
