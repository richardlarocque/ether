module Tests.HUnit.Interop(tests) where

import qualified Data.ByteString      as B
import           Data.Serialize
import           Ethereum.State.Block
import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.TestData       as D

parseBlock :: B.ByteString -> Assertion
parseBlock b = case runGet getBlock b of
                 Left err -> assertFailure err
                 Right _ -> return ()

parseBlocks :: [TestTree]
parseBlocks =
    let cases = zip [(1::Int)..] D.blocks in
    map (\(n, b) -> testCase ("Block " ++ show n) (parseBlock b)) cases

tests :: TestTree
tests = testGroup "Interop" [testGroup "Block Parsing" parseBlocks]
