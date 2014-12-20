module Tests.HUnit.Block(tests) where

import           Data.ByteString        as B
import           Ethereum.Builders
import           Ethereum.Crypto
import           Ethereum.Encoding.RLP
import           Ethereum.State.Address
import           Ethereum.State.Block
import           Test.Tasty
import           Tests.Helpers

-- roundTripBlockHeader :: BlockHeader -> TestTree
-- roundTripBlockHeader = roundTripTest blockHeaderToRLP blockHeaderFromRLP
--
-- roundTripTransactionReceipt :: TransactionReceipt -> TestTree
-- roundTripTransactionReceipt = roundTripTest putTransactionReceipt getTransactionReceipt

roundTripBlock :: Block -> TestTree
roundTripBlock = roundTripTest putRLP getRLP

pr1 :: PrivateKey
(Right pr1) = asPrivateKey 1234

transactionReceipt1 :: TransactionReceipt
transactionReceipt1 = TransactionReceipt t 0 100
        where t = initContractCreation pr1 0 10 2 1000 B.empty

transactionReceipt2 :: TransactionReceipt
transactionReceipt2 = TransactionReceipt t 0 50
        where t = initMessageCall pr1 0xABAB 10 2 1000 (A 1234) B.empty

block1 :: Block
block1 = Block bh [transactionReceipt1, transactionReceipt2] []
        where bh = BlockHeader 1234 4321 (A 1234) 0 0 (2 ^ (22 :: Integer)) 1 2 1000 150 1401778307 B.empty 111

tests ::  TestTree
tests = testGroup "Block" [serializeTests]

serializeTests :: TestTree
serializeTests = testGroup "Serialization" [
        --[  testGroup "BlockHeader" [
        --roundTripBlockHeader genesisBlockHeader
        --], testGroup "TransactionReceipt" [
        --roundTripTransactionReceipt transactionReceipt1,
        --roundTripTransactionReceipt transactionReceipt2
        --],
        testGroup "Block" [
        roundTripBlock block1
        ]
        ]
