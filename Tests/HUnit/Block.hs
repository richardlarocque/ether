module Tests.HUnit.Block(tests) where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Ethereum.Crypto
import Ethereum.State.Block
import Ethereum.State.Transaction
import Ethereum.State.Address
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.ByteString as B
import Data.ByteString.Lazy as L

roundTripTest :: (Show a, Eq a) => (a -> Put) -> Get a -> a -> Test.Framework.Test
roundTripTest p g x = testCase (show x) $
        do let enc = runPut $ p x
           case runGetOrFail g enc of
                   Left (_, _, err) -> assertFailure err
                   Right (rest, _, _) | not (L.null rest) -> assertFailure "Some bytes remain"
                   Right (_, _, v) -> x @=? v

roundTripBlockHeader :: BlockHeader -> Test.Framework.Test
roundTripBlockHeader = roundTripTest putBlockHeader getBlockHeader

roundTripTransactionReceipt :: TransactionReceipt -> Test.Framework.Test
roundTripTransactionReceipt = roundTripTest putTransactionReceipt getTransactionReceipt

roundTripBlock :: Block -> Test.Framework.Test
roundTripBlock = roundTripTest putBlock getBlock

pr1 :: PrivateAccount
pr1 = makePrivateAccount 1234

transactionReceipt1 :: TransactionReceipt
transactionReceipt1 = TransactionReceipt t 0 100
        where t = initContractCreation pr1 0 10 2 1000 B.empty

transactionReceipt2 :: TransactionReceipt
transactionReceipt2 = TransactionReceipt t 0 50
        where t = initMessageCall pr1 0xABAB 10 2 1000 (A 1234) B.empty

block1 :: Block
block1 = Block bh [transactionReceipt1, transactionReceipt2] []
        where bh = BlockHeader 1234 4321 (A 1234) 0 0 (2 ^ (22 :: Integer)) 1401778307 1 2 1000 150 B.empty 111

tests ::  [Test.Framework.Test]
tests = [
          testGroup "serializations" serializeTests
          ]

serializeTests ::  [Test.Framework.Test]
serializeTests =
        [  testGroup "BlockHeader" [
        roundTripBlockHeader genesisBlockHeader
        ], testGroup "TransactionReceipt" [
        roundTripTransactionReceipt transactionReceipt1,
        roundTripTransactionReceipt transactionReceipt2
        ],  testGroup "Block" [
        roundTripBlock block1
        ]
        ]

