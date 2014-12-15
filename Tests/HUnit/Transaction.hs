module Tests.HUnit.Transaction(tests) where

import qualified Data.ByteString                  as B
import           Ethereum.Builders
import           Ethereum.Crypto
import           Ethereum.State.Account
import           Ethereum.State.Address
import           Ethereum.State.Transaction
import           Ethereum.Storage.Trie            (zeroRef)
import           Ethereum.TransactionVerification
import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.Helpers

roundTripTransaction :: Transaction -> TestTree
roundTripTransaction = roundTripTest putTransaction getTransaction

roundTripContractCreation :: ContractCreation -> TestTree
roundTripContractCreation =
    roundTripTest putContractCreation getContractCreation

roundTripMessageCall :: MessageCall -> TestTree
roundTripMessageCall = roundTripTest putMessageCall getMessageCall

cc :: PrivateKey -> Integer -> Integer -> Integer -> Integer -> B.ByteString
   -> Transaction
cc = initContractCreation

acc1234 :: PrivateKey
(Right acc1234) = asPrivateKey 1234

addr1234 :: Address
addr1234 = privateToAddress acc1234

ccWithInitialGas :: Integer -> Transaction
ccWithInitialGas gl = cc acc1234 0 10 1 gl (B.pack [0..9])

ccWithNonce :: Integer -> Transaction
ccWithNonce n = cc acc1234 n 10 1 1000 (B.pack [0..9])

accWithNonce :: Integer -> Account
accWithNonce n = Account n 100 zeroRef NullCodeHash

accWithBalance :: Integer -> Account
accWithBalance b = Account 10 b zeroRef NullCodeHash

ccWithExpenses :: Integer -> Integer -> Integer -> Transaction
ccWithExpenses v gp gl = cc acc1234 10 v gp gl (B.pack [0..9])

mc :: PrivateKey -> Integer -> Integer -> Integer -> Integer -> Address
   -> B.ByteString -> Transaction
mc = initMessageCall

validGasCheck :: String -> Transaction -> Bool -> TestTree
validGasCheck s t e = testCase s $ e @=? isGasValid t

nonceCheck :: String -> Transaction -> Account -> Bool -> TestTree
nonceCheck s t a e = testCase s $ e @=? isNonceValid t a

upFrontCostCheck :: String -> Transaction -> Integer -> TestTree
upFrontCostCheck s t e = testCase s $ e @=? upFrontCost t

balanceCheck :: String -> Transaction -> Account -> Bool -> TestTree
balanceCheck s t a e = testCase s $ e @=? isBalanceAvailable t a

senderCheck :: String -> Address -> Transaction -> TestTree
senderCheck msg sender transaction =
    testCase msg $
      case transactionSender transaction of
        Nothing -> assertFailure "Fetching sender failed"
        Just sender' -> assertEqual "Sender matching" sender sender'

tests ::  TestTree
tests = testGroup "Transaction" [ verifyTests, serializeTests ]

verifyTests :: TestTree
verifyTests = testGroup "Verify"
      [ testGroup "InitialGasCheck" [
        validGasCheck "0"     (ccWithInitialGas 0)    False,
        validGasCheck "10"    (ccWithInitialGas 10)   False,
        validGasCheck "1000"  (ccWithInitialGas 1000) True
        ],
        testGroup "NonceCheck" [
        nonceCheck "ne"      (ccWithNonce  5) (accWithNonce  7)  False,
        nonceCheck "smaller" (ccWithNonce 10) (accWithNonce  9)  False,
        nonceCheck "eq"      (ccWithNonce 10) (accWithNonce 10)  True,
        nonceCheck "greater" (ccWithNonce 10) (accWithNonce 11)  False
        ],
        testGroup "upFrontCost" [
        upFrontCostCheck "110" (ccWithExpenses 10 1 100) 110,
        upFrontCostCheck "199" (ccWithExpenses 99 1 100) 199,
        upFrontCostCheck "299" (ccWithExpenses 99 2 100) 299,
        upFrontCostCheck "301" (ccWithExpenses 99 2 101) 301
        ],
        testGroup "BalanceCheck" [
        balanceCheck "smaller"
                         (ccWithExpenses 10 1 100) (accWithBalance 109) False,
        balanceCheck "eq"
                         (ccWithExpenses 10 1 100) (accWithBalance 110) True,
        balanceCheck "greater"
                         (ccWithExpenses 10 1 100) (accWithBalance 111) True
        ],
        testGroup "RecoverAddress" [
          senderCheck "case 1" addr1234 (cc acc1234 10 10000 1 10 B.empty),
          senderCheck "case 2" addr1234 (mc acc1234 10 10000 1 10 (A 10) B.empty)
        ]]

serializeTests ::  TestTree
serializeTests = testGroup "Serialize"
        [  testGroup "ContractCreation" [
        roundTripContractCreation $ ContractCreation B.empty
        ], testGroup "MessageCall" [
        roundTripMessageCall $ MessageCall zeroAddress B.empty
        ], testGroup "TransactionCC" [
        roundTripTransaction $ cc acc1234 10 10000 1 10 B.empty,
        roundTripTransaction $ cc acc1234 10 10000 1 10 (B.replicate 10 0),
        roundTripTransaction $ cc acc1234 10 10000 1 10 (B.replicate 10 0xff),
        let v = 2^(255 :: Integer) in
            roundTripTransaction $ cc acc1234 v v v v B.empty
        ], testGroup "TransactionMC" [
        roundTripTransaction $ mc acc1234 10 10000 1 10 (A 10) B.empty,
        roundTripTransaction $
            mc acc1234 10 10000 1 10 (A 10) (B.replicate 10 0),
        roundTripTransaction $
            mc acc1234 10 10000 1 10 (A 10) (B.replicate 10 0xff),
        let v = 2^(255 :: Integer) in
            roundTripTransaction $ mc acc1234 v v v v (A 10) B.empty
        ]
        ]
