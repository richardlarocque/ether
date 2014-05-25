module Tests.HUnit.Transaction(tests) where

import Crypto.Random
import Data.Binary.Put
import Data.Binary.Get
import Ethereum.Crypto
import Ethereum.Storage.Trie(zeroRef)
import Ethereum.State.Transaction
import Ethereum.State.Account
import Ethereum.SimpleTypes
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

roundTripTest :: (Show a, Eq a) => (a -> Put) -> (Get a) -> a -> Test.Framework.Test
roundTripTest p g x = testCase (show x) $
        do let enc = runPut $ p x
           case runGetOrFail g enc of
                   Left (_, _, err) -> assertFailure err
                   Right (rest, _, _) | not (L.null rest) -> assertFailure "Some bytes remain"
                   Right (_, _, v) -> x @=? v

roundTripTestIO :: (Show a, Eq a) => String -> (a -> Put) -> Get a -> IO a -> Test.Framework.Test
roundTripTestIO s p g x' = testCase s $
        do x <- x'
           let enc = runPut $ p x
           case runGetOrFail g enc of
                   Left (_, _, err) -> assertFailure err
                   Right (rest, _, _) | not (L.null rest) -> assertFailure "Some bytes remain"
                   Right (_, _, v) -> x @=? v


roundTripTransaction :: String -> IO Transaction -> Test.Framework.Test
roundTripTransaction s = roundTripTestIO s putTransaction getTransaction

roundTripSignature :: TSignature -> Test.Framework.Test
roundTripSignature = roundTripTest (putSignature) (getSignature)

roundTripContractCreation :: ContractCreation -> Test.Framework.Test
roundTripContractCreation = roundTripTest (putContractCreation) (getContractCreation)

roundTripMessageCall :: MessageCall -> Test.Framework.Test
roundTripMessageCall = roundTripTest (putMessageCall) (getMessageCall)

cc :: PrivateAccount -> Integer -> Integer -> Integer -> Integer -> B.ByteString -> IO Transaction
cc pr n v gv gl bs = do
        pool <- createEntropyPool
        let cprg = cprgCreate pool :: SystemRNG
        return $ initContractCreation cprg pr n v gv gl bs

acc1234 :: PrivateAccount
acc1234 = makePrivateAccount 1234

ccWithInitialGas :: Integer -> IO Transaction
ccWithInitialGas gl = cc acc1234 0 10 1 gl (B.pack [0..9])

ccWithNonce :: Integer -> IO Transaction
ccWithNonce n = cc acc1234 n 10 1 1000 (B.pack [0..9])

accWithNonce :: Integer -> Account
accWithNonce n = Account n 100 zeroRef NullCodeHash

accWithBalance :: Integer -> Account
accWithBalance b = Account 10 b zeroRef NullCodeHash

ccWithExpenses :: Integer -> Integer -> Integer -> IO Transaction
ccWithExpenses v gp gl = cc acc1234 10 v gp gl (B.pack [0..9])

mc :: PrivateAccount -> Integer -> Integer -> Integer -> Integer -> Address -> B.ByteString -> IO Transaction
mc pr n v gv gl to dat = do
        pool <- createEntropyPool
        let cprg = cprgCreate pool :: SystemRNG
        return $ initMessageCall cprg pr n v gv gl to dat

validGasCheck :: String -> IO Transaction -> Bool -> Test.Framework.Test
validGasCheck s t e = testCase s $
        do t' <- t
           e @=? isGasValid t'

nonceCheck :: String -> IO Transaction -> Account -> Bool -> Test.Framework.Test
nonceCheck s t a e = testCase s $
        do t' <- t
           e @=? isNonceValid t' a

upFrontCostCheck :: String -> IO Transaction -> Integer -> Test.Framework.Test
upFrontCostCheck s t e = testCase s $
        do t' <- t
           e @=? upFrontCost t'

balanceCheck :: String -> IO Transaction -> Account -> Bool -> Test.Framework.Test
balanceCheck s t a e = testCase s $
        do t' <- t
           e @=? isBalanceAvailable t' a

tests ::  [Test.Framework.Test]
tests = [ testGroup "verifications" verifyTests,
          testGroup "serializations" serializeTests
          ]

verifyTests ::  [Test.Framework.Test]
verifyTests = [ testGroup "InitialGasCheck" [
        validGasCheck ("0")     (ccWithInitialGas 0)    False,
        validGasCheck ("10")    (ccWithInitialGas 10)   False,
        validGasCheck ("1000")  (ccWithInitialGas 1000) True
        ],
        testGroup "NonceCheck" [
        nonceCheck ("ne")      (ccWithNonce  5) (accWithNonce  7)  False,
        nonceCheck ("smaller") (ccWithNonce 10) (accWithNonce  9)  False,
        nonceCheck ("eq")      (ccWithNonce 10) (accWithNonce 10)  True,
        nonceCheck ("greater") (ccWithNonce 10) (accWithNonce 11)  False
        ],
        testGroup "upFrontCost" [
        upFrontCostCheck "110" (ccWithExpenses 10 1 100) 110,
        upFrontCostCheck "199" (ccWithExpenses 99 1 100) 199,
        upFrontCostCheck "299" (ccWithExpenses 99 2 100) 299,
        upFrontCostCheck "301" (ccWithExpenses 99 2 101) 301
        ],
        testGroup "BalanceCheck" [
        balanceCheck ("smaller") (ccWithExpenses 10 1 100) (accWithBalance 109) False,
        balanceCheck ("eq")      (ccWithExpenses 10 1 100) (accWithBalance 110) True,
        balanceCheck ("greater") (ccWithExpenses 10 1 100) (accWithBalance 111) True
        ]
        ]

serializeTests ::  [Test.Framework.Test]
serializeTests =
        [  testGroup "ContractCreation" [
        roundTripContractCreation $ ContractCreation B.empty
        ], testGroup "MessageCall" [
        roundTripMessageCall $ MessageCall zeroAddress B.empty
        ], testGroup "Signatures" [
        roundTripSignature $ (nonSig $ acc1234)
        ], testGroup "TransactionCC" [
        roundTripTransaction "basic"     $ cc acc1234 10 10000 1 10 (B.empty),
        roundTripTransaction "withCode1" $ cc acc1234 10 10000 1 10 (B.replicate 10 0),
        roundTripTransaction "withCode2" $ cc acc1234 10 10000 1 10 (B.replicate 10 0xff),
        let v = 2^(255 :: Integer) in roundTripTransaction "bigInts"  $ cc acc1234 v v v v (B.empty)
        ], testGroup "TransactionMC" [
        roundTripTransaction "basic"     $ mc acc1234 10 10000 1 10 (A 10) (B.empty),
        roundTripTransaction "withCode1" $ mc acc1234 10 10000 1 10 (A 10) (B.replicate 10 0),
        roundTripTransaction "withCode2" $ mc acc1234 10 10000 1 10 (A 10) (B.replicate 10 0xff),
        let v = 2^(255 :: Integer) in roundTripTransaction "bigInts"  $ mc acc1234 v v v v (A 10) (B.empty)
        ]
        ]

