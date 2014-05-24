module Tests.HUnit.Transaction(tests) where

import Control.Monad
import Crypto.Random
import Data.LargeWord
import Ethereum.State.Transaction
import Ethereum.FeeSchedule as F
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

roundTripTest :: (Show a, Eq a) => (a -> L.ByteString) -> (L.ByteString -> a) -> a -> Test.Framework.Test
roundTripTest e d x = testCase (show x) $ x @=? (d.e) x

-- serialization_tests :: [Test.Framework.Test]
-- serialization_tests = [
--         testGroup "Serialization" [

defaultContractCreation :: IO (Word256 -> Integer -> Integer -> Integer -> Integer -> B.ByteString -> Transaction)
defaultContractCreation = do
        pool <- createEntropyPool
        let cprg = cprgCreate pool :: SystemRNG
        return $ initContractCreation cprg

ccWithInitialGas :: Integer -> IO Transaction
ccWithInitialGas gl = do dcc <- defaultContractCreation
                         return $ dcc 1234 0 10 1 gl (B.pack [0..9])

validGas :: String -> IO Transaction -> Bool -> Test.Framework.Test
validGas s t e = testCase s $
        do t' <- t
           isGasValid t' @=? e

tests ::  [Test.Framework.Test]
tests = verifyTests

verifyTests ::  [Test.Framework.Test]
verifyTests = [ testGroup "initialGasCheck" [
        validGas ("0")     (ccWithInitialGas 0)    False,
        validGas ("10")    (ccWithInitialGas 10)   False,
        validGas ("1000") (ccWithInitialGas 1000) True
        ],
        testGroup "Null" [
        ]
        ]
