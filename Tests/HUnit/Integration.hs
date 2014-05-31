module Tests.HUnit.Integration(tests) where

import Crypto.Random
import Data.Maybe
import Ethereum.Crypto
import Ethereum.State.Account as A
import Ethereum.State.Transaction as T
import Ethereum.Storage.Context
import Ethereum.Execution
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.ByteString as B

buildAndVerify :: IO ()
buildAndVerify = do
        pool <- createEntropyPool
        let cprg = cprgCreate pool :: SystemRNG

        let c0 = initContext

        let pr = makePrivateAccount 1234
        let addr = addressFromPriv pr
        let acc = Account 0 90000 nullStateRoot NullCodeHash
        let c1 = updateAccount c0 (addr, acc) 

        let cc1 = initContractCreation cprg pr (A.nonce acc) 10 1 1000 B.empty

        assertBool "verify transaction" (isJust $ doTransaction c1 cc1)

tests ::  [Test.Framework.Test]
tests = [ testCase "BuildAndVerify" buildAndVerify ]
