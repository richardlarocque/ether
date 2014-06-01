module Tests.HUnit.Integration(tests) where

import Control.Monad
import Crypto.Random
import Data.Maybe
import Ethereum.Crypto
import Ethereum.Execution
import Ethereum.State.Account as A
import Ethereum.State.Address
import Ethereum.State.Transaction as T
import Ethereum.Storage.Context
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.ByteString as B

priv1 :: PrivateAccount
priv1 = makePrivateAccount 1234

acc1 :: Account
acc1 = Account 0 90000 nullStateRoot NullCodeHash

initCPRG :: IO SystemRNG
initCPRG =
        do pool <- createEntropyPool
           return $ cprgCreate pool

initTestContext :: Context
initTestContext =
        let c0 = initContext
        in updateAccount c0 (addressFromPriv priv1, acc1) 

makeCCWithCode :: CPRG g => g -> Context -> PrivateAccount -> B.ByteString -> Transaction
makeCCWithCode cprg c pr cs =
        let addr = addressFromPriv pr
            acc = fromJust $ getAccount c addr
        in initContractCreation cprg pr (A.nonce acc) 10 2 10000 cs

makeMCWithData :: CPRG g => g -> Context -> PrivateAccount -> Address -> B.ByteString -> Transaction
makeMCWithData cprg c pr toAddr dat =
        let senderAddr = addressFromPriv pr
            senderAcc = fromJust $ getAccount c senderAddr
        in initMessageCall cprg pr (A.nonce senderAcc) 10 2 10000 toAddr dat

buildAndVerify :: IO Context
buildAndVerify = do
        cprg <- initCPRG
        let c = initTestContext
        let cc1 = makeCCWithCode cprg c priv1 B.empty

        let result = doTransaction c cc1
        assertBool "verify transaction" (isJust $ result)

        let Just c' = result
        assertBool "X" (isNothing $ getAccount c' (A 0))

        return c'

tests ::  [Test.Framework.Test]
tests = [ testCase "BuildAndVerify" (void buildAndVerify) ]
