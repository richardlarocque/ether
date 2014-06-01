module Tests.HUnit.Integration(tests) where

import Data.Monoid
import Control.Monad
import Crypto.Random
import Data.Maybe
import Ethereum.Crypto
import Ethereum.Execution
import Ethereum.State.Account as A
import Ethereum.State.Address
import Ethereum.State.Transaction as T
import Ethereum.Storage.Context
import Ethereum.Lang.Ops as L
import Ethereum.SimpleTypes
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

getGeneratedAddress :: Context -> Transaction -> Address
getGeneratedAddress c t =
        case t of
                (T n _ _ _ (Right _) _) -> generateValidAddress c (sender t) n
                _ -> undefined

buildCC :: CPRG g => g -> Context -> B.ByteString -> IO (Address, Context)
buildCC cprg c bs =
        do let cc1 = makeCCWithCode cprg c priv1 bs

           -- TODO: Find a better way to fetch the address.
           let newAddr = getGeneratedAddress c cc1

           let result = doTransaction c cc1
           assertBool "verify transaction" (isJust $ result)

           let Just c' = result
           assertBool "verify new contract" (isJust $ getAccount c' newAddr)

           return (newAddr, c')

contactCreationTest :: IO ()
contactCreationTest =
        do cprg <- initCPRG
           let c = initTestContext
           void $ buildCC cprg c B.empty

returnProgram :: B.ByteString -> B.ByteString
returnProgram prog =
        compile $ memLiteral 0 prog
               <> returnOp (p1 0) (p32 (fromIntegral $ B.length prog))

runCreatedContract :: IO ()
runCreatedContract =
        do cprg <- initCPRG
           let c = initTestContext

           (contractAddr, c') <- buildCC cprg c (returnProgram incrementCounter)
           assertEqual "ininital" 0 (accountLoad c' contractAddr 0)

           let mc = makeMCWithData cprg c' priv1 contractAddr (toBytes 10)
           let Just c'' = doTransaction c' mc

           assertEqual "incremented" 1 (accountLoad c'' contractAddr 0)

        where incrementCounter = compile $
                sstore (p1 0) (add (p1 1) (sload (p1 0)))


tests ::  [Test.Framework.Test]
tests = [ testCase "createContract" (contactCreationTest),
          testCase "runCreatedContract" runCreatedContract ]
