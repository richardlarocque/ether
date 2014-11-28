module Tests.HUnit.Integration(tests) where

import           Control.Monad
import qualified Data.ByteString            as B
import           Data.Maybe
import           Data.Monoid
import           Ethereum.Crypto
import           Ethereum.Execution
import           Ethereum.Lang.Ops          as L
import           Ethereum.SimpleTypes
import           Ethereum.State.Account     as A
import           Ethereum.State.Address
import           Ethereum.State.Block
import           Ethereum.State.Transaction as T
import           Ethereum.Storage.Context
import           Test.Tasty
import           Test.Tasty.HUnit

priv1 :: PrivateAccount
priv1 = makePrivateAccount 1234

acc1 :: Account
acc1 = Account 0 90000 nullStateRoot NullCodeHash

initTestContext :: Context
initTestContext =
        let c0 = initContext
        in updateAccount c0 (addressFromPriv priv1, acc1)

makeCCWithCode :: Context -> PrivateAccount -> B.ByteString -> Transaction
makeCCWithCode c pr cs =
        let addr = addressFromPriv pr
            acc = fromJust $ getAccount c addr
        in initContractCreation pr (A.nonce acc) 10 2 10000 cs

makeMCWithData :: Context -> PrivateAccount -> Address -> B.ByteString -> Transaction
makeMCWithData c pr toAddr dat =
        let senderAddr = addressFromPriv pr
            senderAcc = fromJust $ getAccount c senderAddr
        in initMessageCall pr (A.nonce senderAcc) 10 2 10000 toAddr dat

getGeneratedAddress :: Context -> Transaction -> Address
getGeneratedAddress c t =
        case t of
                (T n _ _ _ (Right _) _) -> generateValidAddress c (sender t) n
                _ -> undefined

buildCC :: Context -> B.ByteString -> IO (Address, Context)
buildCC c bs =
        do let cc1 = makeCCWithCode c priv1 bs
           let bh = genesisBlockHeader

           -- TODO: Find a better way to fetch the address.
           let newAddr = getGeneratedAddress c cc1

           let result = doTransaction bh c cc1
           assertBool "verify transaction" (isJust $ result)

           let Just c' = result
           assertBool "verify new contract" (isJust $ getAccount c' newAddr)

           return (newAddr, c')

contractCreationTest :: TestTree
contractCreationTest = testCase "contractCreationTest" $
        do let c = initTestContext
           void $ buildCC c B.empty

returnProgram :: B.ByteString -> B.ByteString
returnProgram prog =
        compile $ memLiteral 0 prog
               <> returnOp (p1 0) (p32 (fromIntegral $ B.length prog))

runCreatedContract :: TestTree
runCreatedContract = testCase "runCreatedContract" $
        do let c = initTestContext
           let bh = genesisBlockHeader

           (contractAddr, c') <- buildCC c (returnProgram incrementCounter)
           assertEqual "ininital" 0 (accountLoad c' contractAddr 0)

           let mc = makeMCWithData c' priv1 contractAddr (toBytes 10)
           let Just c'' = doTransaction bh c' mc

           assertEqual "incremented" 1 (accountLoad c'' contractAddr 0)

        where incrementCounter = compile $
                sstore (p1 0) (add (p1 1) (sload (p1 0)))


tests ::  TestTree
tests = testGroup "Integration"
        [ contractCreationTest,
          runCreatedContract ]
