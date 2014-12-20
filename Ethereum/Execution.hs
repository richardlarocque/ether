module Ethereum.Execution where

import           Control.Monad
import           Data.ByteString                   as B
import           Data.Maybe
import           Ethereum.Crypto.Hash
import           Ethereum.Crypto.Pubkey            (transactionSender)
import           Ethereum.EVM.ExecutionEnvironment
import           Ethereum.EVM.MachineState
import           Ethereum.EVM.VM
import           Ethereum.State.Account            as A
import           Ethereum.State.Address
import           Ethereum.State.Block
import           Ethereum.State.Transaction        as T
import           Ethereum.Storage.Context
import           Ethereum.TransactionVerification

doTransaction ::  BlockHeader -> Context -> Transaction -> Maybe Context
doTransaction bh c t =
           -- Equation 38
        do addr <- transactionSender t
           acc <- getAccount c addr
           unless (isTransactionValid t acc) Nothing

           let (T n v gp gl tt _ _ _) = t
           let g = gl - intrinsicGas t

           -- Equation 42
           let (c_p, g') = case tt of
                Right (ContractCreation ini) ->
                        let c_0 = ccCheckpointState c t (addr, acc)
                        in runContractCreation bh c_0 addr n g gp v ini -- Eq 49
                Left (MessageCall toAddr dat) ->
                        let c_1 = mcCheckpointState c addr toAddr v
                        in runMessageCall_ bh c_1  addr addr toAddr g gp v dat -- Eq 64

           -- Equation 45, refund some gas to the sender.
           let c_p' = creditAccount c_p addr (g'*gl)

           -- TODO: Pay the miner.
           -- TODO: Count gas spent against the block's limit.

           return c_p'

-- | Equations 39, 40, and 41.
ccCheckpointState :: Context -> Transaction -> (Address, Account) -> Context
ccCheckpointState c t (addr, acc) =
        let acc' = debit acc (upFrontCost  t)
            acc'' = nextNonce acc' in
        updateAccount c (addr, acc'')

-- | Equations 65 and 66 (assuming those equations *meant* to debit the sender)
mcCheckpointState :: Context -> Address -> Address -> Integer -> Context
mcCheckpointState c s r v = (\cx -> creditAccount cx r v) . (\cx -> debitAccount cx s v) $ c

-- | Equation 49 has a more sensible definition of Lambda than Equation 42.
runContractCreation :: BlockHeader -> Context -> Address -> Integer -> Integer -> Integer -> Integer -> B.ByteString -> (Context, Integer)
runContractCreation bh c s n g gp v ini =
        let newAddr = generateValidAddress c s n
            newAcc = Account 0 v nullStateRoot NullCodeHash  -- | Equation 54.
            c' = updateAccount c (newAddr, newAcc)
            ee = EE newAddr s gp B.empty s v ini bh -- | Equation 57-63.
        in case executeCode c' g ee of
                OutOfGas -> (c, 0)
                Result c'' ms' _ body -> (updateCodeBody c'' newAddr body, gas ms')

runMessageCall_ :: BlockHeader -> Context -> Address -> Address -> Address -> Integer -> Integer -> Integer -> B.ByteString -> (Context, Integer)
runMessageCall_ bh c s o r g gp v dat =
        case runMessageCall bh c s o r g gp v dat of
                OutOfGas -> (c, 0)
                Result c' ms' _ _ret -> (c', gas ms')

generateValidAddress :: Context -> Address -> Integer -> Address
generateValidAddress c a n = let a1 = generateAddress a n in incUntilValid c a1

-- | Equation (52): Iterate to find unused addresses.
incUntilValid :: Context -> Address -> Address
incUntilValid c a@(A x) =
        case getAccount c a of
                Nothing -> a
                Just _ -> incUntilValid c (A (x+1))

updateCodeBody :: Context -> Address -> Maybe B.ByteString -> Context
updateCodeBody c addr body =
    fromMaybe c $ do body' <- body
                     let h = hashAsWord body'
                     let c' = insertToStorage c (h, body')
                     modifyAccount c' addr (\a -> a{codeHash=CodeHash h})

creditAccount :: Context -> Address -> Integer -> Context
creditAccount c addr e = fromMaybe c $ modifyAccount c addr (\a-> credit a e)

debitAccount :: Context -> Address -> Integer -> Context
debitAccount c addr e = fromMaybe c $ modifyAccount c addr (\a-> debit a e)
