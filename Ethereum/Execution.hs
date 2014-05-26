module Ethereum.Execution where

import Control.Monad
import Ethereum.EVM.ExecutionEnvironment
import Ethereum.EVM.MachineState
import Ethereum.EVM.VM
import Ethereum.SimpleTypes
import Ethereum.Storage.Context
import Ethereum.State.Address
import Ethereum.State.Account as A
import Ethereum.State.Transaction as T

import Data.ByteString as B

-- Equation 38
startTransaction ::  Context -> Transaction -> Maybe Context
startTransaction c t =
        do let addr = (sender t)
           acc <- getAccount c addr
           unless (isTransactionValid t acc) Nothing

           -- Start irrevocable changes.
           return $ checkpointState c t (addr, acc)

-- Equations 39, 40, and 41.
checkpointState :: Context -> Transaction -> (Address, Account) -> Context
checkpointState c t (addr, acc) =
        let acc' = debit acc (upFrontCost  t)
            acc'' = nextNonce acc' in
        updateAccount c (addr, acc'')

runTransaction :: Context -> Address -> Transaction -> (Context, Integer)
runTransaction c addr t@(T n v gp gl (Right (ContractCreation ini)) _) = 
        runContractCreation c addr n (gl - intrinsicGas t) gp v ini
runTransaction _ _ _ = error "not implemented"

-- | Equation 49 has a more sensible definition of Lambda than Equation 42.
runContractCreation :: Context -> Address -> Integer -> Integer -> Integer -> Integer -> B.ByteString -> (Context, Integer)
runContractCreation c s n g gp v _ini_FIXME =
        let newAddr = generateValidAddress c s n
            newAcc = Account 0 v nullStateRoot NullCodeHash  -- | Equation 54.
            c' = updateAccount c (newAddr, newAcc)
            -- TODO initialize EE with code properly.
            ee = EE newAddr s gp emptyByteArray s v emptyByteArray -- | Equation 57-63.
        in case executeTransaction c' g ee of
                OutOfGas -> (c, 0)
                Result c'' ms' _ _ -> (c'', gas ms')

generateValidAddress :: Context -> Address -> Integer -> Address
generateValidAddress c a n = let a1 = generateAddress a n in validateAddress c a1

-- | Equation (52): Iterate to find unused addresses.
validateAddress :: Context -> Address -> Address
validateAddress c a@(A x) =
        case getAccount c a of
                Nothing -> a
                Just _ -> validateAddress c (A (x+1))

-- If these checks pass, we start making irrevocable state changes.
-- Step 1: Increment the sender's nonce, and remove the up front gas.
-- Step 2: Process the message call or contract creation.
--         Side effects according to those procedure definitions.
-- Step 3:
--   - Refund remaining gas to sender.
--   - Send spent gas to this block's "coinbase".
--   - Add initial gas + spent gas towards the block's limit.
