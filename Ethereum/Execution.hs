module Ethereum.Execution where

import Control.Monad
import Data.Maybe
import Ethereum.Common
import Ethereum.EVM.ExecutionEnvironment
import Ethereum.EVM.MachineState
import Ethereum.EVM.VM
import Ethereum.Storage.Context
import Ethereum.State.Address
import Ethereum.State.Account as A
import Ethereum.State.Transaction as T

import Data.ByteString as B

startTransaction ::  Context -> Transaction -> Maybe Context
startTransaction c t =
           -- Equation 38
        do let addr = (sender t)
           acc <- getAccount c addr
           unless (isTransactionValid t acc) Nothing

           -- Start irrevocable changes.
           let c_0 = checkpointState c t (addr, acc)

           let (T n v gp gl tt _) = t

           -- Equation 42, but translated to be more like equation 42.
           let (c_p, g') = case tt of
                (Right (ContractCreation ini)) ->
                        let g = (gl - intrinsicGas t)
                        in runContractCreation c_0 addr n g gp v ini
                _ -> undefined

           -- Equation 45, refund some gas to the sender.
           let c_p' = creditAccount c_p addr (g'*gl)

           -- TODO: Pay the miner.

           return c_p'

-- Equations 39, 40, and 41.
checkpointState :: Context -> Transaction -> (Address, Account) -> Context
checkpointState c t (addr, acc) =
        let acc' = debit acc (upFrontCost  t)
            acc'' = nextNonce acc' in
        updateAccount c (addr, acc'')

-- | Equation 49 has a more sensible definition of Lambda than Equation 42.
runContractCreation :: Context -> Address -> Integer -> Integer -> Integer -> Integer -> B.ByteString -> (Context, Integer)
runContractCreation c s n g gp v ini =
        let newAddr = generateValidAddress c s n
            newAcc = Account 0 v nullStateRoot NullCodeHash  -- | Equation 54.
            c' = updateAccount c (newAddr, newAcc)
            -- TODO initialize EE with code properly.
            ee = EE newAddr s gp B.empty s v ini -- | Equation 57-63.
        in case executeCode c' g ee of
                OutOfGas -> (c, 0)
                Result c'' ms' _ body -> (updateCodeBody c'' newAddr body, gas ms')

generateValidAddress :: Context -> Address -> Integer -> Address
generateValidAddress c a n = let a1 = generateAddress a n in validateAddress c a1

-- | Equation (52): Iterate to find unused addresses.
validateAddress :: Context -> Address -> Address
validateAddress c a@(A x) =
        case getAccount c a of
                Nothing -> a
                Just _ -> validateAddress c (A (x+1))

updateCodeBody :: Context -> Address -> Maybe B.ByteString -> Context
updateCodeBody c addr body =
    fromMaybe c $ do body' <- body
                     let h = hashBytes body'
                     let c' = insertToStorage c (h, body')
                     modifyAccount c' addr (\a -> a{codeHash=CodeHash h})

creditAccount :: Context -> Address -> Integer -> Context
creditAccount c addr e = fromMaybe c $ modifyAccount c addr (\a-> credit a e)

-- If these checks pass, we start making irrevocable state changes.
-- Step 1: Increment the sender's nonce, and remove the up front gas.
-- Step 2: Process the message call or contract creation.
--         Side effects according to those procedure definitions.
-- Step 3:
--   - Refund remaining gas to sender.
--   - Send spent gas to this block's "coinbase".
--   - Add initial gas + spent gas towards the block's limit.
