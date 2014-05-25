module Ethereum.Execution where

import Control.Monad
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

runTransaction :: Context -> Address -> Transaction -> Integer -> (Context, Integer)
runTransaction c addr (T n v gp _gl (Right (ContractCreation ini)) _) g = 
        runContractCreation c addr n g gp v ini
runTransaction _ _ _ _ = error "not implemented"

-- | Equation 49 has a more sensible definition of Lambda than Equation 42.
runContractCreation :: Context -> Address -> Integer -> Integer -> Integer -> Integer -> B.ByteString -> (Context, Integer)
runContractCreation c s n g gp v ini =
        let newAddr = generateValidAddress c s n
            newAcc = Account 0 v nullStateRoot NullCodeHash  -- | Equation 54.
            c' = updateAccount c (newAddr, newAcc)
            ee = ExecutionEnvironment newAddr s gp B.empty s v ini -- | Equation 57-63.
            case execContractCreation ee of
                    Left OutOfGas -> (c, 0)
                    Left (InvalidInstruction (c'', g')) -> (c'', g')
                    Left (StackUnderflow (c'', g')) -> (c'', g')
                    Right ValidHalt (c'', g') _bs -> (c'', g')
        in (c', 0)

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
