module Ethereum.TransactionVerification where

import qualified Data.ByteString            as B
import           Ethereum.Crypto
import qualified Ethereum.FeeSchedule       as F
import qualified Ethereum.State.Account     as A
import           Ethereum.State.Transaction

import           Debug.Trace

isGasValid :: Transaction -> Bool
isGasValid t@(T _ _ _ gl _ _ _ _) = traceShow "gasVald" $ intrinsicGas t < gl

-- Equation (36)
intrinsicGas :: Transaction -> Integer
intrinsicGas (T _ _ _ _ (Left (MessageCall _ dat)) _ _ _) =
        F.transaction + F.txdata * fromIntegral (B.length dat)
intrinsicGas (T _ _ _ _ (Right (ContractCreation ini)) _ _ _) =
        F.transaction + F.txdata * fromIntegral (B.length ini)

-- Equation (37)
upFrontCost :: Transaction -> Integer
upFrontCost (T _ v gp gl _ _ _ _) = gp * gl + v

isBalanceAvailable :: Transaction -> A.Account -> Bool
isBalanceAvailable t a = traceShow "balance" $ upFrontCost t <= A.balance a

isNonceValid :: Transaction -> A.Account -> Bool
isNonceValid t a = traceShow "nonceValid" $ nonce t == A.nonce a

-- Section 6
isTransactionValid :: Transaction -> A.Account -> Bool
isTransactionValid t a =
        -- TODO: Check sender ID = account address
        isSignatureValid t &&
        isNonceValid t a &&
        isGasValid t &&
        isBalanceAvailable t a
