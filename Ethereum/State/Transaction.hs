{- |
Module      :  Ethereum.State.Transaction
Description :  Implementation of an Ethereum transaction
Copyright   :  (c) Richard Larocque
License     :  GPL-3.0+

Maintainer  :  richard.larocque@gmail.com
Stability   :  unstable
Portability :  non-portable (Unknown portability)

See Ethereum Yellow Paper, Proof-of-Concept V, Section 4.1
-}

module Ethereum.State.Transaction where

import Control.Applicative
import Control.Monad
import Crypto.Random
import Data.Binary
import Data.Binary.Get
import Data.LargeWord
import Ethereum.Crypto
import Ethereum.SimpleTypes
import Ethereum.Encoding.RLP
import qualified Ethereum.State.Account as A
import qualified Ethereum.FeeSchedule as F
import qualified Data.ByteString as B

data Transaction = T
        Integer -- n
        Integer -- v
        Integer -- gp
        Integer -- gl
        (Either MessageCall ContractCreation)
        TSignature
        deriving (Show, Eq)

data ContractCreation = ContractCreation B.ByteString
        deriving (Show, Eq)

data MessageCall = MessageCall Address B.ByteString
        deriving (Show, Eq)

nonce :: Transaction -> Integer
nonce (T n _ _ _ _ _) = n

putContractCreation :: ContractCreation -> Put
putContractCreation (ContractCreation ini) =
        do putAddress $ zeroAddress  -- The blank 'to' address
           -- FIXME: Where and what is T_b?
           putArray ini  -- T_i

putMessageCall :: MessageCall -> Put
putMessageCall (MessageCall to dat) =
        do putAddress to  -- T_t
           putArray dat  -- T_d

getContractCreation :: Get ContractCreation
getContractCreation = do
        to <- getAddress
        unless (to == zeroAddress) (fail "CC with non-zero to address")
        getArray >>= return.ContractCreation

getMessageCall :: Get MessageCall
getMessageCall = do
        to <- getAddress
        dat <- getArray
        return $ MessageCall to dat

putTransaction :: Transaction -> Put
putTransaction (T n v gp gl x ts) = putSequence $
        do putScalar n   -- T_n
           putScalar v   -- T_v
           putScalar gp  -- T_p
           putScalar gl  -- T_g
           case x of
                   Left mc -> putMessageCall mc
                   Right cc -> putContractCreation cc
           putSignature ts

getTransaction :: Get Transaction
getTransaction =
        do len <- getSequenceHeader
           isolate len $ (getCC <|> getMC)
        where getCC = do n <- getScalar
                         v <- getScalar
                         gp <- getScalar
                         gl <- getScalar
                         cc <- getContractCreation
                         ts <- getSignature
                         return $ T n v gp gl (Right cc) ts
              getMC = do n <- getScalar
                         v <- getScalar
                         gp <- getScalar
                         gl <- getScalar
                         mc <- getMessageCall
                         ts <- getSignature
                         return $ T n v gp gl (Left mc) ts

{-
-- Known as 'e' in Appendix F
transactionHashMC :: TCommon -> MessageCall -> Word256
transactionHashMC c mc = hashBytes $ L.toStrict $ runPut $ putSequence $
        do putCommon c
           putMessageCall mc

transactionHashCC :: TCommon -> ContractCreation -> Word256
transactionHashCC c cc = hashBytes $ L.toStrict $ runPut $ putSequence $
        do putCommon c
           putContractCreation cc
-}

initContractCreation :: CPRG g => g -> Word256 -> Integer -> Integer -> Integer -> Integer -> B.ByteString -> Transaction
initContractCreation _cprg _pr nonc v gp gl ini = T nonc v gp gl (Right (ContractCreation ini)) NonSig


initMessageCall :: CPRG g => g -> Word256 -> Integer -> Integer -> Integer -> Integer -> Address -> B.ByteString -> Transaction
initMessageCall _cprg _pr nonc v gp gl to dat = T nonc v gp gl (Left (MessageCall to dat)) NonSig

----

isSignatureValid :: Transaction -> Bool
isSignatureValid (T _ _ _ _ _ NonSig) = True

isGasValid :: Transaction -> Bool
isGasValid t@(T _ _ _ gl _ _) = intrinsicGas t < gl

-- Equation (36)
intrinsicGas :: Transaction -> Integer
intrinsicGas (T _ _ _ _ (Left (MessageCall _ dat)) _) = 
        F.transaction + F.txdata * (fromIntegral $ B.length dat)
intrinsicGas (T _ _ _ _ (Right (ContractCreation ini)) _) = 
        F.transaction + F.txdata * (fromIntegral $ B.length ini)

-- Equation (37)
upFrontCost :: Transaction -> Integer
upFrontCost (T _ v gp gl _ _) = gp * gl + v

isBalanceAvailable :: Transaction -> A.Account -> Bool
isBalanceAvailable t a = (upFrontCost t) <= (A.balance a)

isNonceValid :: Transaction -> A.Account -> Bool
isNonceValid t a = (nonce t) == (A.nonce a)

-- sender :: MapStorage -> Transaction -> Account

-- Section 6
isTransactionValid :: Transaction -> A.Account -> Bool
isTransactionValid t a = and [
        -- TODO: Check sender ID = account address
        isSignatureValid t,
        isNonceValid t a,
        isGasValid t,
        isBalanceAvailable t a ]
