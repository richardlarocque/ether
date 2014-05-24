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
import Data.Binary.Put
import Data.LargeWord
import Ethereum.Crypto
import Ethereum.Common
import Ethereum.SimpleTypes
import Ethereum.Encoding.RLP
import qualified Ethereum.State.Account as A
import qualified Ethereum.FeeSchedule as F
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

data Transaction = CC TCommon ContractCreation TSignature
                 | MC TCommon MessageCall TSignature
                 deriving (Show, Eq)

data TCommon = TCommon
        Integer -- n
        Integer -- v
        Integer -- gp
        Integer -- gl
        deriving (Show, Eq)

data ContractCreation = ContractCreation B.ByteString
        deriving (Show, Eq)

data MessageCall = MessageCall Address B.ByteString
        deriving (Show, Eq)

nonce :: Transaction -> Integer
nonce (CC (TCommon n _ _ _) _ _) = n
nonce (MC (TCommon n _ _ _) _ _) = n

putCommon :: TCommon -> Put
putCommon (TCommon n v gp gl) =
        do putScalar n   -- T_n
           putScalar v   -- T_v
           putScalar gp  -- T_p
           putScalar gl  -- T_g

putContractCreation :: ContractCreation -> Put
putContractCreation (ContractCreation ini) =
        do putAddress $ zeroAddress  -- The blank 'to' address
           -- FIXME: Where and what is T_b?
           putArray ini  -- T_i

putMessageCall :: MessageCall -> Put
putMessageCall (MessageCall to dat) =
        do putAddress to  -- T_t
           putArray dat  -- T_d

getCommon :: Get TCommon
getCommon = do
        n <- getScalar
        v <- getScalar
        gp <- getScalar
        gl <- getScalar
        return $ TCommon n v gp gl

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
putTransaction (CC tc cc ts) = putSequence $
        do putCommon tc
           putContractCreation cc
           putSignature ts
putTransaction (MC tc mc ts) = putSequence $
        do putCommon tc
           putMessageCall mc
           putSignature ts

getTransaction :: Get Transaction
getTransaction =
        do len <- getSequenceHeader
           isolate len (getCC <|> getMC)
        where getCC = do tc <- getCommon
                         cc <- getContractCreation
                         ts <- getSignature
                         return $ CC tc cc ts
              getMC = do tc <- getCommon
                         mc <- getMessageCall
                         ts <- getSignature
                         return $ MC tc mc ts

-- Known as 'e' in Appendix F
transactionHashMC :: TCommon -> MessageCall -> Word256
transactionHashMC c mc = hashBytes $ L.toStrict $ runPut $ putSequence $
        do putCommon c
           putMessageCall mc

transactionHashCC :: TCommon -> ContractCreation -> Word256
transactionHashCC c cc = hashBytes $ L.toStrict $ runPut $ putSequence $
        do putCommon c
           putContractCreation cc

initContractCreation :: CPRG g => g -> Word256 -> Integer -> Integer -> Integer -> Integer -> B.ByteString -> Transaction
initContractCreation _cprg _pr nonc v gp gl ini =
       let tc = TCommon nonc v gp gl
           cc = ContractCreation ini
       in CC tc cc NonSig


initMessageCall :: CPRG g => g -> Word256 -> Integer -> Integer -> Integer -> Integer -> Address -> B.ByteString -> Transaction
initMessageCall _cprg _pr nonc v gp gl to dat = 
       let tc = TCommon nonc v gp gl
           mc = MessageCall to dat
       in MC tc mc NonSig

----

isSignatureValid :: Transaction -> Bool
isSignatureValid (MC _ _ NonSig) = True
isSignatureValid (CC _ _ NonSig) = True
isSignatureValid (MC c mc tsig) =
        let hashable = L.toStrict $ runPut $ do putCommon c
                                                putMessageCall mc
        in verifyTSig hashable tsig
isSignatureValid (CC c cc tsig) =
        let hashable = L.toStrict $ runPut $ do putCommon c
                                                putContractCreation cc
        in verifyTSig hashable tsig

isGasValid :: Transaction -> Bool
isGasValid t@(CC (TCommon _ _ _ gl) _ _) = intrinsicGas t < gl
isGasValid t@(MC (TCommon _ _ _ gl) _ _) = intrinsicGas t < gl

-- Equation (36)
intrinsicGas :: Transaction -> Integer
intrinsicGas (CC _ (ContractCreation ini) _)  =
        F.transaction + F.txdata * (fromIntegral $ B.length ini)
intrinsicGas (MC _ (MessageCall _ dat) _) =
        F.transaction + F.txdata * (fromIntegral $ B.length dat)

-- Equation (37)
upFrontCost :: Transaction -> Integer
upFrontCost (CC (TCommon _ v gp gl) _ _)  = gp * gl + v
upFrontCost (MC (TCommon _ v gp gl) _ _)  = gp * gl + v

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
