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

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString        as B
import           Data.Serialize
import           Ethereum.Encoding.RLP
import           Ethereum.State.Address

data Transaction = T
        Integer -- n
        Integer -- v
        Integer -- gp
        Integer -- gl
        (Either MessageCall ContractCreation)
        Integer -- w
        Integer -- r
        Integer -- s
        deriving (Show, Eq)

data ContractCreation = ContractCreation B.ByteString
        deriving (Show, Eq)

data MessageCall = MessageCall Address B.ByteString
        deriving (Show, Eq)

nonce :: Transaction -> Integer
nonce (T n _ _ _ _ _ _ _) = n

putContractCreation :: ContractCreation -> Put
putContractCreation (ContractCreation ini) =
        do putAddress zeroAddress  -- The blank 'to' address
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
        liftM ContractCreation getArray

getMessageCall :: Get MessageCall
getMessageCall = do
        to <- getAddress
        dat <- getArray
        return $ MessageCall to dat

putTransaction :: Transaction -> Put
putTransaction t@(T _ _ _ _ _ w r s) = putSequence $
        do putUnsignedTransaction' t
           putScalar w
           putScalar r
           putScalar s

putUnsignedTransaction :: Transaction -> Put
putUnsignedTransaction = putSequence . putUnsignedTransaction'

putUnsignedTransaction' :: Transaction -> Put
putUnsignedTransaction' (T n v gp gl x _ _ _) =
        do putScalar n   -- T_n
           putScalar v   -- T_v
           putScalar gp  -- T_p
           putScalar gl  -- T_g
           case x of
                   Left mc -> putMessageCall mc
                   Right cc -> putContractCreation cc

getTransaction :: Get Transaction
getTransaction =
        do len <- getSequenceHeader
           isolate len $ (getCC <|> getMC)
        where getCC = do n <- getScalar
                         v <- getScalar
                         gp <- getScalar
                         gl <- getScalar
                         cc <- getContractCreation
                         w <- getScalar
                         r <- getScalar
                         s <- getScalar
                         return $ T n v gp gl (Right cc) w r s
              getMC = do n <- getScalar
                         v <- getScalar
                         gp <- getScalar
                         gl <- getScalar
                         mc <- getMessageCall
                         w <- getScalar
                         r <- getScalar
                         s <- getScalar
                         return $ T n v gp gl (Left mc) w r s

{-
-- Known as 'e' in Appendix F
transactionHashMC :: TCommon -> MessageCall -> Word256
transactionHashMC c mc = hashPut $ putSequence $
        do putCommon c
           putMessageCall mc

transactionHashCC :: TCommon -> ContractCreation -> Word256
transactionHashCC c cc = hashPut $ putSequence $
        do putCommon c
           putContractCreation cc
-}

----
