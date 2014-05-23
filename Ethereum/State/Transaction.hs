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

import Crypto.Random
import Data.Binary
import Data.Binary.Put
import Data.LargeWord
import Ethereum.Crypto
import Ethereum.Common
import Ethereum.SimpleTypes
import Ethereum.Encoding.RLP
import qualified Ethereum.FeeSchedule as F
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

data Transaction = CC TCommon ContractCreation TSignature
                 | MC TCommon MessageCall TSignature

data TCommon = TCommon {
        nonce :: Integer,
        value :: Integer,
        gasPrice :: Integer,
        gasLimit :: Integer
}

data ContractCreation = ContractCreation B.ByteString

data MessageCall = MessageCall Address B.ByteString

putCommon :: TCommon -> Put
putCommon TCommon { nonce=n, value=v, gasPrice=gp, gasLimit=gl } =
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
getContractCreation = getArray >>= return.ContractCreation

getMessageCall :: Get MessageCall
getMessageCall = do
        to <- getAddress
        dat <- getArray
        return $ MessageCall to dat

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
initContractCreation cprg pr nonc v gp gl ini =
       let pk = makePrivateKey pr
           tc = TCommon nonc v gp gl
           cc = ContractCreation ini
           hashable = L.toStrict $ runPut $ do putCommon tc
                                               putContractCreation cc
           sig = signTransaction cprg pk hashable
       in CC tc cc NonSig

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
isGasValid t@(CC (TCommon _ _ _ gl) _ _) =
        intrinsicGas t > gl
isGasValid t@(MC (TCommon _ _ _ gl) _ _) =
        intrinsicGas t > gl

-- Equation (36)
intrinsicGas :: Transaction -> Integer
intrinsicGas (CC _ (ContractCreation ini) _)  =
        F.transaction + F.txdata * (fromIntegral $ B.length ini)
intrinsicGas (MC _ (MessageCall _ dat) _) =
        F.transaction + F.txdata * (fromIntegral $ B.length dat)

-- Equation (37)
upFrontConst :: Transaction -> Integer
upFrontConst t@(CC (TCommon _ v gp gl) _ _)  =
        gp * gl + v
upFrontConst t@(MC (TCommon _ v gp gl) _ _)  =
        gp * gl + v

-- sender :: MapStorage -> Transaction -> Account
