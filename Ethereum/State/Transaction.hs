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
putContractCreation (ContractCreation init) =
        do putAddress $ zeroAddress  -- The blank 'to' address
           -- FIXME: Where and what is T_b?
           putArray init  -- T_i

putMessageCall :: MessageCall -> Put
putMessageCall (MessageCall to dat) =
        do putAddress to  -- T_t
           putArray dat  -- T_d

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
initContractCreation cprg pr nonce v gp gl init =
       let pk = makePrivateKey pr
           tc = TCommon nonce v gp gl
           cc = ContractCreation init
           hashable = L.toStrict $ runPut $ do putCommon tc
                                               putContractCreation cc
           sig = signTransaction cprg pk hashable
       in CC tc cc sig

----

isSignatureValid :: Transaction -> Bool
isSignatureValid (CC c cc tsig) =
        let hashable = L.toStrict $ runPut $ do putCommon c
                                                putContractCreation cc
        in verifyTSig hashable tsig

isGasValid :: Transaction -> Bool
isGasValid _ = True  -- FIXME check intrinsicGas against gasLimit

-- Equation (36)
intrinsicGas :: Transaction -> Integer
intrinsicGas _ = 0  -- FIXME different for each transaction type...

-- Equation (37)
upFrontConst :: Transaction -> Integer
upFrontConst _ = 0  -- FIXME: insitrinsicGas * gasPrice + T_v (transaction value)

-- sender :: MapStorage -> Transaction -> Account
