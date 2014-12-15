module Ethereum.Builders where

import qualified Data.ByteString            as B
import           Ethereum.Crypto
import           Ethereum.State.Address
import           Ethereum.State.Transaction

initContractCreation :: PrivateKey -> Integer -> Integer -> Integer -> Integer
                     -> B.ByteString
                     -> Transaction
initContractCreation pr nonc v gp gl ini =
    initTransaction pr nonc v gp gl (Right (ContractCreation ini))

initMessageCall :: PrivateKey -> Integer -> Integer -> Integer -> Integer
                -> Address -> B.ByteString
                -> Transaction
initMessageCall pr nonc v gp gl to dat =
    initTransaction pr nonc v gp gl (Left (MessageCall to dat))

initTransaction :: PrivateKey -> Integer -> Integer -> Integer -> Integer
                -> Either MessageCall ContractCreation
                -> Transaction
initTransaction pr nonc v gp gl specs =
    let protoT = T nonc v gp gl specs
        unsignedT = protoT 0 0 0
        Just (w, r, s) = signTransaction pr unsignedT
        signedT = protoT w r s
    in signedT
