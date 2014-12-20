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

import qualified Data.ByteString        as B
import           Ethereum.Encoding.RLP
import           Ethereum.State.Address

data Transaction = T {
        t_n  :: Integer,
        t_v  :: Integer,
        t_gp :: Integer,
        t_gl :: Integer,
        t_x  :: Either MessageCall ContractCreation,
        t_w  :: Integer,
        t_r  :: Integer,
        t_s  :: Integer
    } deriving (Show, Eq)

data ContractCreation = ContractCreation B.ByteString
        deriving (Show, Eq)

data MessageCall = MessageCall Address B.ByteString
        deriving (Show, Eq)

instance RLPSerialize Transaction where
    toRLP t = Group $ headerRLPSnippet t
              ++ middleRLPSnippet t
              ++ signatureRLPSnippet t
    fromRLP (Group [n', v', gp', gl', a', d', w', r', s']) =
        do (n, v, gp, gl) <- headerToRLPFragment [n', v', gp', gl']
           x <- middleToRLPFragment [a', d']
           (w, r, s) <- signatureToRLPFragment [w', r', s']
           return $ T n v gp gl x w r s
    fromRLP _ = Nothing

headerToRLPFragment :: [RLP] -> Maybe (Integer, Integer, Integer, Integer)
headerToRLPFragment [n', v', gp', gl'] =
    do [n, v, gp, gl] <- mapM fromRLP [n', v', gp', gl']
       return (n, v, gp, gl)
headerToRLPFragment _ = Nothing

middleToRLPFragment :: [RLP] -> Maybe (Either MessageCall ContractCreation)
middleToRLPFragment [addr, Item dat] =
    do av <- fromRLP addr
       Just $ if av == zeroAddress
              then Right $ ContractCreation dat
              else Left $ MessageCall av dat
middleToRLPFragment _ = Nothing

signatureToRLPFragment :: [RLP] -> Maybe (Integer, Integer, Integer)
signatureToRLPFragment [w', r', s'] =
    do [w, r, s] <- mapM fromRLP [w', r', s']
       return (w, r, s)
signatureToRLPFragment _ = Nothing

headerRLPSnippet :: Transaction -> [RLP]
headerRLPSnippet (T n v gp gl _ _ _ _) = map toRLP [ n, v, gp, gl ]

middleRLPSnippet :: Transaction -> [RLP]
middleRLPSnippet (T _ _ _ _ (Left (MessageCall to dat)) _ _ _) =
    [ toRLP to, Item dat ]
middleRLPSnippet (T _ _ _ _ (Right (ContractCreation ini)) _ _ _) =
    [ nullRLP, Item ini ]

signatureRLPSnippet :: Transaction -> [RLP]
signatureRLPSnippet (T _ _ _ _ _ w r s) = map toRLP [ w, r, s ]

unsignedTransactionRLP :: Transaction -> RLP
unsignedTransactionRLP t = Group $ headerRLPSnippet t ++ middleRLPSnippet t
