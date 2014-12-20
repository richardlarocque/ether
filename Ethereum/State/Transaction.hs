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

data Transaction = T {
        t_n  :: Integer,
        t_v  :: Integer,
        t_gp :: Integer,
        t_gl :: Integer,
        t_x  :: (Either MessageCall ContractCreation),
        t_w  :: Integer,
        t_r  :: Integer,
        t_s  :: Integer
    } deriving (Show, Eq)

data ContractCreation = ContractCreation B.ByteString
        deriving (Show, Eq)

data MessageCall = MessageCall Address B.ByteString
        deriving (Show, Eq)

instance RLPSerialize Transaction where
    asRLP t = Group $ headerRLPSnippet t
              ++ middleRLPSnippet t
              ++ signatureRLPSnippet t
    fromRLP (Group [n', v', gp', gl', a', d', w', r', s']) =
        do (n, v, gp, gl) <- withHeaderRLP [n', v', gp', gl']
           x <- withMiddleRLP $ [a', d']
           (w, r, s) <- withSignatureRLP [w', r', s']
           return $ T n v gp gl x w r s

withHeaderRLP :: [RLP] -> Maybe (Integer, Integer, Integer, Integer)
withHeaderRLP [n', v', gp', gl'] =
    do [n, v, gp, gl] <- mapM fromRLP [n', v', gp', gl']
       return (n, v, gp, gl)
withHeaderRLP _ = Nothing

withMiddleRLP :: [RLP] -> Maybe (Either MessageCall ContractCreation)
withMiddleRLP [addr, Item dat] =
    do av <- fromRLP addr
       Just $ if av == zeroAddress
              then Right $ ContractCreation dat
              else Left $ MessageCall av dat
withMiddleRLP _ = Nothing

withSignatureRLP :: [RLP] -> Maybe (Integer, Integer, Integer)
withSignatureRLP [w', r', s'] =
    do [w, r, s] <- mapM fromRLP [w', r', s']
       return (w, r, s)

headerRLPSnippet :: Transaction -> [RLP]
headerRLPSnippet (T n v gp gl _ _ _ _) = map scalarToRLP [ n, v, gp, gl ]

middleRLPSnippet :: Transaction -> [RLP]
middleRLPSnippet (T _ _ _ _ (Left (MessageCall to dat)) _ _ _) =
    [ asRLP to, Item dat ]
middleRLPSnippet (T _ _ _ _ (Right (ContractCreation ini)) _ _ _) =
    [ asRLP zeroAddress, Item ini ]

signatureRLPSnippet :: Transaction -> [RLP]
signatureRLPSnippet (T _ _ _ _ _ w r s) = map scalarToRLP [ w, r, s ]

unsignedTransactionRLP :: Transaction -> RLP
unsignedTransactionRLP t = Group $ headerRLPSnippet t ++ middleRLPSnippet t

nonce :: Transaction -> Integer
nonce (T n _ _ _ _ _ _ _) = n

-- putTransaction :: Transaction -> Put
-- putTransaction t@(T _ _ _ _ _ w r s) = putSequence $
--         do putUnsignedTransaction' t
--            putScalar w
--            putScalar r
--            putScalar s

-- putContractCreation :: ContractCreation -> Put
-- putContractCreation (ContractCreation ini) =
--         do putAddress zeroAddress  -- The blank 'to' address
--            -- FIXME: Where and what is T_b?
--            putArray ini  -- T_i
--
-- putMessageCall :: MessageCall -> Put
-- putMessageCall (MessageCall to dat) =
--         do putAddress to  -- T_t
--            putArray dat  -- T_d
--
-- getContractCreation :: Get ContractCreation
-- getContractCreation = do
--         to <- getAddress
--         unless (to == zeroAddress) (fail "CC with non-zero to address")
--         liftM ContractCreation getArray
--
-- getMessageCall :: Get MessageCall
-- getMessageCall = do
--         to <- getAddress
--         dat <- getArray
--         return $ MessageCall to dat
--
-- putUnsignedTransaction :: Transaction -> Put
-- putUnsignedTransaction = putSequence . putUnsignedTransaction'
--
-- putUnsignedTransaction' :: Transaction -> Put
-- putUnsignedTransaction' (T n v gp gl x _ _ _) =
--         do putScalar n   -- T_n
--            putScalar v   -- T_v
--            putScalar gp  -- T_p
--            putScalar gl  -- T_g
--            case x of
--                    Left mc -> putMessageCall mc
--                    Right cc -> putContractCreation cc
--
-- getTransaction :: Get Transaction
-- getTransaction =
--         do len <- getSequenceHeader
--            isolate len $ (getCC <|> getMC)
--         where getCC = do n <- getScalar
--                          v <- getScalar
--                          gp <- getScalar
--                          gl <- getScalar
--                          cc <- getContractCreation
--                          w <- getScalar
--                          r <- getScalar
--                          s <- getScalar
--                          return $ T n v gp gl (Right cc) w r s
--               getMC = do n <- getScalar
--                          v <- getScalar
--                          gp <- getScalar
--                          gl <- getScalar
--                          mc <- getMessageCall
--                          w <- getScalar
--                          r <- getScalar
--                          s <- getScalar
--                          return $ T n v gp gl (Left mc) w r s
