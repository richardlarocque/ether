{- |
Module      :  Ethereum.State.Account
Description :  Implementation of an Ethereum account's state
Copyright   :  (c) Richard Larocque
License     :  GPL-3.0+

Maintainer  :  richard.larocque@gmail.com
Stability   :  unstable
Portability :  non-portable (Unknown portability)

See Ethereum Yellow Paper, Proof-of-Concept V, Section 4.1
-}

module Ethereum.State.Account where

import qualified Data.ByteString       as B
import           Data.LargeWord
import           Data.Serialize
import           Ethereum.Common
import           Ethereum.Encoding.RLP
import           Ethereum.Storage.Trie

data Account = Account {
        nonce     :: Integer,
        balance   :: Integer,
        stateRoot :: TreeRef,
        codeHash  :: CodeHash
        }
        deriving Show

data CodeHash = CodeHash Word256
              | NullCodeHash
              deriving Show

emptyHash :: Word256
emptyHash = hashBytes B.empty

instance Serialize CodeHash where
        put (CodeHash h) = putScalar256 h
        put (NullCodeHash) = putScalar256 (hashBytes B.empty)

        get = do h <- getScalar256
                 return $ if h == emptyHash
                          then NullCodeHash
                          else CodeHash h

instance Serialize Account where
        put a = putSequence $
                do putScalar $ nonce a
                   putScalar $ balance a
                   put $ stateRoot a
                   put $ codeHash a

        get = do len <- getSequenceHeader
                 isolate (fromIntegral len) $
                         do n <- getScalar
                            b <- getScalar
                            s <- get :: Get TreeRef
                            c <- get :: Get CodeHash
                            return $ Account n b s c

debit :: Account -> Integer -> Account
debit a@(Account {balance=b}) i = a { balance = b - i }

credit :: Account -> Integer -> Account
credit a@(Account {balance=b}) i = a { balance = b + i }

nextNonce :: Account -> Account
nextNonce a@(Account {nonce=n}) = a { nonce = n + 1 }
