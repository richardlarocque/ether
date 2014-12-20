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

import           Control.Applicative
import qualified Data.ByteString       as B
import           Data.LargeWord
import           Ethereum.Crypto.Hash
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
emptyHash = hashAsWord B.empty

instance RLPSerialize CodeHash where
    toRLP (CodeHash h) = toRLP h
    toRLP NullCodeHash = toRLP emptyHash
    fromRLP i = case fromRLP i of
                  Nothing -> Nothing
                  Just h | h == emptyHash -> Just NullCodeHash
                  Just h                  -> Just $ CodeHash h

instance RLPSerialize Account where
    toRLP a = Group [
               toRLP $ nonce a,
               toRLP $ balance a,
               toRLP $ stateRoot a,
               toRLP $ codeHash a ]
    fromRLP (Group [n, b, sr, ch]) =
        return Account <*> fromRLP n <*> fromRLP b <*> fromRLP sr <*> fromRLP ch
    fromRLP _ = Nothing

debit :: Account -> Integer -> Account
debit a@(Account {balance=b}) i = a { balance = b - i }

credit :: Account -> Integer -> Account
credit a@(Account {balance=b}) i = a { balance = b + i }

nextNonce :: Account -> Account
nextNonce a@(Account {nonce=n}) = a { nonce = n + 1 }
