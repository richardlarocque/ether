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

import Data.Binary
import Data.Binary.Get
import Data.LargeWord
import Ethereum.Common
import Ethereum.Encoding.RLP
import Ethereum.Storage.Trie
import qualified Data.ByteString as B

data Account = Account {
        nonce :: Integer,
        balance :: Integer,
        stateRoot :: TreeRef,
        codeHash :: CodeHash
        }
        deriving Show

data CodeHash = CodeHash Word256
              | NullCodeHash
              deriving Show

emptyHash :: Word256
emptyHash = hashBytes B.empty

instance Binary CodeHash where
        put (CodeHash h) = putScalar256 h
        put (NullCodeHash) = putScalar256 (hashBytes B.empty)

        get = do h <- getScalar256
                 if h == emptyHash
                    then return $ NullCodeHash
                    else return $ CodeHash h

instance Binary Account where
        put a = putSequence $ do
                        putScalar $ nonce a
                        putScalar $ balance a
                        put $ stateRoot a
                        put $ codeHash a

        get = do len <- getSequenceHeader
                 isolate (fromIntegral len) $ do
                         n <- getScalar
                         b <- getScalar
                         s <- (get :: Get TreeRef)
                         c <- (get :: Get CodeHash)
                         return $ Account n b s c

