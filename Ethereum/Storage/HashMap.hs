{- |
Module      :  Ethereum.Storage.HashMap
Description :  Persistent mapping of hashes to byestrings for Ethereum storage.
Copyright   :  (c) Richard Larocque
License     :  GPL-3.0+

Maintainer  :  richard.larocque@gmail.com
Stability   :  unstable
Portability :  non-portable (Unknown portability)

Translation of Ethereum Yellow Paper, Proof-of-Concept V, Appendix D
-}

module Ethereum.Storage.HashMap(
        MapStorage,
        emptyMapStorage,
        store,
        load) where

import Data.LargeWord
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as DM

type MapStorage = DM.Map Word256 L.ByteString

emptyMapStorage :: MapStorage
emptyMapStorage = DM.empty

store :: Word256 -> L.ByteString -> MapStorage -> MapStorage
store = DM.insert

load :: Word256 -> MapStorage -> Maybe L.ByteString
load = DM.lookup
