module Ethereum.State.Context where

import Data.Binary
import Ethereum.State.Account
import Ethereum.State.Address
import Ethereum.Storage.HashMap
import Ethereum.Storage.Trie as T

data Context = Context MapStorage TreeRef

getAccount :: Context -> Address -> Maybe Account
getAccount (Context s r) addr = do (I a) <- T.lookup (s, r) (asBigEndian addr)
                                   return $ decode a

