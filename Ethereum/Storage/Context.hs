module Ethereum.Storage.Context where

import Data.Binary
import Control.Monad.Reader(runReader)
import Ethereum.State.Account
import Ethereum.State.Address
import Ethereum.Storage.HashMap
import Ethereum.Storage.Trie as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

data Context = Context MapStorage TreeRef

initContext :: Context
initContext = Context emptyMapStorage T.zeroRef

nullStateRoot :: TreeRef
nullStateRoot = zeroRef

getAccount :: Context -> Address -> Maybe Account
getAccount c addr = do a <- lookupInTrie c (addressAsKey addr)
                       return $ decode (L.fromStrict a)

updateAccount :: Context -> (Address, Account) -> Context
updateAccount c (addr, acc) =
        insertToTrie c (addressAsKey addr, (L.toStrict . encode) acc)

insertToTrie :: Context -> (B.ByteString, B.ByteString) -> Context
insertToTrie (Context s tr) kv =
        let (tr', newNodes) = runReader (T.insert tr kv) s
            s' = updateStorage s newNodes
        in Context s' tr'

lookupInTrie :: Context -> B.ByteString -> Maybe B.ByteString
lookupInTrie (Context s tr) k = runReader (T.lookup tr k) s

updateStorage :: MapStorage -> [Tree] -> MapStorage
updateStorage s ts = foldr (flip storeTree) s ts
