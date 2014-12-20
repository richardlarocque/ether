module Ethereum.Storage.Context where

import           Control.Monad.Reader     (runReader)
import qualified Data.ByteString          as B
import           Data.LargeWord
import           Data.Maybe
import           Data.Serialize
import           Ethereum.SimpleTypes
import           Ethereum.State.Account
import           Ethereum.State.Address
import           Ethereum.Storage.HashMap
import           Ethereum.Storage.Trie    as T

data Context = Context MapStorage TreeRef

initContext :: Context
initContext = Context emptyMapStorage T.zeroRef

nullStateRoot :: TreeRef
nullStateRoot = zeroRef

rootHash :: Context -> Word256
rootHash (Context _ (TreeHash h)) = h
rootHash (Context _ _) = undefined

getAccount :: Context -> Address -> Maybe Account
getAccount c addr = do a <- lookupInTrie c (addressAsKey addr)
                       case decode a of
                         Left _ -> Nothing
                         Right x -> Just x

updateAccount :: Context -> (Address, Account) -> Context
updateAccount c (addr, acc) =
        insertToTrie c (addressAsKey addr, encode acc)

modifyAccount :: Context -> Address -> (Account -> Account) -> Maybe Context
modifyAccount c addr f = do acc <- getAccount c addr
                            return $ updateAccount c (addr, f acc)

insertToTrie :: Context -> (B.ByteString, B.ByteString) -> Context
insertToTrie(Context s tr) kv =
        let (tr', newNodes) = runReader (T.insert tr kv) s
            s' = updateStorage s newNodes
        in Context s' tr'

lookupInTrie :: Context -> B.ByteString -> Maybe B.ByteString
lookupInTrie (Context s tr) k = runReader (T.lookup tr k) s

updateStorage :: MapStorage -> [Tree] -> MapStorage
updateStorage s ts = foldr (flip storeTree) s ts

insertToStorage :: Context -> (Word256, B.ByteString) -> Context
insertToStorage (Context s tr) (h, bs) = Context (store h bs s) tr

lookupInStorage :: Context -> Word256 -> Maybe B.ByteString
lookupInStorage (Context s _) h = load h s

accountStore :: Context -> Address -> (Word256, Word256) -> Context
accountStore c@(Context s tr) addr (k, v) =
        fromMaybe c $
        do acc <- getAccount c addr
           let accountContext = Context s (stateRoot acc)
           let (kb, vb) = (toBytes k, toBytes v)
           let (Context s' acc_tr') = insertToTrie accountContext (kb, vb)
           let acc' = acc{stateRoot=acc_tr'}
           let c' = Context s' tr
           return $ updateAccount c' (addr, acc')

accountLoad :: Context -> Address -> Word256 -> Word256
accountLoad c@(Context s _) addr k =
        fromMaybe 0 $
        do acc <- getAccount c addr
           let accountContext = Context s (stateRoot acc)
           let kb = toBytes k
           vb <- lookupInTrie accountContext kb
           return $ fromBytes vb
