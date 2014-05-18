{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{- |
Module      :  Ethereum.EVM.FeeSchedule
Description :  Fee schedule declarations for Ethereum
Copyright   :  (c) Richard Larocque
License     :  GPL-3.0+

Maintainer  :  richard.larocque@gmail.com
Stability   :  unstable
Portability :  non-portable (Unknown portability)

Translation of Ethereum Yellow Paper, Proof-of-Concept V, Appendix D
-}

module Ethereum.MMPTree.MMPTree where

import Control.Applicative
import Control.Monad
import Data.Array
import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.LargeWord
import Data.Maybe
import Data.Word.Odd
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Data.ByteString.Lazy as L
import Ethereum.Encoding.HexPrefix
import Ethereum.Encoding.RLP

data Tree = Empty
          | Leaf [Word4] Item
          | Extension [Word4] TreeRef
          | Branch (Array Word4 TreeRef) (Maybe Item)
          deriving (Show, Eq)

data Item = Embedded L.ByteString
        deriving (Show, Eq)

data TreeRef = Serialized L.ByteString
             | TreeHash Word256 
             deriving (Show, Eq)

type Storage = DM.Map Word256 L.ByteString

instance Ix Word4 where
        range (a,b) = [a..b]
        index (a,b) i = fromIntegral $ i - a
        inRange (l,u) i = l <= i && i <= u

----

instance Binary TreeRef where
        put (Serialized bs) = putSequenceBytes bs
        put (TreeHash h) = putScalar256 h

        get = getHash <|> getSerialized
                where getHash = getScalar256 >>= return.TreeHash
                      getSerialized = getSequenceBytes >>= return.Serialized

instance Binary Item where
        put (Embedded e) = putArray e
        get = getArray >>= return.Embedded

instance Binary Tree where
        put (Leaf ns i) = putSequence $ do
                putHexPrefix ns True
                put i
        put (Extension ns tr) = putSequence $ do
                putHexPrefix ns False
                put tr
        put (Branch ts mi) = putSequence $ do
                mapM_ put $ elems ts
                case mi of
                        Just i -> put i
                        Nothing -> return ()

        get = getSequence (getLeaf <|> getExtension <|> getBranch)

zeroRef :: TreeRef
zeroRef = TreeHash 0

getLeaf :: Get Tree
getLeaf = do ns <- getHexPrefix True
             i <- get
             return $ Leaf ns i

getExtension :: Get Tree
getExtension = do ns <- getHexPrefix False
                  tr <- get
                  return $ Extension ns tr

getBranch :: Get Tree
getBranch = do
        trs <- replicateM 16 (get :: Get TreeRef)
        done <- isEmpty
        mi <- if not done
           then (get :: Get Item) >>= return.Just
           else return Nothing
        return $ Branch (listArray (0,15) trs) mi

putHexPrefix :: [Word4] -> Bool -> Put
putHexPrefix ns f =
        let bs = runPut (putHexPrefixBytes ns f)
        in putArray bs

getHexPrefix :: Bool -> Get [Word4]
getHexPrefix f = do
        len <- getArrayHeader
        isolate (fromIntegral len) (getHexPrefixBytes f)

nibbleize :: [Word8] -> [Word4]
nibbleize bs = map fromIntegral $ concatMap toNibbles bs
        where toNibbles b = [b `shiftR` 4, b .&. 0x0f]

fromTreeRef :: Storage -> TreeRef -> Tree
fromTreeRef s tr =
        case tr of
                TreeHash 0 -> Empty
                TreeHash h -> decodeTree $ fromMaybe (error "lookup failed") (DM.lookup h s)
                Serialized bs -> decodeTree $ bs

decodeTree :: L.ByteString -> Tree
decodeTree bs = runGet get bs

path :: Storage -> Tree -> [Word4] -> ([Tree], Maybe Item)
path s t k = path' [] (fromTreeRef s) t k

path' :: [Tree] -> (TreeRef -> Tree) -> Tree -> [Word4] -> ([Tree], Maybe Item)
path' acc tlookup tree ns = case tree of
        Leaf ps i | ns == ps           -> endHere $ Just i
        Leaf ps i | head ns == head ps -> endHere $ Nothing

        Extension ps t ->
                case ps `DL.stripPrefix` ns of
                        (Just rest) -> recurse rest t
                        Nothing     -> endHere $ Nothing

        Branch _  mi | null ns  -> endHere mi
        Branch ts _             -> recurse (tail ns) (ts ! head ns)

        where recurse k tr = path' (tree:acc) tlookup (tlookup tr) k
              endHere mi = (tree:acc, mi)

