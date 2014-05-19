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
import Control.Monad.Reader
import Data.Char
import Data.Array
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.LargeWord
import Data.Maybe
import Data.Word.Odd
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified Data.ByteString.Lazy as L
import Ethereum.Common
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
        index (a,_) i = fromIntegral $ i - a
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
        put (Empty) = error "Can't directly put empty tree"
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

        get = do len <- getSequenceHeader
                 isolate (fromIntegral len) (getLeaf <|> getExtension <|> getBranch)

-----

fromTreeRef :: Storage -> TreeRef -> Tree
fromTreeRef s tr =
        case tr of
                TreeHash 0 -> Empty
                TreeHash h -> decodeTree $ fromMaybe (error "lookup failed") (DM.lookup h s)
                Serialized bs -> decodeTree $ bs

toTreeRef :: Tree -> TreeRef
toTreeRef Empty = TreeHash 0
toTreeRef t     = let serialized = encode t
                  in if L.length serialized < 32
                        then Serialized serialized
                        else TreeHash $ hashLazyBytes serialized

decodeTree :: L.ByteString -> Tree
decodeTree bs = runGet get bs

lookupTree :: TreeRef -> Reader Storage Tree
lookupTree tr =
        case tr of
                TreeHash 0 -> return Empty
                TreeHash h -> do 
                        s <- ask
                        let bs = fromMaybe (error "lookup failed") (DM.lookup h s) 
                        return $ decodeTree bs
                Serialized bs -> return $ decodeTree bs

-----

zeroRef :: TreeRef
zeroRef = TreeHash 0

emptyBranch :: Tree
emptyBranch = Branch (listArray (0,15) (replicate 16 zeroRef)) Nothing

initialTree :: (Storage, TreeRef)
initialTree = (DM.empty, zeroRef)

-----

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

updateStorage :: Storage -> [Tree] -> Storage
updateStorage s ts = foldr doInsert s ts
        where doInsert t s1 = case toTreeRef t of
                TreeHash 0 -> s1
                TreeHash k -> DM.insert k (encode t) s1
                _ -> s1

insert :: (Storage, TreeRef) -> (String, Item) -> (Storage, TreeRef)
insert (s, tr) (k, v) =
        let (r', ns) = runReader doInsert s in (s `updateStorage` ns, toTreeRef r')
        where doInsert = do
                let k' = nibbleize $ map (fromIntegral.ord) k
                t <- lookupTree tr
                treeInsert t (k', v)

lookup :: (Storage, TreeRef) -> String -> Maybe Item
lookup (s, tr) k = runReader doLookup s
        where doLookup = do t <- lookupTree tr
                            let k' = nibbleize $ map (fromIntegral.ord) k
                            (_, i) <- path t k'
                            return i

path :: Tree -> [Word4] -> Reader Storage ([Tree], Maybe Item)
path t k = path' [] t k

path' :: [Tree] -> Tree -> [Word4] -> Reader Storage ([Tree], Maybe Item)
path' acc tree ns = case tree of
        Empty  -> endHere $ Nothing

        Leaf ps i | ns == ps           -> endHere $ Just i
        Leaf _  _                      -> endHere $ Nothing

        Extension ps t ->
                case ps `DL.stripPrefix` ns of
                        (Just rest) -> recurse rest t
                        Nothing     -> endHere $ Nothing

        Branch _  mi | null ns  -> endHere mi
        Branch ts _             -> recurse (tail ns) (ts ! head ns)

        where recurse k tr =
                do t <- lookupTree tr
                   path' (tree:acc) t k
              endHere mi = return (tree:acc, mi)

-----------------------------------

treeInsert :: Tree -> ([Word4], Item) -> Reader Storage (Tree, [Tree])

-- Empty
treeInsert Empty (ik, iv) =
        let l = Leaf ik iv in return (l, [l])

-- Leaves
treeInsert (Leaf lk _) (ik, iv) | ik == lk =
        return $ let l' = (Leaf ik iv) in (l', [l'])
treeInsert (Leaf lk lv) (ik, iv) = do
        let (cp, lsuf, isuf) = commonPrefix lk ik
        (b1, ts1) <- emptyBranch `treeInsert` (isuf, iv)
        (b2, ts2) <- b1          `treeInsert` (lsuf, lv)
        if (not.null) cp
           then let e = Extension cp (toTreeRef b2)
                in return (e, [e] ++ [b2] ++ ts1 ++ ts2)
           else return    (b2,       [b2] ++ ts1 ++ ts2)

-- Extensions
treeInsert (Extension ek tr) (ik, iv) =
        -- NOTE: Our child tref *must* be a branch.
        -- If it wasn't then this node should instead be a longer leaf
        -- or a longer extension.
        case commonPrefix ek ik of
                ( _, esuf, isuf) | null esuf -> do
                        b <- lookupTree tr
                        b `treeInsert` (isuf, iv)

                (cp, esuf, isuf) -> do
                        let e1 = Extension (tail esuf) tr
                        let b1 = emptyBranch `updateBranchSubTree` (head esuf, e1)
                        (b2, ts) <- b1 `treeInsert` (isuf, iv)
                        return $ tryPrependPrefix cp $ (b2, [b2,e1] ++ ts)

-- Branches
treeInsert (Branch ts _) (ks, v) | null ks =
        let b' = Branch ts (Just v) in return (b', [b'])
treeInsert b@(Branch ts _) (ks, v) = do
        let i = DL.head ks
        t <- lookupTree (ts ! i)
        (t', newNodes) <- treeInsert t (DL.tail ks, v)
        let b' = updateBranchSubTree b (i, t')
        return (b', b':newNodes)

tryPrependPrefix :: [Word4] -> (Tree, [Tree]) -> (Tree, [Tree])
tryPrependPrefix ks x | null ks = x
tryPrependPrefix ks (r, ns) = let e = Extension ks (toTreeRef r) in (e, e:ns)

commonPrefix ::  Eq t => [t] -> [t] -> ([t], [t], [t])
commonPrefix [] ys = ([], [], ys)
commonPrefix xs [] = ([], xs, [])
commonPrefix (x:xs) (y:ys)
    | x == y    = (\(acc, a1, a2) -> (x:acc, a1, a2)) (commonPrefix xs ys)
    | otherwise = ([], x:xs, y:ys)

updateBranchSubTree :: Tree -> (Word4, Tree) -> Tree
updateBranchSubTree (Branch arr mi) (k, t) =
        let arr' = arr // [(k, toTreeRef t)]
        in Branch arr' mi
updateBranchSubTree _ _  = error "Function applies to branches only"
