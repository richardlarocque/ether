{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{- |
Module      :  Ethereum.Storage.Trie
Description :  Modified Merkle Patricia Tree for Ethereum state tracking.
Copyright   :  (c) Richard Larocque
License     :  GPL-3.0+

Maintainer  :  richard.larocque@gmail.com
Stability   :  unstable
Portability :  non-portable (Unknown portability)

Translation of Ethereum Yellow Paper, Proof-of-Concept V, Appendix D
-}

module Ethereum.Storage.Trie(
        Item(..),
        TreeRef(..),
        Tree(..),
        initialTree,
        insert,
        Ethereum.Storage.Trie.lookup) where

-- TODO: Many of those exports are meant only for tests...

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Char
import Data.Array
import Data.Binary
import Data.Binary.Get
import Data.LargeWord
import Data.Maybe
import Data.Word.Odd
import qualified Data.List as DL
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Ethereum.Common
import Ethereum.Storage.HashMap
import Ethereum.Encoding.HexPrefix
import Ethereum.Encoding.RLP

data Tree = Empty
          | Leaf [Word4] Item
          | Extension [Word4] TreeRef
          | Branch (Array Word4 TreeRef) (Maybe Item)
          deriving (Show, Eq)

data Item = I B.ByteString
        deriving (Show, Eq)

data TreeRef = Serialized B.ByteString
             | TreeHash Word256 
             deriving (Show, Eq)

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
        put (I e) = putArray e
        get = getArray >>= return.I

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

tref :: Tree -> TreeRef
tref Empty = TreeHash 0
tref t     = let serialized = encode t
                  in if L.length serialized < 32
                        then Serialized (L.toStrict serialized)
                        else TreeHash $ hashLazyBytes serialized

deref :: TreeRef -> Reader MapStorage Tree
deref tr =
        case tr of
                TreeHash 0 -> return Empty
                TreeHash h -> do 
                        s <- ask
                        let bs = fromMaybe (error "lookup failed") (load h s) 
                        return $ runGet get bs
                Serialized bs -> return $ runGet get (L.fromStrict bs)

-----

zeroRef :: TreeRef
zeroRef = TreeHash 0

emptyBranch :: Tree
emptyBranch = Branch (listArray (0,15) (replicate 16 zeroRef)) Nothing

initialTree :: (MapStorage, TreeRef)
initialTree = (emptyMapStorage, zeroRef)

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

----

-- | Inserts an item into the trie.
insert :: (MapStorage, TreeRef) -> (String, Item) -> (MapStorage, TreeRef)
insert (s, tr) (k, v) =
        let (r', ns) = runReader doInsert s in (s `updateStorage` ns, tref r')
        where doInsert = do
                let k' = nibbleize $ map (fromIntegral.ord) k
                t <- deref tr
                treeInsert t (k', v)

-- | Looks up an item in the trie.
lookup :: (MapStorage, TreeRef) -> B.ByteString -> Maybe Item
lookup (s, tr) k = runReader doLookup s
        where doLookup = do let k' = nibbleize $ B.unpack k
                            deref tr >>= lookup' k'

-- Helpers

updateStorage :: MapStorage -> [Tree] -> MapStorage
updateStorage s ts = foldr addToStorage s ts
        where addToStorage t s1 = case tref t of
                TreeHash 0 -> s1
                TreeHash k -> store k (encode t) s1
                _ -> s1

lookup' :: [Word4] -> Tree -> Reader MapStorage (Maybe Item)
lookup' ns tree = case tree of
        Empty                   -> return Nothing

        Leaf ps i | ns == ps    -> return $ Just i
        Leaf _  _               -> return Nothing

        Extension ps t ->
                case ps `DL.stripPrefix` ns of
                        (Just rest) -> recurse rest t
                        Nothing     -> return Nothing

        Branch _  mi | null ns  -> return mi
        Branch ts _             -> recurse (tail ns) (ts ! head ns)

        where recurse k tr = deref tr >>= lookup' k

-----------------------------------

treeInsert :: Tree -> ([Word4], Item) -> Reader MapStorage (Tree, [Tree])

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
           then let e = Extension cp (tref b2)
                in return (e, [e] ++ [b2] ++ ts1 ++ ts2)
           else return    (b2,       [b2] ++ ts1 ++ ts2)

-- Extensions
treeInsert (Extension ek tr) (ik, iv) =
        -- NOTE: Our child tref *must* be a branch.
        -- If it wasn't then this node should instead be a longer leaf
        -- or a longer extension.
        case commonPrefix ek ik of
                ( _, esuf, isuf) | null esuf -> do
                        b <- deref tr
                        (b', ts) <- b `treeInsert` (isuf, iv)
                        let e' = Extension ek (tref b')
                        return (e', e':ts)


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
        t <- deref (ts ! i)
        (t', newNodes) <- treeInsert t (DL.tail ks, v)
        let b' = updateBranchSubTree b (i, t')
        return (b', b':newNodes)

-- Helper functions

-- | Prepend an extension branch if it makes sense to do so.
tryPrependPrefix :: [Word4] -> (Tree, [Tree]) -> (Tree, [Tree])
tryPrependPrefix ks x | null ks = x
tryPrependPrefix ks (r, ns) = let e = Extension ks (tref r) in (e, e:ns)

-- | Find the common prefix and remaining suffixes.
commonPrefix ::  Eq t => [t] -> [t] -> ([t], [t], [t])
commonPrefix [] ys = ([], [], ys)
commonPrefix xs [] = ([], xs, [])
commonPrefix (x:xs) (y:ys)
    | x == y    = (\(acc, a1, a2) -> (x:acc, a1, a2)) (commonPrefix xs ys)
    | otherwise = ([], x:xs, y:ys)

-- | Inserts a subtree into a branch node.
updateBranchSubTree :: Tree -> (Word4, Tree) -> Tree
updateBranchSubTree (Branch arr mi) (k, t) =
        let arr' = arr // [(k, tref t)]
        in Branch arr' mi
updateBranchSubTree _ _  = error "Function applies to branches only"
