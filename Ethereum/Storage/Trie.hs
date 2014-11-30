{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
        TreeRef(..),
        Tree(..),
        zeroRef,
        insert,
        storeTree,
        Ethereum.Storage.Trie.lookup) where

-- TODO: Many of those exports are meant only for tests...

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader
import           Data.Array
import qualified Data.ByteString             as B
import           Data.LargeWord
import qualified Data.List                   as DL
import           Data.Maybe
import           Data.Serialize
import           Data.Word.Odd
import           Ethereum.Common
import           Ethereum.Encoding.HexPrefix
import           Ethereum.Encoding.RLP
import           Ethereum.Storage.HashMap

-- TODO: Stop using this.
ignoreFailure :: Either a b -> b
ignoreFailure (Right b) = b

data Tree = Empty
          | Leaf [Word4] B.ByteString
          | Extension [Word4] TreeRef
          | Branch (Array Word4 TreeRef) (Maybe B.ByteString)
          deriving (Show, Eq)

data TreeRef = Serialized B.ByteString
             | TreeHash Word256
             deriving (Show, Eq)

instance Ix Word4 where
        range (a,b) = [a..b]
        index (a,_) i = fromIntegral $ i - a
        inRange (l,u) i = l <= i && i <= u

----

instance Serialize TreeRef where
        put (Serialized bs) = putSequenceBytes bs
        put (TreeHash h) = put256 h

        get = getHash <|> getSerialized
                where getHash = liftM TreeHash get256
                      getSerialized = liftM Serialized getSequenceBytes

instance Serialize Tree where
        put (Empty) = error "Can't directly put empty tree"
        put (Leaf ns i) = putSequence $ do
                putHexPrefix ns True
                putArray i
        put (Extension ns tr) = putSequence $ do
                putHexPrefix ns False
                put tr
        put (Branch ts mi) = putSequence $ do
                mapM_ put $ elems ts
                case mi of
                        Just i -> putArray i
                        Nothing -> return ()

        get = do len <- liftM fromIntegral getSequenceHeader
                 isolate len getLeaf
                  <|> isolate len getExtension
                  <|> isolate len getBranch

-----

tref :: Tree -> TreeRef
tref Empty = TreeHash 0
tref t     = let serialized = encode t
                  in if B.length serialized < 32
                        then Serialized serialized
                        else TreeHash $ hashBytes serialized

deref :: TreeRef -> Reader MapStorage Tree
deref tr =
        case tr of
                TreeHash 0 -> return Empty
                TreeHash h -> do
                        s <- ask
                        let bs = fromMaybe (error "lookup failed") (load h s)
                        return $ ignoreFailure $ runGet get bs
                Serialized bs -> return $ ignoreFailure $ runGet get bs

-----

zeroRef :: TreeRef
zeroRef = TreeHash 0

emptyBranch :: Tree
emptyBranch = Branch (listArray (0,15) (replicate 16 zeroRef)) Nothing

-----

getLeaf :: Get Tree
getLeaf = do ns <- getHexPrefix True
             i <- getArray
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
           then liftM Just getArray
           else return Nothing
        return $ Branch (listArray (0,15) trs) mi


insert :: TreeRef -> (B.ByteString, B.ByteString) -> Reader MapStorage (TreeRef, [Tree])
insert tr (k, v) = do t <- deref tr
                      (tr', nodes) <- treeInsert t (nibbleize $ B.unpack k, v)
                      return (tref tr', nodes)

lookup :: TreeRef -> B.ByteString -> Reader MapStorage (Maybe B.ByteString)
lookup tr k = do t <- deref tr
                 lookup' (nibbleize $ B.unpack k) t

storeTree :: MapStorage -> Tree -> MapStorage
storeTree s t = case tref t of
        TreeHash 0 -> s
        TreeHash k -> store k (encode t) s
        _ -> s

-- Helpers

lookup' :: [Word4] -> Tree -> Reader MapStorage (Maybe B.ByteString)
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

treeInsert :: Tree -> ([Word4], B.ByteString) -> Reader MapStorage (Tree, [Tree])

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
