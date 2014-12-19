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

import           Debug.Trace

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

-- instance Serialize TreeRef where
--         put (Serialized bs) = putByteString bs
--         --put (TreeHash 0) = putArray B.empty -- Empty tree == RLP Null == empty array
--         put (TreeHash h) = put256 h  -- FIXME is this right? put256AsArray???
--
--         get = (liftM (TreeHash . decode256be) getArray)
--             <|> (liftM Serialized $ do { len <- getSequenceHeader; bs <- getByteString len; return $ runPut $ putSequenceBytes bs }) -- FIXME: awful hack
--             <|> (do { len <- remaining; x <- getBytes len; traceShow ("Complete failure: " ++ (show x)) (error "x") } )
--          --bs <- traceShow "Getting TreeRef" getArray
--          --        return $ case B.length bs of
--          --                   0 -> TreeHash 0
--          --                   x | x < 32 -> Serialized bs
--          --                   x | x == 32 -> TreeHash $ decode256be bs
--          --                   _ -> error "Strange parse"

treeRefToRLP :: TreeRef -> RLP
treeRefToRLP (Serialized bs) = Item bs
treeRefToRLP (TreeHash h) = scalarToRLP h

treeToRLP :: Tree -> RLP
treeToRLP Empty = error "Can't directly serialize empty tree"
treeToRLP (Leaf ns i) = Group [Item (asHexPrefix ns True), Item i]
treeToRLP (Extension ns tr) = Group [Item (asHexPrefix ns False),
                                     treeRefToRLP tr]
treeToRLP (Branch ts mi) =
    Group $ map treeRefToRLP (elems ts) ++ [lastItem]
    where lastItem = Item $ fromMaybe B.empty mi

treeRefFromRLP :: RLP -> Maybe TreeRef
treeRefFromRLP (Item bs) | B.length bs > 32 = mzero
treeRefFromRLP (Item bs) =
    return $ case runGet get bs of
      Right (Group x) -> traceShow ("Serialized: " ++ show x) $ Serialized bs
      _               -> traceShow ("TreeHash: " ++ show bs) $ TreeHash $ decode256be bs

treeRefFromRLP (Item bs) | B.length bs <  32 = return $ Serialized bs
treeRefFromRLP (Item bs) | B.length bs == 32 = return $ TreeHash (decode256be bs)
treeRefFromRLP _                             = mzero

treeFromRLP :: RLP -> Maybe Tree
treeFromRLP (Group [Item hp, i@(Item bs)]) =
    case unHexPrefix hp of
      Right (HPArray ns True)  -> return $ Leaf ns bs
      Right (HPArray ns False) ->
          case treeRefFromRLP i of
            Nothing -> mzero
            Just tr -> return $ Extension ns tr
      _ -> mzero

treeFromRLP (Group ts) | length ts == 17 && all isItem ts =
    let (bs, [Item li]) = splitAt 16 ts
        lv = if B.null li then Nothing else Just li
    in do bv <- mapM treeRefFromRLP bs
          return $ Branch (listArray (0,15) bv) lv
treeFromRLP _ = mzero

instance Serialize Tree where
    put t = (\x -> traceShow ("Serializing tree: " ++ show (t,x)) $ put x) (treeToRLP t)
    get = get >>= \t -> traceShow ("Unserializing tree: " ++ show t) (maybe mzero return (treeFromRLP t))

instance Serialize TreeRef where
    --put = put . treeRefToRLP
    put t = (\x -> traceShow ("Serializing treeref: " ++ show (t,x)) $ put x) (treeRefToRLP t)
    get = get >>= \t -> traceShow ("Unserializing treeref: " ++ show t) (maybe mzero return (treeRefFromRLP t))

-- instance Serialize Tree where
--         put (Empty) = error "Can't directly put empty tree"
--         put (Leaf ns i) = putSequence $ do
--                 putHexPrefix ns True
--                 putArray i
--         put (Extension ns tr) = putSequence $ do
--                 putHexPrefix ns False
--                 put tr
--         put b@(Branch ts mi) = traceShow ("PUTTING: " ++ show b) putSequence $ do
--                 mapM_ put $ elems ts
--                 case mi of
--                         Just i -> putArray i
--                         Nothing -> putArray B.empty
--
--         get = do len <- liftM fromIntegral getSequenceHeader
--                  traceShow ("TreeGet len: " ++ (show len)) $ isolate len getLeaf
--                   <|> isolate len getExtension
--                   <|> isolate len getBranch

-----

tref :: Tree -> TreeRef
tref Empty = TreeHash 0
tref t     = let serialized = encode t
                  in if B.length serialized < 32
                        then traceShow ( "Will Serialize " ++ show (t, serialized) ) (Serialized serialized)
                        else TreeHash $ hashAsWord serialized

deref :: TreeRef -> Reader MapStorage Tree
deref tr =
        case tr of
                TreeHash 0 -> return Empty
                TreeHash h -> do
                        s <- ask
                        let bs = fromMaybe (error "lookup failed") (load h s)
                        return $ case runGet get bs of
                                   Left err -> error ("TreeHash runget Failed: " ++ show bs)
                                   Right x -> x
                Serialized bs -> return $ case runGet get bs of
                                            Left err -> error ("Serialized runget Failed " ++ show bs)
                                            Right x -> x

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
        t <- getArray
        let mi = if B.length t == 0
                 then Nothing
                 else Just t
        return $ Branch (listArray (0,15) trs) mi

insert :: TreeRef -> (B.ByteString, B.ByteString) -> Reader MapStorage (TreeRef, [Tree])
insert tr (k, v) = do t <- deref tr
                      (tr', nodes) <- treeInsert t (nibbleize $ B.unpack k, v)
                      return $ traceShow ("path" ++ show (map (\x -> (tref x, x)) nodes)) $ (tref tr', nodes)

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
-- TODO: Figure out this tangled mess...

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
