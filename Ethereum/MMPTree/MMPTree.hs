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

import Data.Array
import Data.Bits
import Data.LargeWord
import Data.Maybe
import Data.Word
import Data.Word.Odd
import qualified Data.List as DL
import qualified Data.Map as DM

data Tree = Empty
          | Leaf [Word4] Item
          | Extension [Word4] TreeRef
          | Branch (Array Word4 TreeRef) (Maybe Item)

data Item = Embedded [Word8]
          | HashRef Word256

data TreeRef = TreeHash Word256
             | Serialized [Word8]

type Storage = DM.Map Word256 [Word8]

instance Ix Word4 where
        range (a,b) = [a..b]
        inRange (l,u) i = l <= i && i < u

nibbleize :: [Word8] -> [Word4]
nibbleize bs = map fromIntegral $ concatMap toNibbles bs
        where toNibbles b = [b `shiftR` 4, b .&. 0x0f]

fromTreeRef :: Storage -> TreeRef -> Tree
fromTreeRef s tr =
        decodeTree $ case tr of
                (TreeHash h) -> fromMaybe (error "lookup failed") (DM.lookup h s)
                Serialized s -> s

decodeTree :: [Word8] -> Tree
decodeTree _ = Leaf [0] (Embedded [1,2,3])

path :: Storage -> Tree -> [Word4] -> Maybe [Tree]
path s tree ns = case tree of
        Empty -> Nothing

        Leaf ps i | ns == ps -> Just []
        Leaf _ _             -> Nothing

        Extension ps t -> do rest <- ns `DL.stripPrefix` ps
                             path' rest t

        Branch _ i  | null ns -> Just []
        Branch ts _           -> path' (tail ns) $ ts ! (head ns)
        where path' k tr = do next <- (path s (fromTreeRef s tr) k)
                              return $ tree:next

