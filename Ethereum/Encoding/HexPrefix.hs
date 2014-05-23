{- |
Module      :  Ethereum.Encoding.HexPrefix
Description :  Implementation of the Ethereum HexPrefix encoding
Copyright   :  (c) Richard Larocque
License     :  GPL-3.0+

Maintainer  :  richard.larocque@gmail.com
Stability   :  unstable
Portability :  non-portable (Unknown portability)

See Ethereum Yellow Paper, Proof-of-Concept V, Appendix D
-}

module Ethereum.Encoding.HexPrefix where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Word.Odd
import Ethereum.Encoding.RLP
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Ethereum.Common

data HPArray = HPArray [Word4] Bool
        deriving (Show,Eq)

putHexPrefix :: [Word4] -> Bool -> Put
putHexPrefix ns f =
        let bs = runPut (putHexPrefixBytes ns f)
        in putArray (L.toStrict bs)

getHexPrefix :: Bool -> Get [Word4]
getHexPrefix f = do
        len <- getArrayHeader
        isolate (fromIntegral len) (getHexPrefixBytes f)

putHexPrefixBytes :: [Word4] -> Bool -> Put
putHexPrefixBytes ns b = do
        let ft = if b then 2 else 0 :: Word8
        let (b0, rest) = case (even.length) ns of 
                True  -> (((16 * ft)       + (0)                     ),       ns )
                False -> (((16 * (ft + 1)) + ((fromIntegral.head) ns)), (tail ns))
        put b0
        mapM_ putWord8 (pairBytes rest)
        where pairBytes :: [Word4] -> [Word8]
              pairBytes (n1:n2:rest) = (makeByte n1 n2) : pairBytes rest
              pairBytes [] = []
              pairBytes [_] = error "Unexpected unpair input"
              makeByte h l = toHigh h + toLow l

getHexPrefixBytes :: Bool -> Get [Word4]
getHexPrefixBytes ef = do
        (HPArray ns af) <- getHexPrefix'
        if af == ef
           then return ns
           else fail "Flag did not match"

getHexPrefix' :: Get HPArray
getHexPrefix' = do
        b <- getWord8
        let ft = b `testBit` 5
        let isEven = not $ b `testBit` 4
        let prefix = if (isEven)
                        then []
                        else [lowNibble b]
        bs <- getRemainingLazyByteString
        return $ HPArray (prefix ++ (nibbleize (L.unpack bs))) ft
