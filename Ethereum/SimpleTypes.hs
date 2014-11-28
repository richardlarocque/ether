{- |
Module      :  Ethereum.SimpleTypes
Description :  Type Declarations for Ethereum
Copyright   :  (c) Richard Larocque
License     :  GPL-3.0+

Maintainer  :  richard.larocque@gmail.com
Stability   :  unstable
Portability :  non-portable (Unknown portability)
-}

module Ethereum.SimpleTypes (
        Gas,
        Stack,
        Ether,
        fromEther,
        brange,
        safeBrange,
        bbyte,
        safeBbyte,
        blength,
        fromBytes,
        toBytes,
        emptyMemSlice,
        memToByteString
) where

import qualified Data.ByteString as B
import           Data.LargeWord
import           Data.Word
import           Ethereum.Common

-- TODO: Remove a bunch of these dumb definitions.
type Gas = Integer
type Stack = [Word256]
type Ether = Integer

fromEther :: Integral a => Ether -> a
fromEther = fromIntegral

brange :: (Int, Int) -> B.ByteString -> B.ByteString
brange (start, len) = B.take len . B.drop start

safeBrange :: (Int, Int) -> B.ByteString -> B.ByteString
safeBrange (start, len) bs = let bufEnd = blength bs
                                 overread = (start+len) - max start bufEnd
                                 suffix = B.replicate overread 0
                                 prefix = if start >= bufEnd
                                             then B.empty
                                             else brange (start, min len (bufEnd-start)) bs
                             in prefix `B.append` suffix

bbyte :: Int -> B.ByteString -> Word8
bbyte i bs = bs `B.index` i

safeBbyte :: Int -> B.ByteString -> Word8
safeBbyte i bs = if i >= B.length bs
                     then 0
                     else bs `B.index` i

blength :: B.ByteString -> Int
blength = B.length

-- Converts a vector of big-endian bytes to a Word256
-- TODO: Do better than this?
fromBytes :: B.ByteString -> Word256
fromBytes bs | B.length bs > 32 = error "Input list too long"
fromBytes bs = (decode256be . pad) bs
        where pad xs = B.replicate (32 - B.length xs) 0 `B.append` xs

toBytes :: Word256 -> B.ByteString
toBytes = encode256be

emptyMemSlice :: B.ByteString
emptyMemSlice = B.empty

memToByteString :: B.ByteString -> B.ByteString
memToByteString = id
