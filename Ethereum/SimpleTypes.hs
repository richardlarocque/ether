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
        MemSlice,
        Stack,
        Address(..),
        Ether,
        ByteArray,
        RunTimeError(..),
        fromAddress,
        fromEther,
        brange,
        bbyte,
        blength,
        fromBytes,
        toBytes,
        emptyMemSlice,
        memToByteString
) where

import Data.Binary
import Data.LargeWord
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V

type Gas = Integer
type MemSlice = V.Vector Word8
type Stack = [Word256]
data Address = Address
type Ether = Integer
type ByteArray = V.Vector Word8

data RunTimeError = OutOfGas
                  | InvalidInstruction
                  | StackUnderflow
                  deriving (Show,Eq)

-- Addresses are 160 bits
fromAddress :: Address -> Word256
fromAddress _ = 0  -- FIXME

fromEther :: Ether -> Word256
fromEther _ = 0  -- FIXME

-- FIXME: brange and bbyte need to support out of range
brange :: Integral a => (a, a) -> ByteArray -> ByteArray
brange (start, len) = V.slice (fromIntegral start) (fromIntegral len)

bbyte :: Integral a => a -> ByteArray -> Word8
bbyte i bs = bs V.! (fromIntegral i)

blength :: ByteArray -> Int
blength = V.length

-- Converts a vector of big-endian bytes to a Word256
-- TODO: Do better than this?
fromBytes :: ByteArray -> Word256
fromBytes bs | V.length bs > 32 = error "Input list too long"
fromBytes bs = (decode . B.pack . pad . V.toList) bs
        where pad xs = (replicate (32 - (length xs)) 0) ++ xs

toBytes :: Word256 -> ByteArray
toBytes = V.fromList . B.unpack . encode

emptyMemSlice :: MemSlice
emptyMemSlice = V.empty

memToByteString :: ByteArray -> B.ByteString
memToByteString = B.pack . V.toList
