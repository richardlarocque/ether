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
        emptyMemSlice
) where

import Data.Binary
import Data.ByteString as B
import Data.LargeWord
import Data.Word
import Data.Vector as V
import qualified Data.Map as M

type Gas = Integer
type MemSlice = Vector Word8
type Stack = [Word256]
data Address = Address
type Ether = Integer
type ByteArray = Vector Word8

data RunTimeError = OutOfGas
                  | InvalidInstruction
                  | StackUnderflow
                  deriving (Show,Eq)

-- Addresses are 160 bits
fromAddress :: Address -> Word256
fromAddress a = 0  -- FIXME

fromEther :: Ether -> Word256
fromEther e = 0  -- FIXME

-- FIXME: brange and bbyte need to support out of range
brange :: Integral a => (a, a) -> ByteArray -> ByteArray
brange (start, end) = slice (fromIntegral start) (fromIntegral end)

bbyte :: Integral a => a -> ByteArray -> Word8
bbyte i bs = bs ! (fromIntegral i)

blength :: ByteArray -> Int
blength = V.length

-- Converts a vector of big-endian bytes to a Word256
-- TODO: Do better than this?
fromBytes :: ByteArray -> Word256
fromBytes bs | V.length bs > 32 = error "Input list too long"
fromBytes bs = (decode . encode . B.pack . V.toList) bs

toBytes :: Word256 -> ByteArray
toBytes = V.fromList . B.unpack . decode . encode

emptyMemSlice = V.empty
