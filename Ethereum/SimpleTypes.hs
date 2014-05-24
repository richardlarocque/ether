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
        safeBrange,
        bbyte,
        safeBbyte,
        blength,
        fromBytes,
        toBytes,
        emptyMemSlice,
        memToByteString,
        zeroAddress
) where

import Data.Binary
import Data.Maybe
import Data.LargeWord
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

-- TODO: Remove a bunch of these dumb definitions.
type Gas = Integer
type MemSlice = V.Vector Word8
type Stack = [Word256]
data Address = A Word160 deriving (Show, Eq)
type Ether = Integer
type ByteArray = V.Vector Word8

data RunTimeError = OutOfGas
                  | InvalidInstruction
                  | StackUnderflow
                  deriving (Show,Eq)

-- Addresses are 160 bits
fromAddress :: Integral a => Address -> a
fromAddress (A w) = fromIntegral w

fromEther :: Integral a => Ether -> a
fromEther = fromIntegral

brange :: (Int, Int) -> ByteArray -> ByteArray
brange (start, len) = V.slice start len

safeBrange :: (Int, Int) -> ByteArray -> ByteArray
safeBrange (start, len) bs = let bufEnd = blength bs
                                 overread = (start+len) - max start bufEnd
                                 suffix = V.replicate overread 0
                                 prefix = if start >= bufEnd
                                             then V.empty
                                             else brange (start, min len (bufEnd-start)) bs
                             in prefix V.++ suffix

bbyte :: Int -> ByteArray -> Word8
bbyte i bs = bs V.! i

safeBbyte :: Int -> ByteArray -> Word8
safeBbyte i bs = fromMaybe 0 $ bs V.!? i

blength :: ByteArray -> Int
blength = V.length

-- Converts a vector of big-endian bytes to a Word256
-- TODO: Do better than this?
fromBytes :: ByteArray -> Word256
fromBytes bs | V.length bs > 32 = error "Input list too long"
fromBytes bs = (decode . BL.pack . pad . V.toList) bs
        where pad xs = (replicate (32 - (length xs)) 0) ++ xs

toBytes :: Word256 -> ByteArray
toBytes = V.fromList . BL.unpack . encode

emptyMemSlice :: MemSlice
emptyMemSlice = V.empty

memToByteString :: ByteArray -> B.ByteString
memToByteString = B.pack . V.toList

zeroAddress :: Address
zeroAddress = A 0
