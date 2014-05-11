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
        Memory,
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
        blength) where

import Data.LargeWord
import Data.Word
import Data.Array
import qualified Data.Map as M

type Gas = Integer
type Memory = M.Map Word256 Word256
type MemSlice = [Word256]
type Stack = [Word256]
data Address = Address
type Ether = Integer
type ByteArray = Array Integer Word8

data RunTimeError = OutOfGas
                  | InvalidInstruction
                  | StackUnderflow
                  deriving (Show,Eq)

-- Addresses are 160 bits
fromAddress :: Address -> Word256
fromAddress a = 0  -- FIXME

fromEther :: Ether -> Word256
fromEther e = 0  -- FIXME

brange :: Integral a => (a, a) -> ByteArray -> [Word8]
brange (start, end) bs = map (\i -> bbyte i bs) [start..end-1]

bbyte :: Integral a => a -> ByteArray -> Word8
bbyte i bs = bs ! (fromIntegral i)

blength :: ByteArray -> Integer
blength = snd.bounds
