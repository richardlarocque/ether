{- |
Module      :  Ethereum.EVM.FeeSchedule
Description :  Fee schedule declarations for Ethereum
Copyright   :  (c) Richard Larocque
License     :  GPL-3.0+

Maintainer  :  richard.larocque@gmail.com
Stability   :  unstable
Portability :  non-portable (Unknown portability)

Translation of Ethereum Yellow Paper, Proof-of-Concept V, Appendix B
-}

module Ethereum.EVM.ExecutionEnvironment(
       --ExecutionEnvironment(address,origin,caller,value,gasPrice),
       ExecutionEnvironment(..),
       crange,
       cbyte,
       clength,
       drange,
       dlength
) where

import Data.Word
import Data.LargeWord
import Ethereum.State.Address

import Ethereum.SimpleTypes

-- |The execution environment tuple defined in section 9.3.
data ExecutionEnvironment = EE {
        address :: Address,
        origin :: Address,
        gasPrice :: Ether,
        input :: ByteArray,
        caller :: Address,
        value :: Ether,
        code :: Code
}

type Code = ByteArray

intRange :: (Word256, Word256) -> (Int, Int)
intRange (a,b) = (fromIntegral a, fromIntegral b)

crange :: (Word256, Word256) -> ExecutionEnvironment -> ByteArray
crange range = (safeBrange (intRange range)).code

cbyte :: Word256 -> ExecutionEnvironment -> Word8
cbyte i = ((safeBbyte.fromIntegral) i).code

clength :: ExecutionEnvironment -> Int
clength = blength.code

drange :: (Word256, Word256) -> ExecutionEnvironment -> ByteArray
drange range = (safeBrange (intRange range)).input

dlength :: ExecutionEnvironment -> Int
dlength = blength.input
