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
import Data.ByteString as B
import Ethereum.State.Address
import Ethereum.State.Block

import Ethereum.SimpleTypes

-- |The execution environment tuple defined in section 9.3.
data ExecutionEnvironment = EE {
        address :: Address,
        origin :: Address,
        gasPrice :: Integer,
        input :: B.ByteString,
        caller :: Address,
        value :: Integer,
        code :: B.ByteString,
        blockHeader :: BlockHeader,
        depth :: Int
}

intRange :: (Word256, Word256) -> (Int, Int)
intRange (a,b) = (fromIntegral a, fromIntegral b)

crange :: (Word256, Word256) -> ExecutionEnvironment -> B.ByteString
crange range = safeBrange (intRange range) . code

cbyte :: Word256 -> ExecutionEnvironment -> Word8
cbyte i = (safeBbyte.fromIntegral) i . code

clength :: ExecutionEnvironment -> Int
clength = blength.code

drange :: (Word256, Word256) -> ExecutionEnvironment -> B.ByteString
drange range = safeBrange (intRange range) . input

dlength :: ExecutionEnvironment -> Int
dlength = blength.input
