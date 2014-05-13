module Ethereum.EVM.ExecutionEnvironment(
       --ExecutionEnvironment(address,origin,caller,value,gasPrice),
       ExecutionEnvironment(..),
       startGas,
       crange,
       cbyte,
       clength,
       drange,
       dlength
) where

import Data.Word
import Data.LargeWord

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

startGas :: ExecutionEnvironment -> Gas
startGas ee = (value ee) `div` (gasPrice ee)

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
