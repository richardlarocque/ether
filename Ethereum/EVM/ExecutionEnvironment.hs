module Ethereum.EVM.ExecutionEnvironment(
       ExecutionEnvironment(address,origin,caller,value),
       startGas,
       crange,
       cbyte,
       clength,
       drange,
       dlength
) where

import Data.Array
import Data.Word

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

type Code = Array Integer Word8

startGas :: ExecutionEnvironment -> Gas
startGas ee = (value ee) `div` (gasPrice ee)

crange :: Integral a => (a, a) -> ExecutionEnvironment -> [Word8]
crange range = (brange range).code

cbyte :: Integral a => a -> ExecutionEnvironment -> Word8
cbyte i = (bbyte i).code

clength :: ExecutionEnvironment -> Integer
clength = blength.code

drange :: Integral a => (a, a) -> ExecutionEnvironment -> [Word8]
drange range = (brange range).input

dlength :: ExecutionEnvironment -> Integer
dlength = blength.input
