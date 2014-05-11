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

crange :: Integral a => (a, a) -> ExecutionEnvironment -> ByteArray
crange range = (brange range).code

cbyte :: Integral a => a -> ExecutionEnvironment -> Word8
cbyte i = (bbyte i).code

clength :: ExecutionEnvironment -> Int
clength = blength.code

drange :: Integral a => (a, a) -> ExecutionEnvironment -> ByteArray
drange range = (brange range).input

dlength :: ExecutionEnvironment -> Int
dlength = blength.input
