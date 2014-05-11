{- |
Module      :  Ethereum.EVM.MachineState
Description :  Defines Ethereum VM machine state and transformations.
Copyright   :  (c) Richard Larocque
License     :  GPL-3.0+

Maintainer  :  richard.larocque@gmail.com
Stability   :  unstable
Portability :  non-portable (Unknown portability)

See Ethereum Yellow Paper, Proof-of-Concept V, Section 9
-}

module Ethereum.EVM.MachineState(
        MachineState,
        pc,
        gas,
        setPC,
        initialState,
        mstore,
        mload,
        push,
        pop,
        popTwo,
        outOfGas) where

import Data.LargeWord
import Data.Lens.Common
import Data.Maybe
import qualified Data.Map as M

import Ethereum.EVM.ExecutionEnvironment
import Ethereum.SimpleTypes

-- |The machine state tuple defined in section 9.4.
data MachineState = MS {
        gas :: Gas,
        pc :: Integer,
        memory :: Memory,
        memsize :: Integer,
        stack :: Stack
}

initialState :: ExecutionEnvironment -> MachineState
initialState ee = MS {
        gas = (value ee) `div` (gasPrice ee),
        pc = 0,
        memory = M.empty,
        memsize = 0,
        stack = []
}

mstore :: Word256 -> Word256 -> MachineState -> MachineState
mstore addr value = (setMem addr value) . (expandMem addr)

mload :: Word256 -> MachineState -> (MachineState, Word256)
mload addr ms = (expandMem addr ms, getMem addr ms)

crange :: (Word256,Word256) -> MachineState -> MemSlice

push :: Word256 -> MachineState -> MachineState
push x = stack' ^%= (x:)

pop :: MachineState -> Either RunTimeError (MachineState, Word256)
pop ms@MS{stack=a:s'} = Right (ms{stack=s'}, a)
pop _ = Left StackUnderflow

popTwo :: MachineState -> Either RunTimeError (MachineState, (Word256, Word256))
popTwo ms@MS{stack=a1:a2:s'} = Right (ms{stack=s'}, (a1,a2))
popTwo _ = Left StackUnderflow

outOfGas :: MachineState -> Bool
outOfGas ms = (gas ms) < 0

setPC :: (Integral a) => a -> MachineState -> MachineState
setPC x ms = ms {pc=(fromIntegral x)}

-- Internal functions.

memory' :: Lens MachineState Memory
memory' = lens (memory) (\x ms -> ms {memory=x})

memsize' :: Lens MachineState Integer
memsize' = lens (memsize) (\x ms -> ms {memsize=x})

stack' :: Lens MachineState Stack
stack' = lens (stack) (\x ms -> ms {stack=x})

expandMem :: Word256 -> MachineState -> MachineState
expandMem addr = memsize' ^%= (\oldsize -> max oldsize (roundedRange addr))
        where roundedRange a = (fromIntegral a + 32) `ceilDiv` 32
              ceilDiv x d = (x + d - 1) `div` d

setMem :: Word256 -> Word256 -> MachineState -> MachineState
setMem addr value = memory' ^%= (M.insert addr value)

getMem :: Word256 -> MachineState -> Word256
getMem addr = (fromMaybe 0) . (M.lookup addr) . (getL memory')

