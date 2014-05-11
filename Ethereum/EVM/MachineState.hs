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
        incPC,
        getOp,
        initialState,
        mstore,
        mload,
        crange,
        push,
        pop,
        popTwo,
        popThree,
        outOfGas) where

import Data.LargeWord
import Data.Lens.Common
import Data.Maybe
import Data.Word
import Data.Vector as V
import qualified Data.Map as M

import Ethereum.EVM.ExecutionEnvironment
import Ethereum.EVM.InstructionSet
import Ethereum.SimpleTypes

type Memory = ByteArray

-- |The machine state tuple defined in section 9.4.
data MachineState = MS {
        gas :: Gas,
        pc :: Integer,
        memory :: ByteArray,
        memsize :: Integer,
        stack :: Stack
}

initialState :: ExecutionEnvironment -> MachineState
initialState ee = MS {
        gas = startGas ee,
        pc = 0,
        memory = V.replicate 512 0,
        memsize = 0,
        stack = []
}

mstore :: Word256 -> Word256 -> MachineState -> MachineState
mstore addr value = (setMem (fromIntegral $ addr*32) (toBytes value)) . (expandMem addr)

mload :: Word256 -> MachineState -> (MachineState, Word256)
mload addr ms = let ms' = expandMem addr ms
                in (ms', fromBytes $ getMem (fromIntegral $ addr*32) 32 ms')

push :: Word256 -> MachineState -> MachineState
push x = stack' ^%= (x:)

pop :: MachineState -> Either RunTimeError (MachineState, Word256)
pop ms@MS{stack=a:s'} = Right (ms{stack=s'}, a)
pop _ = Left StackUnderflow

popTwo :: MachineState -> Either RunTimeError (MachineState, (Word256, Word256))
popTwo ms@MS{stack=a1:a2:s'} = Right (ms{stack=s'}, (a1,a2))
popTwo _ = Left StackUnderflow

popThree ms@MS{stack=a1:a2:a3:s'} = Right (ms{stack=s'}, (a1,a2,a3))
popThree _ = Left StackUnderflow

outOfGas :: MachineState -> Bool
outOfGas ms = (gas ms) < 0

setPC :: (Integral a) => a -> MachineState -> MachineState
setPC x = pc' ^= fromIntegral x

incPC :: MachineState -> MachineState
incPC = pc' ^+= 1

-- Equation 86
getOp :: ExecutionEnvironment -> MachineState -> Maybe Instruction
getOp ee ms =
        if pc ms < (fromIntegral $ clength ee)
           then fromOpcode $ cbyte (pc ms) ee
           else Just STOP

-- Internal functions.

memory' :: Lens MachineState Memory
memory' = lens (memory) (\x ms -> ms {memory=x})

memsize' :: Lens MachineState Integer
memsize' = lens (memsize) (\x ms -> ms {memsize=x})

stack' :: Lens MachineState Stack
stack' = lens (stack) (\x ms -> ms {stack=x})

pc' :: Lens MachineState Integer
pc' = lens (pc) (\x ms -> ms {pc=x})

expandMem :: Word256 -> MachineState -> MachineState
expandMem addr =
        let target = (fromIntegral addr + 32) `ceilDiv` 32
        in (updateMemSize target) . (updateMemVector target)
        where ceilDiv x d = (x + d - 1) `div` d

updateMemSize :: Int -> MachineState -> MachineState
updateMemSize target = memsize' ^%= (max (fromIntegral target))

updateMemVector :: Int -> MachineState -> MachineState
updateMemVector target ms@MS{memory=origMem} =
        let origSize = blength origMem
        in if (origSize < target)
              then ms {memory= origMem V.++ (V.replicate origSize 0)}
              else ms

setMem :: Int -> ByteArray -> MachineState -> MachineState
setMem baddr val = memory' ^%= (\mem -> update mem (V.zip (V.fromList [1..]) val))

getMem :: Int -> Int -> MachineState -> ByteArray
getMem baddr len ms = slice baddr len (memory ms)

