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
        memsize,
        gas,
        setPC,
        incPC,
        addPC,
        getOp,
        initialState,
        mstore,
        mstorerange,
        mload,
        mloadrange,
        crange,
        push,
        pop,
        popTwo,
        popThree,
        outOfGas,
        showStack
) where

import Data.LargeWord
import Data.Lens.Common
import Data.Vector as V

import Debug.Trace

import Ethereum.EVM.ExecutionEnvironment
import Ethereum.EVM.InstructionSet
import Ethereum.SimpleTypes

type Memory = ByteArray

-- |The machine state tuple defined in section 9.4.
data MachineState = MS {
        gas :: Gas,
        pc :: Integer,
        memory :: ByteArray,
        memsize :: Word256,
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
mstore addr word = (setMem (fromIntegral $ addr) (toBytes word)) . (expandMem (addr+31))

mstorerange :: Word256 -> ByteArray -> MachineState -> MachineState
mstorerange addr bs = let addr' = fromIntegral $ addr
                      in (setMem addr' bs) . (expandMem (fromIntegral (addr' + blength bs)))

mload :: Word256 -> MachineState -> (MachineState, Word256)
mload addr ms = let ms' = expandMem (addr+31) ms
                in (ms', fromBytes $ getMem (fromIntegral $ addr) 32 ms')

mloadrange :: Word256 -> Word256 -> MachineState -> (MachineState, ByteArray)
mloadrange start len  ms = let ms' = expandMem (fromIntegral (start+len)) ms
                           in (ms', getMem (fromIntegral $ start) (fromIntegral len) ms')

push :: Word256 -> MachineState -> MachineState
push x = stack' ^%= (x:)

pop :: MachineState -> Either RunTimeError (MachineState, Word256)
pop ms@MS{stack=a:s'} = Right (ms{stack=s'}, a)
pop _ = Left StackUnderflow

popTwo :: MachineState -> Either RunTimeError (MachineState, (Word256, Word256))
popTwo ms@MS{stack=a1:a2:s'} = Right (ms{stack=s'}, (a1,a2))
popTwo _ = Left StackUnderflow

popThree :: MachineState-> Either RunTimeError (MachineState, (Word256, Word256, Word256))
popThree ms@MS{stack=a1:a2:a3:s'} = Right (ms{stack=s'}, (a1,a2,a3))
popThree _ = Left StackUnderflow

outOfGas :: MachineState -> Bool
outOfGas ms = (gas ms) < 0

setPC :: (Integral a) => a -> MachineState -> MachineState
setPC x = pc' ^= fromIntegral x

incPC :: MachineState -> MachineState
incPC = pc' ^+= 1

addPC :: (Integral a) => a -> MachineState -> MachineState
addPC n = pc' ^+= (fromIntegral n)

-- Equation 86
getOp :: ExecutionEnvironment -> MachineState -> Maybe Instruction
getOp ee ms =
        if pc ms < (fromIntegral $ clength ee)
           then fromOpcode $ cbyte (fromIntegral $ pc ms) ee
           else Just STOP

-- Internal functions.

memory' :: Lens MachineState Memory
memory' = lens (memory) (\x ms -> ms {memory=x})

memsize' :: Lens MachineState Word256
memsize' = lens (memsize) (\x ms -> ms {memsize=x})

stack' :: Lens MachineState Stack
stack' = lens (stack) (\x ms -> ms {stack=x})

pc' :: Lens MachineState Integer
pc' = lens (pc) (\x ms -> ms {pc=x})

expandMem :: Word256 -> MachineState -> MachineState
expandMem target = (updateMemSize target) . (updateMemVector target)

updateMemSize :: Word256 -> MachineState -> MachineState
updateMemSize target = memsize' ^%= (max (target `ceilDiv` 32))
        where ceilDiv x d = (x + d - 1) `div` d

updateMemVector :: Word256 -> MachineState -> MachineState
updateMemVector target ms@MS{memory=origMem} =
        let origSize = blength origMem
            expandSize = ((2^).ceiling.(logBase 2)) ((fromIntegral target) :: Double)
        in traceShow (origSize, target, expandSize) $ if (origSize < expandSize)
              then ms {memory= origMem V.++ (V.replicate (expandSize - origSize) 0)}
              else ms

setMem :: Int -> ByteArray -> MachineState -> MachineState
setMem baddr val = memory' ^%= (\mem -> update mem (V.zip (V.fromList [baddr..]) val))

getMem :: Int -> Int -> MachineState -> ByteArray
getMem baddr len ms = slice baddr len (memory ms)

showStack :: MachineState -> String
showStack = show.stack
