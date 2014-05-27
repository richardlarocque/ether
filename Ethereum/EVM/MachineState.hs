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
        MachineState(..),
        initWithGas,
        initMem,
        incPC,
        getOp,
        mstorerange,
        mloadrange,
        crange,
        outOfGas,
        showStack
) where

import Data.LargeWord
import Data.Lens.Common
import Data.ByteString as B
import Ethereum.EVM.ExecutionEnvironment
import Ethereum.EVM.InstructionSet
import Ethereum.SimpleTypes

-- |The machine state tuple defined in section 9.4.
data MachineState = MS {
        gas :: Gas,
        pc :: Integer,
        memory :: B.ByteString,
        memsize :: Word256,
        stack :: Stack
}

initWithGas :: Integer -> MachineState
initWithGas g = MS g 0 initMem 0 []

initMem :: B.ByteString
initMem = B.replicate 512 0

mstorerange :: Word256 -> B.ByteString -> MachineState -> MachineState
mstorerange addr bs = let addr' = fromIntegral addr
                      in setMem addr' bs . expandMem (fromIntegral (addr' + blength bs))

mloadrange :: Word256 -> Word256 -> MachineState -> (MachineState, B.ByteString)
mloadrange start len  ms = let ms' = expandMem (fromIntegral (start+len)) ms
                           in (ms', getMem (fromIntegral start) (fromIntegral len) ms')


outOfGas :: MachineState -> Bool
outOfGas ms = gas ms < 0

incPC :: MachineState -> MachineState
incPC = pc' ^+= 1

-- Equation 86
getOp :: ExecutionEnvironment -> MachineState -> Maybe Instruction
getOp ee ms =
        if pc ms < fromIntegral (clength ee)
           then fromOpcode $ cbyte (fromIntegral $ pc ms) ee
           else Just STOP

-- Internal functions.

memory' :: Lens MachineState B.ByteString
memory' = lens memory (\x ms -> ms {memory=x})

memsize' :: Lens MachineState Word256
memsize' = lens memsize (\x ms -> ms {memsize=x})

pc' :: Lens MachineState Integer
pc' = lens pc (\x ms -> ms {pc=x})

expandMem :: Word256 -> MachineState -> MachineState
expandMem target = updateMemSize target . updateMemVector target

updateMemSize :: Word256 -> MachineState -> MachineState
updateMemSize target = memsize' ^%= max (target `ceilDiv` 32)
        where ceilDiv x d = (x + d - 1) `div` d

updateMemVector :: Word256 -> MachineState -> MachineState
updateMemVector target ms@MS{memory=origMem} =
        let origSize = blength origMem
            expandSize = (((2^) :: Int -> Int).ceiling.logBase 2) (fromIntegral target :: Double)
        in if origSize < expandSize
              then ms {memory= origMem `B.append` B.replicate (expandSize - origSize) 0}
              else ms

setMem :: Int -> B.ByteString -> MachineState -> MachineState
setMem baddr val = memory' ^%= (\mem ->
        let (pre, post) = B.splitAt baddr mem
        in pre `B.append` val `B.append` B.drop (B.length val) post)

getMem :: Int -> Int -> MachineState -> B.ByteString
getMem baddr len ms = brange (baddr, len) (memory ms)

showStack :: MachineState -> String
showStack = show.stack
