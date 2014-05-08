{- |
Module      :  Ethereum.EVM.FeeSchedule
Description :  Implementation of the Ethereum Virtual Machine
Copyright   :  (c) Richard Larocque
License     :  GPL-3.0+

Maintainer  :  richard.larocque@gmail.com
Stability   :  unstable
Portability :  non-portable (Unknown portability)

See Ethereum Yellow Paper, Proof-of-Concept V, Section 9
-}

module Ethereum.EVM.VM where

import Ethereum.EVM.InstructionSet
import Ethereum.SimpleTypes

import qualified Ethereum.EVM.FeeSchedule as Fee -- FIXME: Should not need this long run.

import Data.Word
import Data.LargeWord
import Data.Array
import Data.Maybe
import qualified Data.Map as M
import Control.Monad

data SystemState = SystemState

-- |The machine state tuple defined in section 9.4.
data MachineState = MS {
        gas :: Gas,
        pc :: Integer,
        memory :: Memory,
        stack :: Stack
}

memRange :: MachineState -> (Word256, Word256) -> MemSlice
memRange ms (startAddr, endAddr) =
        map (memWord ms) [startAddr..endAddr]

memWord :: MachineState -> Word256 -> Word256
memWord ms addr = fromMaybe 0 $ M.lookup addr (memory ms)

-- |The execution environment tuple defined in section 9.3.
data ExecutionEnvironment = EE {
        owner :: Address,
        sender :: Address,
        gasPrice :: Ether,
        input :: ByteArray,
        execCause :: Address,
        value :: Ether,
        code :: Code
}

inBounds :: Ix i => i -> Array i e -> Bool
inBounds index a = let (left,right) = bounds a
                   in left <= index && index < right

codeRange :: Code -> (Integer, Integer) -> [Word8]
codeRange c (startAddr, endAddr) =
        map (codeByte c) [startAddr..endAddr]

codeByte :: Code -> Integer -> Word8
codeByte c addr = if inBounds addr c
                      then c ! addr
                      else 0  -- FIXME: Is this right for PUSH?
                              -- It's definitely not right for CODECOPY...

getNextCost :: ExecutionEnvironment -> MachineState -> Gas
getNextCost ee ms =
        (getMemoryCost ee ms) + (getInstructionCost ee ms)

getMemoryCost ee ms = 0

getInstructionCost ee ms =
        let instr = nextOp ee ms
        in case instr of
                STOP -> Fee.stop
                _ -> Fee.step

-- Equation 86
nextOp :: ExecutionEnvironment -> MachineState -> Instruction
nextOp (EE {code=cs}) (MS {pc=counter}) =
        if counter < (snd.bounds) cs
           then fromOpcode $ cs ! counter
           else STOP

data RunTimeError = OutOfGas
                  | InvalidInstruction
                  | StackUnderflow
                  deriving Eq

-- |Checks for exceptional halt as defined in section 9.4.1.
checkException :: ExecutionEnvironment -> MachineState -> Maybe RunTimeError
checkException ee ms =
        let w = nextOp ee ms
        in msum $ [
                if gas ms < getNextCost ee ms
                   then Just OutOfGas
                   else Nothing,
                if INVALID == w
                   then Just InvalidInstruction
                   else Nothing,
                if isStackUnderflow (stackPopCount w) (stack ms)
                   then Just StackUnderflow
                   else Nothing
                ]
        where isStackUnderflow n s = case n of
                0 -> False
                x -> not.null $ drop (n-1) s

-- |Checks for normal halt as defined in section 9.4.2.
checkHalt :: ExecutionEnvironment -> MachineState -> Maybe MemSlice
checkHalt ee ms =
  let instr = nextOp ee ms in
  case instr of
    RETURN -> let startAddr:endAddr:_ = stack ms in
              Just $ memRange ms (startAddr, endAddr)
    STOP -> Just []
    SUICIDE -> Just []
    _ -> Nothing

execOp :: ExecutionEnvironment -> MachineState -> MachineState
execOp ee ms =
        case (nextOp ee ms) of
                ADD -> execStackBinaryOp ee ms (+)
                MUL -> execStackBinaryOp ee ms (*)
                SUB -> execStackBinaryOp ee ms (-)
                DIV -> execStackBinaryOp ee ms (safeDiv)
                MOD -> execStackBinaryOp ee ms (mod)
                --EXP -> execSimpleBinOp ee ms (**)
                NEG -> execStackUnaryOp ee ms (*(-1))
                --
                POP -> execStackOp ee ms tail
                DUP -> execStackOp ee ms (\(x:xs) -> x:x:xs)
                SWAP -> execStackOp ee ms (\(a:b:xs) -> b:a:xs)
                --
                PUSH1 ->  execPushOp ee ms  1
                PUSH2 ->  execPushOp ee ms  2
                PUSH3 ->  execPushOp ee ms  3
                PUSH4 ->  execPushOp ee ms  4
                PUSH5 ->  execPushOp ee ms  5
                PUSH6 ->  execPushOp ee ms  6
                PUSH7 ->  execPushOp ee ms  7
                PUSH8 ->  execPushOp ee ms  8
                PUSH9 ->  execPushOp ee ms  9
                PUSH10 -> execPushOp ee ms 10
                PUSH11 -> execPushOp ee ms 11
                PUSH12 -> execPushOp ee ms 12
                PUSH13 -> execPushOp ee ms 13
                PUSH14 -> execPushOp ee ms 14
                PUSH15 -> execPushOp ee ms 15
                PUSH16 -> execPushOp ee ms 16
                PUSH17 -> execPushOp ee ms 17
                PUSH18 -> execPushOp ee ms 18
                PUSH19 -> execPushOp ee ms 19
                PUSH20 -> execPushOp ee ms 20
                PUSH21 -> execPushOp ee ms 21
                PUSH22 -> execPushOp ee ms 22
                PUSH23 -> execPushOp ee ms 23
                PUSH24 -> execPushOp ee ms 24
                PUSH25 -> execPushOp ee ms 25
                PUSH26 -> execPushOp ee ms 26
                PUSH27 -> execPushOp ee ms 27
                PUSH28 -> execPushOp ee ms 28
                PUSH29 -> execPushOp ee ms 29
                PUSH30 -> execPushOp ee ms 30
                PUSH31 -> execPushOp ee ms 31
                PUSH32 -> execPushOp ee ms 32

safeDiv a 0 = 0
safeDiv a b = a `div` b

execStackOp :: ExecutionEnvironment -> MachineState ->
        (Stack -> Stack) -> MachineState
execStackOp ee ms f = MS {
        gas = (gas ms) - getNextCost ee ms,
        pc = (pc ms) + 1,
        memory = memory ms,
        stack = f (stack ms)
}

execStackUnaryOp :: ExecutionEnvironment -> MachineState ->
        (Word256 -> Word256) -> MachineState
execStackUnaryOp ee ms f = execStackOp ee ms (\(x:xs) -> (f x):xs)

execStackBinaryOp :: ExecutionEnvironment-> MachineState ->
        (Word256 -> Word256 -> Word256) -> MachineState
execStackBinaryOp ee ms f = execStackOp ee ms (\(a:b:xs) -> (f a b):xs)

execPushOp ee ms n = MS {
        gas = (gas ms) - getNextCost ee ms,
        pc = (pc ms) + 1 + n,
        memory = memory ms,
        stack = (wordToPush (code ee) (pc ms)):(stack ms)
}
        where
                wordToPush :: Code -> Integer -> Word256
                wordToPush code pc = packBytes $ codeRange code (pc+1, pc+1+n)
                packBytes :: [Word8] -> Word256
                packBytes bs = foldr (\x a -> (a * 256) + (fromIntegral x)) (0 :: Word256) (bs :: [Word8])

initialMachineState :: ExecutionEnvironment -> MachineState
initialMachineState ee = MS {
        gas = (value ee) `div` (gasPrice ee),
        pc = 0,
        memory = M.empty,
        stack = []
}

runVM :: ExecutionEnvironment -> Either RunTimeError MemSlice
runVM ee = runVM' ee (initialMachineState ee)

runVM' :: ExecutionEnvironment -> MachineState -> Either RunTimeError MemSlice
runVM' ee ms = do
        case checkException ee ms of
                Just error -> Left error
                Nothing -> Right ()
        case checkHalt ee ms of
                Just result -> Right result
                Nothing -> runVM' ee (execOp ee ms)
