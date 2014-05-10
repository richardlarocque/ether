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

import Ethereum.EVM.InstructionSet as E
import Ethereum.SimpleTypes

import qualified Ethereum.EVM.FeeSchedule as Fee -- FIXME: Should not need this long run.

import Control.Monad
import Data.Array
import Data.Binary
import Data.Bits
import Data.Digest.Pure.SHA
import Data.LargeWord
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M

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
        map (memWord ms) [startAddr..endAddr-1]

memWord :: MachineState -> Word256 -> Word256
memWord ms addr = fromMaybe 0 $ M.lookup addr (memory ms)

setWord :: Word256 -> Word256 -> Memory -> Memory
setWord = M.insert

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
        map (codeByte c) [startAddr..endAddr-1]

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
        let w = unsafeNextOp ee ms
        in case w of
                STOP -> Fee.stop
                _ -> Fee.step

-- Equation 86
nextOp :: ExecutionEnvironment -> MachineState -> Maybe Instruction
nextOp (EE {code=cs}) (MS {pc=counter}) =
        if counter < (snd.bounds) cs
           then fromOpcode $ cs ! counter
           else Just STOP

unsafeNextOp :: ExecutionEnvironment -> MachineState -> Instruction
unsafeNextOp ee ms = fromJust $ nextOp ee ms

data RunTimeError = OutOfGas
                  | InvalidInstruction
                  | StackUnderflow
                  deriving Eq

-- |Checks for exceptional halt as defined in section 9.4.1.
checkException :: ExecutionEnvironment -> MachineState -> Maybe RunTimeError
checkException ee ms =
        let w = nextOp ee ms
        in msum $ [
                if isNothing w
                   then Just InvalidInstruction
                   else Nothing,
                if gas ms < getNextCost ee ms
                   then Just OutOfGas
                   else Nothing,
                if isStackUnderflow (stackPopCount (fromJust w)) (stack ms)
                   then Just StackUnderflow
                   else Nothing
                ]
        where isStackUnderflow n s = case n of
                0 -> False
                x -> null $ drop (n-1) s

-- |Checks for normal halt as defined in section 9.4.2.
checkHalt :: ExecutionEnvironment -> MachineState -> Maybe MemSlice
checkHalt ee ms =
  let w = unsafeNextOp ee ms
  in case w of
          RETURN -> let startAddr:endAddr:_ = stack ms
                    in Just $ memRange ms (startAddr, endAddr)
          STOP -> Just []
          SUICIDE -> Just []
          _ -> Nothing

execOp :: ExecutionEnvironment -> MachineState -> MachineState
execOp ee ms =
        case (unsafeNextOp ee ms) of
                {- 0s: Stop and Arithmetic Operations -}
                STOP    -> error "halting operations handled elsewhere"
                ADD     -> execStackBinaryOp ee ms (+)
                MUL     -> execStackBinaryOp ee ms (*)
                SUB     -> execStackBinaryOp ee ms (-)
                DIV     -> execStackBinaryOp ee ms (safeDiv)
                SDIV    -> error "not implemented"
                MOD     -> execStackBinaryOp ee ms (mod)
                SMOD    -> error "not implemented"
                EXP     -> error "not implemented"
                NEG     -> execStackUnaryOp ee ms (*(-1))
                E.LT    -> execStackBinaryBoolOp ee ms (<)
                E.GT    -> execStackBinaryBoolOp ee ms (>)
                SLT     -> error "not implemented"
                SGT     -> error "not implemented"
                E.EQ    -> execStackBinaryBoolOp ee ms (==)
                NOT     -> execStackUnaryBoolOp ee ms (0 ==)
                AND     -> execStackBinaryOp ee ms (.&.)
                OR      -> execStackBinaryOp ee ms (.|.)
                XOR     -> execStackBinaryOp ee ms (xor)
                BYTE    -> execBYTE ee ms

                {- 20s: SHA3 -}
                SHA3    -> execSHA3 ee ms

                {- 30s: Environment -}
                ADDRESS         -> error "not implemented"
                BALANCE         -> error "not implemented"
                ORIGIN          -> error "not implemented"
                CALLER          -> error "not implemented"
                CALLVALUE       -> error "not implemented"
                CALLDATALOAD    -> error "not implemented"
                CALLDATASIZE    -> error "not implemented"
                CALLDATACOPY    -> error "not implemented"
                CODESIZE        -> error "not implemented"
                CODECOPY        -> error "not implemented"
                GASPRICE        -> error "not implemented"

                {- 40s: Block Information -}
                PREVHASH        -> error "not implemented"
                COINBASE        -> error "not implemented"
                TIMESTAMP       -> error "not implemented"
                NUMBER          -> error "not implemented"
                DIFFICULTY      -> error "not implemented"
                GASLIMIT        -> error "not implemented"

                {- 50s: Stack, Memory, Storage and Flow Operations -}
                POP             -> execStackOp ee ms tail
                DUP             -> execStackOp ee ms (\(x:xs) -> x:x:xs)
                SWAP            -> execStackOp ee ms (\(a:b:xs) -> b:a:xs)
                MLOAD           -> execMLOAD ee ms
                MSTORE          -> execMSTORE ee ms
                MSTORE8         -> execMSTORE8 ee ms
                SLOAD           -> error "not implemented"
                SSTORE          -> error "not implemented"
                JUMP            -> execJUMP ee ms
                JUMPI           -> error "not implemented"
                PC              -> execPC ee ms
                MSIZE           -> error "not implemented"
                GAS             -> execGAS ee ms

                {- 60s and 70s: Push Operations -}
                PUSH1   -> execPushOp ee ms  1
                PUSH2   -> execPushOp ee ms  2
                PUSH3   -> execPushOp ee ms  3
                PUSH4   -> execPushOp ee ms  4
                PUSH5   -> execPushOp ee ms  5
                PUSH6   -> execPushOp ee ms  6
                PUSH7   -> execPushOp ee ms  7
                PUSH8   -> execPushOp ee ms  8
                PUSH9   -> execPushOp ee ms  9
                PUSH10  -> execPushOp ee ms 10
                PUSH11  -> execPushOp ee ms 11
                PUSH12  -> execPushOp ee ms 12
                PUSH13  -> execPushOp ee ms 13
                PUSH14  -> execPushOp ee ms 14
                PUSH15  -> execPushOp ee ms 15
                PUSH16  -> execPushOp ee ms 16
                PUSH17  -> execPushOp ee ms 17
                PUSH18  -> execPushOp ee ms 18
                PUSH19  -> execPushOp ee ms 19
                PUSH20  -> execPushOp ee ms 20
                PUSH21  -> execPushOp ee ms 21
                PUSH22  -> execPushOp ee ms 22
                PUSH23  -> execPushOp ee ms 23
                PUSH24  -> execPushOp ee ms 24
                PUSH25  -> execPushOp ee ms 25
                PUSH26  -> execPushOp ee ms 26
                PUSH27  -> execPushOp ee ms 27
                PUSH28  -> execPushOp ee ms 28
                PUSH29  -> execPushOp ee ms 29
                PUSH30  -> execPushOp ee ms 30
                PUSH31  -> execPushOp ee ms 31
                PUSH32  -> execPushOp ee ms 32

                {- f0s: System operations -}
                CREATE  -> error "not implemented"
                CALL    -> error "not implemented"
                RETURN  -> error "halting operations are handled elsewhere"
                SUICIDE -> error "halting operations are handled elsewhere"

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

execStackUnaryBoolOp :: ExecutionEnvironment-> MachineState ->
        (Word256 -> Bool) -> MachineState
execStackUnaryBoolOp ee ms f =
        execStackOp ee ms (\(x:xs) -> (if f x then 1 else 0):xs)

execStackBinaryBoolOp :: ExecutionEnvironment-> MachineState ->
        (Word256 -> Word256 -> Bool) -> MachineState
execStackBinaryBoolOp ee ms f =
        execStackOp ee ms (\(a:b:xs) -> (if f a b then 1 else 0):xs)

execBYTE ee ms = execStackBinaryOp ee ms byteOp where
        byteOp :: Word256 -> Word256 -> Word256
        byteOp i w =
                if i < 32
                   then extractByte w i
                   else 0

-- extracts byte starting from most-significant in big-endian
extractByte ::  (Integral a, Integral b) => Word256 -> a -> b
extractByte b i = fromIntegral $ (encode b) `B.index` (fromIntegral i)

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
                packBytes bs = foldl (\a x -> (a * 256) + (fromIntegral x)) (0 :: Word256) (bs :: [Word8])

execMLOAD ee ms =
        let (addr:stack') = stack ms
        in MS {
                gas = (gas ms) - getNextCost ee ms,
                pc = (pc ms) + 1,
                memory = memory ms,
                stack = (memWord ms addr):stack'
        }

execMSTORE ee ms =
        let (addr:value:stack') = stack ms
        in MS {
                gas = (gas ms) - getNextCost ee ms,
                pc = (pc ms) + 1,
                memory = setWord addr value (memory ms),
                stack = stack'
        }

execMSTORE8 ee ms =
        let (addr:value:stack') = stack ms
        in MS {
                gas = (gas ms) - getNextCost ee ms,
                pc = (pc ms) + 1,
                memory = setWord addr (extractByte value 31) (memory ms),
                stack = stack'
        }

execJUMP ee ms =
        let (dest:stack') = stack ms
        in MS {
                gas = (gas ms) - getNextCost ee ms,
                pc = fromIntegral dest,
                memory = memory ms,
                stack = stack'
        }

execPC ee ms =
        MS {
                gas = (gas ms) - getNextCost ee ms,
                pc = (pc ms) + 1,
                memory = memory ms,
                stack = (fromIntegral $ pc ms):(stack ms)
        }

execGAS ee ms =
        MS {
                gas = (gas ms) - getNextCost ee ms,
                pc = (pc ms) + 1,
                memory = memory ms,
                stack = (fromIntegral $ gas ms):(stack ms)
        }

execSHA3 ee ms =
        let (s:e:stack') = stack ms
            hashResult = (decode $ encode $ sha256 $ B.pack $ decode $ encode $ memRange ms (s,e))
        in MS {
                gas = (gas ms) - getNextCost ee ms,
                pc = (pc ms) + 1,
                memory = memory ms,
                stack = hashResult:stack'
        }

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
