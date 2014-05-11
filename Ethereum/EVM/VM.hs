{- |
Module      :  Ethereum.EVM.VM
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
import Ethereum.EVM.MachineState
import Ethereum.EVM.ExecutionEnvironment
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

data SystemState = SystemState

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
nextOp (EE {code=cs}) ms =
        if (pc ms) < (snd.bounds) cs
           then fromOpcode $ cs ! (pc ms)
           else Just STOP

unsafeNextOp :: ExecutionEnvironment -> MachineState -> Instruction
unsafeNextOp ee ms = fromJust $ nextOp ee ms


{-
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
-}

execNext :: ExecutionEnvironment -> MachineState -> Either RunTimeError MemSlice
execNext ee ms = do
        case nextOp ee ms of
                Nothing   -> Left InvalidInstruction
                Just STOP -> Right []
                Just w    -> do ms' <- execOp ee ms w
                                gasCheck ms'
                                execNext ee ms'

gasCheck :: MachineState -> Either RunTimeError ()
gasCheck ms = case outOfGas ms of
        True -> Left OutOfGas
        False -> Right ()

execOp :: ExecutionEnvironment -> MachineState -> Instruction -> Either RunTimeError MachineState
execOp ee ms w =
        case (unsafeNextOp ee ms) of
                {- 0s: Stop and Arithmetic Operations -}
                STOP    -> error "halting operations handled elsewhere"
                ADD     -> stackBinOp (+) ms
                MUL     -> stackBinOp (-) ms
                SUB     -> stackBinOp (-) ms
                DIV     -> stackBinOp (safeDiv) ms
                SDIV    -> error "not implemented"
                MOD     -> stackBinOp (mod) ms
                SMOD    -> error "not implemented"
                EXP     -> error "not implemented"
                NEG     -> stackUnOp (*(-1)) ms
                E.LT    -> stackBinOp (unbool2 (<)) ms
                E.GT    -> stackBinOp (unbool2 (>)) ms
                SLT     -> error "not implemented"
                SGT     -> error "not implemented"
                E.EQ    -> stackBinOp (unbool2 (==)) ms
                NOT     -> stackUnOp ((unbool2 (==)) 0) ms
                AND     -> stackBinOp (.&.) ms
                OR      -> stackBinOp (.|.) ms
                XOR     -> stackBinOp (xor) ms
                BYTE    -> stackBinOp (byteIndex) ms

                {- 20s: SHA3 -}
                -- SHA3    -> execSHA3 ee ms -- FIXME

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
                -- FIXME: Very ugly...
                POP             -> ((liftM fst).pop) ms
                DUP             -> withArg (\x -> (push x).(push x)) ms
                SWAP            -> withTwoArgs (\a b -> (push a).(push b)) ms
                MLOAD           -> withArg (\x ms' -> let (ms'', v) = mload x ms' in push v ms'') ms
                MSTORE          -> withTwoArgs mstore ms
                MSTORE8         -> withTwoArgs (\a v -> mstore a (byteIndex 31 v)) ms
                SLOAD           -> error "not implemented"
                SSTORE          -> error "not implemented"
                JUMP            -> withArg setPC ms
                JUMPI           -> error "not implemented"
                PC              -> return $ (\ms' -> push ((fromIntegral.pc) ms') ms') ms
                MSIZE           -> error "not implemented"
                GAS             -> return $ (\ms' -> push ((fromIntegral.gas) ms') ms') ms

                {- 60s and 70s: Push Operations -}
                -- PUSH1   -> execPushOp ee ms  1
                -- PUSH2   -> execPushOp ee ms  2
                -- PUSH3   -> execPushOp ee ms  3
                -- PUSH4   -> execPushOp ee ms  4
                -- PUSH5   -> execPushOp ee ms  5
                -- PUSH6   -> execPushOp ee ms  6
                -- PUSH7   -> execPushOp ee ms  7
                -- PUSH8   -> execPushOp ee ms  8
                -- PUSH9   -> execPushOp ee ms  9
                -- PUSH10  -> execPushOp ee ms 10
                -- PUSH11  -> execPushOp ee ms 11
                -- PUSH12  -> execPushOp ee ms 12
                -- PUSH13  -> execPushOp ee ms 13
                -- PUSH14  -> execPushOp ee ms 14
                -- PUSH15  -> execPushOp ee ms 15
                -- PUSH16  -> execPushOp ee ms 16
                -- PUSH17  -> execPushOp ee ms 17
                -- PUSH18  -> execPushOp ee ms 18
                -- PUSH19  -> execPushOp ee ms 19
                -- PUSH20  -> execPushOp ee ms 20
                -- PUSH21  -> execPushOp ee ms 21
                -- PUSH22  -> execPushOp ee ms 22
                -- PUSH23  -> execPushOp ee ms 23
                -- PUSH24  -> execPushOp ee ms 24
                -- PUSH25  -> execPushOp ee ms 25
                -- PUSH26  -> execPushOp ee ms 26
                -- PUSH27  -> execPushOp ee ms 27
                -- PUSH28  -> execPushOp ee ms 28
                -- PUSH29  -> execPushOp ee ms 29
                -- PUSH30  -> execPushOp ee ms 30
                -- PUSH31  -> execPushOp ee ms 31
                -- PUSH32  -> execPushOp ee ms 32

                {- f0s: System operations -}
                CREATE  -> error "not implemented"
                CALL    -> error "not implemented"
                RETURN  -> error "halting operations are handled elsewhere"
                SUICIDE -> error "halting operations are handled elsewhere"

safeDiv a 0 = 0
safeDiv a b = a `div` b

unbool2 f = (\a b -> if (f a b) then 0 else 1)

byteIndex i w = if i < 32
                   then extractByte w i
                   else 0

extractByte :: (Integral b) => Word256 -> Word256 -> b
extractByte b i = fromIntegral $ (encode b) `B.index` (fromIntegral i)

{-
withTwoArgs :: (Word256 -> Word256 -> MachineState -> MachineState) -> (MachineState -> MachineState)

withTwoArgsAndPush :: (Word256 -> Word256 -> Word256) -> (MachineState -> MachineState)

push :: Word256 -> MachineState -> MachineState

f :: Word256 -> Word256 -> Word256
-}

{-a
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
-}

withArg :: (Word256 -> MachineState -> MachineState) ->
        (MachineState -> Either RunTimeError MachineState)
withArg f ms = do (ms', arg) <- pop ms
                  return $ f arg ms'

withTwoArgs :: (Word256 -> Word256 -> MachineState -> MachineState) ->
        (MachineState -> Either RunTimeError MachineState)
withTwoArgs f ms = do (ms', (arg1, arg2)) <- popTwo ms
                      return $ f arg1 arg2 ms'

stackBinOp :: (Word256 -> Word256 -> Word256) ->
        MachineState -> Either RunTimeError MachineState
stackBinOp f ms = withTwoArgs (\a b -> push (f a b)) ms 

stackUnOp :: (Word256 -> Word256) -> MachineState -> Either RunTimeError MachineState
stackUnOp f ms = withArg (\a -> push (f a)) ms


--execStackUnaryOp ee ms f = withTwoArgs (\a ms -> push (f a) ms)

--execStackBinaryBoolOp ee ms f = withTwoArgs (\a b ms -> push ((asBool.f) a b) ms)
        --where asBool x = if x == 0 then 0 else 1

--execMSTORE _ ms = withTwoArgs mstore ms

{-
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
-}
