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

import Control.Monad
import Data.Binary
import Data.Bits
import Data.Digest.Pure.SHA
import Data.LargeWord
import Data.Maybe
import qualified Data.ByteString.Lazy as B

import Ethereum.EVM.InstructionSet as E
import Ethereum.EVM.MachineState
import Ethereum.EVM.ExecutionEnvironment
import Ethereum.SimpleTypes

data SystemState = SystemState

execNext :: ExecutionEnvironment -> MachineState -> Either RunTimeError MemSlice
execNext ee ms = do
        case getOp ee ms of
                Nothing   -> Left InvalidInstruction
                Just STOP -> Right []
                Just w    -> do ms' <- execOp w ee ms
                                gasCheck ms'
                                execNext ee ms'

gasCheck :: MachineState -> Either RunTimeError ()
gasCheck ms = case outOfGas ms of
        True -> Left OutOfGas
        False -> Right ()

execOp :: Instruction -> ExecutionEnvironment -> MachineState -> Either RunTimeError MachineState
execOp w ee ms =
        case w of
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
                ADDRESS         -> noArgs ((push.fromAddress) (address ee)) ms
                BALANCE         -> error "not implemented"
                ORIGIN          -> noArgs ((push.fromAddress) (origin ee)) ms
                CALLER          -> noArgs ((push.fromAddress) (caller ee)) ms
                CALLVALUE       -> noArgs ((push.fromEther) (value ee)) ms
                CALLDATALOAD    -> withArg (\a -> (push.fromBytes) $ drange (a,a+32) ee) ms
                CALLDATASIZE    -> noArgs ((push.fromIntegral) $ dlength ee) ms
                -- CALLDATACOPY    -> withThreeArgs (\maddr daddr len -> 
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
                JUMPI           -> withTwoArgs (\a c -> if c == 0
                                                           then setPC a
                                                           else incPC) ms
                PC              -> noArgs (\ms' -> push ((fromIntegral.pc) ms') ms') ms
                MSIZE           -> error "not implemented"
                GAS             -> noArgs (\ms' -> push ((fromIntegral.gas) ms') ms') ms

                {- 60s and 70s: Push Operations -}
                PUSH1   -> pushOp  1 ee ms
                PUSH2   -> pushOp  2 ee ms
                PUSH3   -> pushOp  3 ee ms
                PUSH4   -> pushOp  4 ee ms
                PUSH5   -> pushOp  5 ee ms
                PUSH6   -> pushOp  6 ee ms
                PUSH7   -> pushOp  7 ee ms
                PUSH8   -> pushOp  8 ee ms
                PUSH9   -> pushOp  9 ee ms
                PUSH10  -> pushOp 10 ee ms
                PUSH11  -> pushOp 11 ee ms
                PUSH12  -> pushOp 12 ee ms
                PUSH13  -> pushOp 13 ee ms
                PUSH14  -> pushOp 14 ee ms
                PUSH15  -> pushOp 15 ee ms
                PUSH16  -> pushOp 16 ee ms
                PUSH17  -> pushOp 17 ee ms
                PUSH18  -> pushOp 18 ee ms
                PUSH19  -> pushOp 19 ee ms
                PUSH20  -> pushOp 20 ee ms
                PUSH21  -> pushOp 21 ee ms
                PUSH22  -> pushOp 22 ee ms
                PUSH23  -> pushOp 23 ee ms
                PUSH24  -> pushOp 24 ee ms
                PUSH25  -> pushOp 25 ee ms
                PUSH26  -> pushOp 26 ee ms
                PUSH27  -> pushOp 27 ee ms
                PUSH28  -> pushOp 28 ee ms
                PUSH29  -> pushOp 29 ee ms
                PUSH30  -> pushOp 30 ee ms
                PUSH31  -> pushOp 31 ee ms
                PUSH32  -> pushOp 32 ee ms

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

noArgs :: (MachineState -> MachineState) -> (MachineState -> Either RunTimeError MachineState)
noArgs f = return.f

withArg :: (Word256 -> MachineState -> MachineState) ->
        (MachineState -> Either RunTimeError MachineState)
withArg f ms = do (ms', arg) <- pop ms
                  return $ f arg ms'

withTwoArgs :: (Word256 -> Word256 -> MachineState -> MachineState) ->
        (MachineState -> Either RunTimeError MachineState)
withTwoArgs f ms = do (ms', (arg1, arg2)) <- popTwo ms
                      return $ f arg1 arg2 ms'

withThreeArgs :: (Word256 -> Word256 -> Word256 -> MachineState -> MachineState) ->
        (MachineState -> Either RunTimeError MachineState)
withThreeArgs f ms = do (ms', (arg1, arg2, arg3)) <- popThree ms
                        return $ f arg1 arg2 arg3 ms'

stackBinOp :: (Word256 -> Word256 -> Word256) ->
        MachineState -> Either RunTimeError MachineState
stackBinOp f ms = withTwoArgs (\a b -> push (f a b)) ms 

stackUnOp :: (Word256 -> Word256) -> MachineState -> Either RunTimeError MachineState
stackUnOp f ms = withArg (\a -> push (f a)) ms

pushOp :: Word256 -> ExecutionEnvironment -> MachineState -> Either RunTimeError MachineState
pushOp l _ _ | l > 32 = error "Invalid push length argument"
pushOp l ee ms = let s = fromIntegral $ (pc ms) + 1
                     v = fromBytes $ crange (s, s+l) ee
                 in return $ push v ms

