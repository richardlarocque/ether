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

execute :: ExecutionEnvironment -> Either RunTimeError MemSlice
execute ee = execNext ee (initialState ee)

execNext :: ExecutionEnvironment -> MachineState -> Either RunTimeError MemSlice
execNext ee ms = do
        case getOp ee ms of
                Nothing         -> Left InvalidInstruction
                Just STOP       -> Right emptyMemSlice
                Just SUICIDE    -> Right emptyMemSlice
                Just RETURN     -> do (ms', (start, len)) <- popTwo ms
                                      return $ snd $ (mloadrange start len ms')
                Just w          -> do ms' <- execOp w ee ms
                                      gasCheck ms'
                                      execNext ee ms'

gasCheck :: MachineState -> Either RunTimeError ()
gasCheck ms = case outOfGas ms of
        True -> Left OutOfGas
        False -> Right ()

pc' :: Either RunTimeError MachineState -> Either RunTimeError MachineState
pc' = liftM incPC

execOp :: Instruction -> ExecutionEnvironment -> MachineState -> Either RunTimeError MachineState
execOp w ee = case w of
       {- 0s: Stop and Arithmetic Operations -}
        STOP    -> error "halting operations handled elsewhere"
        ADD     -> pc' . stackBinOp (+)
        MUL     -> pc' . stackBinOp (-)
        SUB     -> pc' . stackBinOp (-)
        DIV     -> pc' . stackBinOp (safeDiv)
        SDIV    -> error "not implemented"
        MOD     -> pc' . stackBinOp (mod)
        SMOD    -> error "not implemented"
        EXP     -> error "not implemented"
        NEG     -> pc' . stackUnOp (*(-1))
        E.LT    -> pc' . stackBinOp (unbool2 (<))
        E.GT    -> pc' . stackBinOp (unbool2 (>))
        SLT     -> error "not implemented"
        SGT     -> error "not implemented"
        E.EQ    -> pc' . stackBinOp (unbool2 (==))
        NOT     -> pc' . stackUnOp ((unbool2 (==)) 0)
        AND     -> pc' . stackBinOp (.&.)
        OR      -> pc' . stackBinOp (.|.)
        XOR     -> pc' . stackBinOp (xor)
        BYTE    -> pc' . stackBinOp (byteIndex)

        {- 20s: SHA3 -}
        -- SHA3    -> execSHA3 ee ms -- FIXME

        {- 30s: Environment -}
        ADDRESS         -> pc' . noArgs ((push.fromAddress) (address ee))
        BALANCE         -> pc' . error "not implemented"
        ORIGIN          -> pc' . noArgs ((push.fromAddress) (origin ee))
        CALLER          -> pc' . noArgs ((push.fromAddress) (caller ee))
        CALLVALUE       -> pc' . noArgs ((push.fromEther) (value ee))
        CALLDATALOAD    -> pc' . withArg (\a -> (push.fromBytes) $ drange (a,a+32) ee)
        CALLDATASIZE    -> pc' . noArgs ((push.fromIntegral) $ dlength ee)
        CALLDATACOPY    -> pc' . withThreeArgs (\maddr daddr len -> mstorerange maddr (drange (daddr*32, (daddr+len)*32) ee))
        CODESIZE        -> pc' . noArgs ((push.fromIntegral) $ clength ee)
        CODECOPY        -> pc' . withThreeArgs (\maddr caddr len -> mstorerange maddr (crange (caddr*32, (caddr+len)*32) ee))
        GASPRICE        -> pc' . noArgs ((push.fromEther) (gasPrice ee))

        {- 40s: Block Information -}
        PREVHASH        -> error "not implemented"
        COINBASE        -> error "not implemented"
        TIMESTAMP       -> error "not implemented"
        NUMBER          -> error "not implemented"
        DIFFICULTY      -> error "not implemented"
        GASLIMIT        -> error "not implemented"

        {- 50s: Stack, Memory, Storage and Flow Operations -}
        -- FIXME: Very ugly...
        POP             -> pc' . ((liftM fst).pop)
        DUP             -> pc' . withArg (\x -> (push x).(push x))
        SWAP            -> pc' . withTwoArgs (\a b -> (push a).(push b))
        MLOAD           -> pc' . withArg (\x ms' -> let (ms'', v) = mload x ms' in push v ms'')
        MSTORE          -> pc' . withTwoArgs mstore
        MSTORE8         -> pc' . withTwoArgs (\a v -> mstore a (byteIndex 31 v))
        SLOAD           -> pc' . error "not implemented"
        SSTORE          -> pc' . error "not implemented"
        JUMP            -> withArg setPC
        JUMPI           -> withTwoArgs (\a c -> if c == 0
                                                   then setPC a
                                                   else incPC)
        PC              -> pc' . noArgs (\ms' -> push ((fromIntegral.pc) ms') ms')
        MSIZE           -> error "not implemented"
        GAS             -> pc' . noArgs (\ms' -> push ((fromIntegral.gas) ms') ms')

        {- 60s and 70s: Push Operations -}
        PUSH1   -> pc' . pushOp  1 ee
        PUSH2   -> pc' . pushOp  2 ee
        PUSH3   -> pc' . pushOp  3 ee
        PUSH4   -> pc' . pushOp  4 ee
        PUSH5   -> pc' . pushOp  5 ee
        PUSH6   -> pc' . pushOp  6 ee
        PUSH7   -> pc' . pushOp  7 ee
        PUSH8   -> pc' . pushOp  8 ee
        PUSH9   -> pc' . pushOp  9 ee
        PUSH10  -> pc' . pushOp 10 ee
        PUSH11  -> pc' . pushOp 11 ee
        PUSH12  -> pc' . pushOp 12 ee
        PUSH13  -> pc' . pushOp 13 ee
        PUSH14  -> pc' . pushOp 14 ee
        PUSH15  -> pc' . pushOp 15 ee
        PUSH16  -> pc' . pushOp 16 ee
        PUSH17  -> pc' . pushOp 17 ee
        PUSH18  -> pc' . pushOp 18 ee
        PUSH19  -> pc' . pushOp 19 ee
        PUSH20  -> pc' . pushOp 20 ee
        PUSH21  -> pc' . pushOp 21 ee
        PUSH22  -> pc' . pushOp 22 ee
        PUSH23  -> pc' . pushOp 23 ee
        PUSH24  -> pc' . pushOp 24 ee
        PUSH25  -> pc' . pushOp 25 ee
        PUSH26  -> pc' . pushOp 26 ee
        PUSH27  -> pc' . pushOp 27 ee
        PUSH28  -> pc' . pushOp 28 ee
        PUSH29  -> pc' . pushOp 29 ee
        PUSH30  -> pc' . pushOp 30 ee
        PUSH31  -> pc' . pushOp 31 ee
        PUSH32  -> pc' . pushOp 32 ee

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
                 in return $ ((addPC l) . (push v)) ms
