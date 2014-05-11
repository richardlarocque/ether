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
                                      ms'' <- updatePC w ms'
                                      gasCheck ms''
                                      execNext ee ms''

gasCheck :: MachineState -> Either RunTimeError ()
gasCheck ms = case outOfGas ms of
        True -> Left OutOfGas
        False -> Right ()

updatePC :: Instruction -> MachineState -> Either RunTimeError MachineState
updatePC w = case w of
        JUMP    -> withArg setPC
        JUMPI   -> withTwoArgs (\a c -> if c == 0
                                           then setPC a
                                           else incPC)
        -- PUSHes also affect PC, but they still need a +1 here.
        _       -> return.incPC

execOp :: Instruction -> ExecutionEnvironment -> MachineState -> Either RunTimeError MachineState
execOp w ee = case w of
       {- 0s: Stop and Arithmetic Operations -}
        STOP    -> error "halting operations handled elsewhere"
        ADD     -> stackBinOp (+)
        MUL     -> stackBinOp (-)
        SUB     -> stackBinOp (-)
        DIV     -> stackBinOp (safeDiv)
        SDIV    -> error "not implemented"
        MOD     -> stackBinOp (mod)
        SMOD    -> error "not implemented"
        EXP     -> error "not implemented"
        NEG     -> stackUnOp (*(-1))
        E.LT    -> stackBinOp (unbool2 (<))
        E.GT    -> stackBinOp (unbool2 (>))
        SLT     -> error "not implemented"
        SGT     -> error "not implemented"
        E.EQ    -> stackBinOp (unbool2 (==))
        NOT     -> stackUnOp ((unbool2 (==)) 0)
        AND     -> stackBinOp (.&.)
        OR      -> stackBinOp (.|.)
        XOR     -> stackBinOp (xor)
        BYTE    -> stackBinOp (byteIndex)

        {- 20s: SHA3 -}
        SHA3    -> withTwoArgs $ \a len ms ->
                let (ms', bytes) = mloadrange a len ms
                    hashed = integerDigest $ sha256 $ memToByteString bytes
                in (push.fromIntegral) hashed ms'

        {- 30s: Environment -}
        ADDRESS         -> noArgs ((push.fromAddress) (address ee))
        BALANCE         -> error "not implemented"
        ORIGIN          -> noArgs ((push.fromAddress) (origin ee))
        CALLER          -> noArgs ((push.fromAddress) (caller ee))
        CALLVALUE       -> noArgs ((push.fromEther) (value ee))
        CALLDATALOAD    -> withArg (\a -> (push.fromBytes) $ drange (a,a+32) ee)
        CALLDATASIZE    -> noArgs ((push.fromIntegral) $ dlength ee)
        CALLDATACOPY    -> withThreeArgs $
                \maddr daddr len -> let bytes = drange (daddr, daddr+len) ee
                                    in mstorerange maddr bytes
        CODESIZE        -> noArgs ((push.fromIntegral) $ clength ee)
        CODECOPY        -> withThreeArgs $
                \maddr caddr len -> let bytes = crange (caddr, caddr+len) ee
                                    in mstorerange maddr bytes

        GASPRICE        -> noArgs ((push.fromEther) (gasPrice ee))

        {- 40s: Block Information -}
        PREVHASH        -> error "not implemented"
        COINBASE        -> error "not implemented"
        TIMESTAMP       -> error "not implemented"
        NUMBER          -> error "not implemented"
        DIFFICULTY      -> error "not implemented"
        GASLIMIT        -> error "not implemented"

        {- 50s: Stack, Memory, Storage and Flow Operations -}
        -- FIXME: Very ugly...
        POP             -> ((liftM fst).pop)
        DUP             -> withArg (\x -> (push x).(push x))
        SWAP            -> withTwoArgs (\a b -> (push a).(push b))
        MLOAD           -> withArg (\x ms' -> let (ms'', v) = mload x ms' in push v ms'')
        MSTORE          -> withTwoArgs mstore
        MSTORE8         -> withTwoArgs (\a v -> mstore a (byteIndex 31 v))
        SLOAD           -> error "not implemented"
        SSTORE          -> error "not implemented"
        JUMP            -> return
        JUMPI           -> return
        PC              -> noArgs (\ms' -> push ((fromIntegral.pc) ms') ms')
        MSIZE           -> error "not implemented"
        GAS             -> noArgs (\ms' -> push ((fromIntegral.gas) ms') ms')

        {- 60s and 70s: Push Operations -}
        PUSH1   -> pushOp  1 ee
        PUSH2   -> pushOp  2 ee
        PUSH3   -> pushOp  3 ee
        PUSH4   -> pushOp  4 ee
        PUSH5   -> pushOp  5 ee
        PUSH6   -> pushOp  6 ee
        PUSH7   -> pushOp  7 ee
        PUSH8   -> pushOp  8 ee
        PUSH9   -> pushOp  9 ee
        PUSH10  -> pushOp 10 ee
        PUSH11  -> pushOp 11 ee
        PUSH12  -> pushOp 12 ee
        PUSH13  -> pushOp 13 ee
        PUSH14  -> pushOp 14 ee
        PUSH15  -> pushOp 15 ee
        PUSH16  -> pushOp 16 ee
        PUSH17  -> pushOp 17 ee
        PUSH18  -> pushOp 18 ee
        PUSH19  -> pushOp 19 ee
        PUSH20  -> pushOp 20 ee
        PUSH21  -> pushOp 21 ee
        PUSH22  -> pushOp 22 ee
        PUSH23  -> pushOp 23 ee
        PUSH24  -> pushOp 24 ee
        PUSH25  -> pushOp 25 ee
        PUSH26  -> pushOp 26 ee
        PUSH27  -> pushOp 27 ee
        PUSH28  -> pushOp 28 ee
        PUSH29  -> pushOp 29 ee
        PUSH30  -> pushOp 30 ee
        PUSH31  -> pushOp 31 ee
        PUSH32  -> pushOp 32 ee

        {- f0s: System operations -}
        CREATE  -> error "not implemented"
        CALL    -> error "not implemented"
        RETURN  -> error "halting operations are handled elsewhere"
        SUICIDE -> error "halting operations are handled elsewhere"

safeDiv ::  Integral a => a -> a -> a
safeDiv _ 0 = 0
safeDiv a b = a `div` b

unbool2 ::  (t -> t1 -> Bool) -> t -> t1 -> Word256
unbool2 f = (\a b -> if (f a b) then 0 else 1)

byteIndex ::  Integral b => Word256 -> Word256 -> b
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
