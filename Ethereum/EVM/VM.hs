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
import Data.Byteable
import Data.Bits
import Crypto.Hash
import Data.LargeWord
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL

import Ethereum.EVM.InstructionSet as E
import Ethereum.EVM.MachineState
import Ethereum.EVM.ExecutionEnvironment
import Ethereum.SimpleTypes

import Debug.Trace

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
                Just w          -> step w ee ms

step :: Instruction -> ExecutionEnvironment -> MachineState -> Either RunTimeError MemSlice
step w ee ms = do ms' <- execOp w ee ms
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
        MUL     -> stackBinOp (*)
        SUB     -> stackBinOp (-)
        DIV     -> stackBinOp (safeDiv)
        SDIV    -> stackBinOp (withSign safeDiv)
        MOD     -> stackBinOp (mod)
        SMOD    -> stackBinOp (withSign mod)
        EXP     -> stackBinOp (^)
        NEG     -> stackUnOp ((1+).complement)
        E.LT    -> stackBinOp (unbool2 (<))
        E.GT    -> stackBinOp (unbool2 (>))
        SLT     -> stackBinOp (unbool2 (\a b -> (a - b) `testBit` 255))
        SGT     -> stackBinOp (unbool2 (\a b -> (b - a) `testBit` 255))
        E.EQ    -> stackBinOp (unbool2 (==))
        NOT     -> stackUnOp ((unbool2 (==)) 0)
        AND     -> stackBinOp (.&.)
        OR      -> stackBinOp (.|.)
        XOR     -> stackBinOp (xor)
        BYTE    -> stackBinOp (\a b -> fromIntegral $ byteIndex a b)

        {- 20s: SHA3 -}
        SHA3    -> withTwoArgs $ \a len ms ->
                let (ms', bytes) = mloadrange a len ms
                    hashed = (hash $ memToByteString bytes) :: Digest SHA3_256
                    asWord256 = (decode . BL.fromStrict . Data.Byteable.toBytes) hashed :: Word256
                in push asWord256 ms'

        {- 30s: Environment -}
        ADDRESS         -> noArgs ((push.fromAddress) (address ee))
        BALANCE         -> error "not implemented"
        ORIGIN          -> noArgs ((push.fromAddress) (origin ee))
        CALLER          -> noArgs ((push.fromAddress) (caller ee))
        CALLVALUE       -> noArgs ((push.fromEther) (value ee))
        CALLDATALOAD    -> withArg (\a -> (push.fromBytes) $ drange (a,32) ee)
        CALLDATASIZE    -> noArgs ((push.fromIntegral) $ dlength ee)
        CALLDATACOPY    -> withThreeArgs $
                \maddr daddr len -> let bytes = drange (daddr, len) ee
                                    in mstorerange maddr bytes
        CODESIZE        -> noArgs ((push.fromIntegral) $ clength ee)
        CODECOPY        -> withThreeArgs $
                \maddr caddr len -> let bytes = crange (caddr, len) ee
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
        POP             -> ((liftM fst).pop)
        DUP             -> withArg (\x -> (push x).(push x))
        SWAP            -> withTwoArgs (\a b -> (push b).(push a))
        MLOAD           -> withArg (\x ms' -> let (ms'', v) = mload x ms' in push v ms'')
        MSTORE          -> withTwoArgs mstore
        MSTORE8         -> withTwoArgs (\a v -> mstorerange a (V.fromList [byteIndex 31 v]))
        SLOAD           -> error "not implemented"
        SSTORE          -> error "not implemented"
        JUMP            -> return
        JUMPI           -> return
        PC              -> noArgs (\ms' -> push ((fromIntegral.pc) ms') ms')
        MSIZE           -> noArgs (\ms' -> push ((fromIntegral.memsize) ms') ms')
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

unbool2 ::  (Word256 -> Word256 -> Bool) -> Word256 -> Word256 -> Word256
unbool2 f =  (\a b -> if (f a b) then 1 else 0)

byteIndex ::  Word256 -> Word256 -> Word8
byteIndex i w = if i < 32
                   then extractByte w i
                   else 0

-- Check the sign bit
isNeg ::  Word256 -> Bool
isNeg = (flip testBit) 255

-- Two's complement
neg ::  Word256 -> Word256
neg = (1+).complement

-- Adding signs to div and mod
withSign ::  (Word256 -> Word256 -> Word256) -> (Word256 -> Word256 -> Word256)
withSign f = (\a b -> let (aNeg, a') = unsign a
                          (bNeg, b') = unsign b
                      in resign aNeg bNeg $ f a' b')
                      where unsign x = if isNeg x then (True, neg x) else (False, x)
                            resign s1 s2 x = if s1 `xor` s2 then neg x else x

extractByte :: (Integral b) => Word256 -> Word256 -> b
extractByte b i = fromIntegral $ (encode b) `BL.index` (fromIntegral i)

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
                     v = fromBytes $ crange (s, l) ee
                 in return $ ((addPC l) . (push v)) ms
