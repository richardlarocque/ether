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

import Control.Applicative (Applicative(..))
import Control.Monad
import Data.Binary
import Data.Bits
import Data.LargeWord
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL

import Ethereum.EVM.InstructionSet as E
import Ethereum.EVM.MachineState
import Ethereum.EVM.ExecutionEnvironment
import Ethereum.SimpleTypes
import Ethereum.Common
import Ethereum.State.Address
import Ethereum.Storage.Context
import qualified Ethereum.FeeSchedule as F

{-
data RunTimeError = OutOfGas
                  | StackUnderflow
                  | InvalidInstruction

data RunResult = OutOfGasHalt
                 | ExceptionHalt Context Integer
                 | NormalHalt Context Integer
                 deriving (Show,Eq)
                 -}

{-
data ExecMonad a = OutOfGas
                 | ExceptionHalt Context MachineState
                 | NormalHalt Context MachineState MemSlice
                 | Step { runStep :: (Context, MachineState, ExecutionEnvironment) -> (a, (Context, MachineState, ExecutionEnvironment)) }

instance Monad (ExecMonad) where
        return a = Step $ \(c, ms, ee) -> (a, (c, ms, ee))
        m >>= k = case m of
                (Step rs) -> \s ->
                        let (a, s') = rs s in
                        case (k a) of 
                                (Step rs') -> Step $ rs' s'
                                exceptional -> exceptional
                exceptional -> exceptional
                -}

data Termination = OutOfGasException
                 | ExceptionHalt
                 | NormalHalt MemSlice

data ExecResult = OutOfGas
                | Result Context MachineState ExecutionEnvironment (Maybe MemSlice)

data ExecMonad a = ExecMonad { runState :: (Context, MachineState, ExecutionEnvironment) ->
        (Either Termination a, (Context, MachineState, ExecutionEnvironment)) }

instance Functor ExecMonad where
        fmap = liftM

instance Applicative ExecMonad where
        pure = return
        (<*>) = ap

instance Monad ExecMonad where
        return a = ExecMonad $ \(c, ms, ee) -> (Right a, (c, ms, ee))
        m >>= k =  ExecMonad $ \s ->
                        let (r, s') = runState m s in
                        case r of
                                Left d -> (Left d, s')
                                Right a -> runState (k a) s'

getContext :: ExecMonad Context
getContext = ExecMonad $ \s@(c,_,_) -> (Right c, s)

getMachineState :: ExecMonad MachineState
getMachineState = ExecMonad $ \s@(_,ms,_) -> (Right ms, s)

putMachineState :: MachineState -> ExecMonad ()
putMachineState ms' = ExecMonad $ \(c,_,ee) -> (Right (), (c, ms', ee))

getEE :: ExecMonad ExecutionEnvironment
getEE = ExecMonad $ \s@(_,_,ee) -> (Right ee, s)

invalidInstruction :: Word8 -> ExecMonad a
invalidInstruction _b = ExecMonad $ \s -> (Left ExceptionHalt, s)

stackUnderflow :: ExecMonad a
stackUnderflow = ExecMonad $ \s -> (Left ExceptionHalt, s)

normalHalt :: MemSlice -> ExecMonad ()
normalHalt bs = ExecMonad $ \s -> (Left $ NormalHalt bs, s)

execute :: Context -> MachineState -> ExecutionEnvironment -> ExecResult
execute c ms ee = case runState execStep (c, ms, ee) of
        (Left OutOfGasException, (c', ms', ee')) -> OutOfGas
        (Left ExceptionHalt, (c', ms', ee'))     -> Result c' ms' ee' Nothing
        (Left (NormalHalt bs), (c', ms', ee'))   -> Result c' ms' ee' (Just bs)
        (Right (), (c', ms', ee'))               -> execute c' ms' ee'

execStep :: ExecMonad ()
execStep = nextOp >>= runOp

nextOp :: ExecMonad Instruction
nextOp = do ee <- getEE
            ms <- getMachineState
            if (pc ms < (fromIntegral $ clength ee))
               then (return STOP) -- Eq. 86.
               else let b = cbyte (fromIntegral $ pc ms) ee
                    in case fromOpcode b of
                        Nothing -> invalidInstruction b
                        Just w -> return w

runOp :: Instruction -> ExecMonad ()
-- 0s: Stop and Arithmetic Operations
runOp STOP    = normalHalt emptyMemSlice

runOp ADD     = stepFee >> stackBinOp (+)
runOp MUL     = stepFee >> stackBinOp (*)
runOp SUB     = stepFee >> stackBinOp (-)
runOp DIV     = stepFee >> stackBinOp safeDiv
runOp SDIV    = stepFee >> stackBinOp (withSign safeDiv)
runOp MOD     = stepFee >> stackBinOp mod
runOp SMOD    = stepFee >> stackBinOp (withSign mod)
runOp EXP     = stepFee >> stackBinOp (^)
runOp NEG     = stepFee >> stackUnOp ((1+).complement)
runOp E.LT    = stepFee >> stackBinOp (unbool2 (<))
runOp E.GT    = stepFee >> stackBinOp (unbool2 (>))
runOp SLT     = stepFee >> stackBinOp (unbool2 (\a b -> (a - b) `testBit` 255))
runOp SGT     = stepFee >> stackBinOp (unbool2 (\a b -> (b - a) `testBit` 255))
runOp E.EQ    = stepFee >> stackBinOp (unbool2 (==))
runOp NOT     = stepFee >> stackUnOp (unbool2 (==) 0)
runOp AND     = stepFee >> stackBinOp (.&.)
runOp OR      = stepFee >> stackBinOp (.|.)
runOp XOR     = stepFee >> stackBinOp xor
runOp BYTE    = stepFee >> stackBinOp (\a b -> fromIntegral $ byteIndex a b)

-- 20s: SHA3
runOp SHA3    = do chargeFee F.sha3
                   a <- pop
                   len <- pop
                   bytes <- memLoad a len
                   push (hashBytes $ memToByteString bytes)

-- 30s: Environment
runOp ADDRESS         = stepFee >> getEE >>= push . fromAddress . address
runOp BALANCE         = chargeFee F.balance >> undefined
runOp ORIGIN          = stepFee >> getEE >>= push . fromAddress . origin
runOp CALLER          = stepFee >> getEE >>= push . fromAddress . caller
runOp CALLVALUE       = stepFee >> getEE >>= push . fromEther . value
runOp CALLDATALOAD    = stepFee >> do a <- pop
                                      v <- dataLoad a 32
                                      push (fromBytes v)
runOp CALLDATASIZE    = stepFee >> dataLength >>= push . fromIntegral
runOp CALLDATACOPY    = stepFee >> do maddr <- pop
                                      daddr <- pop
                                      len <- pop
                                      bs <- dataLoad daddr len
                                      memStore maddr bs
runOp CODESIZE        = stepFee >> getEE >>= push . fromIntegral . clength
runOp CODECOPY        = stepFee >> do maddr <- pop
                                      caddr <- pop
                                      len <- pop
                                      bs <- codeLoad caddr len
                                      memStore maddr bs
runOp GASPRICE        = stepFee >> getEE >>= push . fromEther . gasPrice



-- FIXME: Many of these ain't right.
runOp SUICIDE = normalHalt emptyMemSlice
runOp RETURN  = normalHalt emptyMemSlice

stepFee :: ExecMonad ()
stepFee = return ()

chargeFee :: Integer -> ExecMonad ()
chargeFee _ = return ()

pop :: ExecMonad Word256
pop = do ms <- getMachineState
         case stack ms of
                 (a:s') -> do putMachineState ms{stack=s'}
                              return a
                 _      -> stackUnderflow

push :: Word256 -> ExecMonad ()
push x = do ms <- getMachineState
            let s = stack ms
            putMachineState ms{stack=x:s}

memLoad :: Word256 -> Word256 -> ExecMonad ByteArray
memLoad a len = do ms <- getMachineState
                   let (ms', bytes) = mloadrange a len ms
                   putMachineState ms'
                   return bytes

memStore :: Word256 -> ByteArray -> ExecMonad ()
memStore a bs = do ms <- getMachineState
                   putMachineState $ mstorerange a bs ms

dataLoad :: Word256 -> Word256 -> ExecMonad ByteArray
dataLoad a len = getEE >>= return . drange (a, len)

dataLength :: ExecMonad Int
dataLength = getEE >>= return . dlength

codeLoad :: Word256 -> Word256 -> ExecMonad ByteArray
codeLoad a len = getEE >>= return . crange (a, len)

stackBinOp :: (Word256 -> Word256 -> Word256) -> ExecMonad ()
stackBinOp f = do a1 <- pop
                  a2 <- pop
                  push (f a1 a2)

stackUnOp :: (Word256 -> Word256) -> ExecMonad ()
stackUnOp f = do a1 <- pop
                 push (f a1)


-- Arithmetic helpers.

safeDiv ::  Integral a => a -> a -> a
safeDiv _ 0 = 0
safeDiv a b = a `div` b

-- Check the sign bit
isNeg ::  Word256 -> Bool
isNeg = flip testBit 255

-- Two's complement
neg ::  Word256 -> Word256
neg = (1+).complement

unbool2 ::  (Word256 -> Word256 -> Bool) -> Word256 -> Word256 -> Word256
unbool2 f a b =  if f a b then 1 else 0

-- Adding signs to div and mod
withSign ::  (Word256 -> Word256 -> Word256) -> Word256 -> Word256 -> Word256
withSign f a b = let (aNeg, a') = unsign a
                     (bNeg, b') = unsign b
                 in resign aNeg bNeg $ f a' b'
                    where unsign x = if isNeg x
                                        then (True, neg x)
                                        else (False, x)
                          resign s1 s2 x = if s1 `xor` s2 then neg x else x

extractByte :: (Integral b) => Word256 -> Word256 -> b
extractByte b i = fromIntegral $ encode b `BL.index` fromIntegral i

byteIndex ::  Word256 -> Word256 -> Word8
byteIndex i w = if i < 32
                   then extractByte w i
                   else 0

{-

executeVM :: Context -> Integer -> ExecutionEnvironment
executeVM c g ee =
        let ms = MachineState g 0 initMem 0 []
        in (execStep c ms ee)

execStep :: Context -> MachineState -> ExecutionEnvironment
execStep c ms =
        case getOp ee ms of
                Nothing         -> ExceptionHalt c ms ee

                -- TODO: These should update some state...
                Just STOP       -> NormalHalt c ms ee emptyMemSlice
                Just SUICIDE    -> NormalHalt c ms ee emptyMemSlice

                Just RETURN     -> NormalHalt c ms ee emptyMemSlice -- FIXME very wrong.
                        {-
                        do (ms', (start, len)) <- popTwo ms
                                      return $ snd $ mloadrange start len ms'
                                      -}
                Just w          -> runStep w ee ms

runStep :: Instruction -> Context -> MachineState -> ExecutionEnvironment -> RunResult
runStep w c ms ee = case execOp w ee ms of



step :: Instruction -> ExecutionEnvironment -> MachineState -> Either RunTimeError MemSlice
step w ee ms = do ms' <- execOp w ee ms
                  ms'' <- updatePC w ms'
                  gasCheck ms''
                  execNext ee ms''

gasCheck :: MachineState -> Either RunTimeError ()
gasCheck ms = if outOfGas ms
                 then Left OutOfGas
                 else Right ()

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
        DIV     -> stackBinOp safeDiv
        SDIV    -> stackBinOp (withSign safeDiv)
        MOD     -> stackBinOp mod
        SMOD    -> stackBinOp (withSign mod)
        EXP     -> stackBinOp (^)
        NEG     -> stackUnOp ((1+).complement)
        E.LT    -> stackBinOp (unbool2 (<))
        E.GT    -> stackBinOp (unbool2 (>))
        SLT     -> stackBinOp (unbool2 (\a b -> (a - b) `testBit` 255))
        SGT     -> stackBinOp (unbool2 (\a b -> (b - a) `testBit` 255))
        E.EQ    -> stackBinOp (unbool2 (==))
        NOT     -> stackUnOp (unbool2 (==) 0)
        AND     -> stackBinOp (.&.)
        OR      -> stackBinOp (.|.)
        XOR     -> stackBinOp xor
        BYTE    -> stackBinOp (\a b -> fromIntegral $ byteIndex a b)

        {- 20s: SHA3 -}
        SHA3    -> withTwoArgs $ \a len ms ->
                let (ms', bytes) = mloadrange a len ms
                    hashed = hashBytes $ memToByteString bytes
                in push hashed ms'

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
        POP             -> (liftM fst.pop)
        DUP             -> withArg (\x -> push x . push x )
        SWAP            -> withTwoArgs (\a b -> push b . push a )
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
unbool2 f a b =  if f a b then 1 else 0

byteIndex ::  Word256 -> Word256 -> Word8
byteIndex i w = if i < 32
                   then extractByte w i
                   else 0

-- Check the sign bit
isNeg ::  Word256 -> Bool
isNeg = flip testBit 255

-- Two's complement
neg ::  Word256 -> Word256
neg = (1+).complement

-- Adding signs to div and mod
withSign ::  (Word256 -> Word256 -> Word256) -> Word256 -> Word256 -> Word256
withSign f a b = let (aNeg, a') = unsign a
                     (bNeg, b') = unsign b
                 in resign aNeg bNeg $ f a' b'
                    where unsign x = if isNeg x
                                        then (True, neg x)
                                        else (False, x)
                          resign s1 s2 x = if s1 `xor` s2 then neg x else x

extractByte :: (Integral b) => Word256 -> Word256 -> b
extractByte b i = fromIntegral $ encode b `BL.index` fromIntegral i

noArgs :: (MachineState -> MachineState) -> MachineState -> Either RunTimeError MachineState
noArgs f = return.f

withArg :: (Word256 -> MachineState -> MachineState) ->
        MachineState -> Either RunTimeError MachineState
withArg f ms = do (ms', arg) <- pop ms
                  return $ f arg ms'

withTwoArgs :: (Word256 -> Word256 -> MachineState -> MachineState) ->
        MachineState -> Either RunTimeError MachineState
withTwoArgs f ms = do (ms', (arg1, arg2)) <- popTwo ms
                      return $ f arg1 arg2 ms'

withThreeArgs :: (Word256 -> Word256 -> Word256 -> MachineState -> MachineState) ->
        MachineState -> Either RunTimeError MachineState
withThreeArgs f ms = do (ms', (arg1, arg2, arg3)) <- popThree ms
                        return $ f arg1 arg2 arg3 ms'

stackBinOp :: (Word256 -> Word256 -> Word256) ->
        MachineState -> Either RunTimeError MachineState
stackBinOp f = withTwoArgs (\a b -> push (f a b))

stackUnOp :: (Word256 -> Word256) -> MachineState -> Either RunTimeError MachineState
stackUnOp f = withArg (push . f)

pushOp :: Word256 -> ExecutionEnvironment -> MachineState -> Either RunTimeError MachineState
pushOp l _ _ | l > 32 = error "Invalid push length argument"
pushOp l ee ms = let s = fromIntegral $ pc ms + 1
                     v = fromBytes $ crange (s, l) ee
                 in return $ (addPC l . push v) ms

-}
