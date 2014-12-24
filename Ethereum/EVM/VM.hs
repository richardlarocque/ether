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

import           Control.Applicative               (Applicative (..))
import           Control.Monad
import           Data.Bits
import qualified Data.ByteString                   as B
import           Data.LargeWord
import           Data.Maybe
import           Data.Word

import           Ethereum.Crypto.Hash
import           Ethereum.EVM.ExecutionEnvironment
import           Ethereum.EVM.InstructionSet       as E
import           Ethereum.EVM.MachineState
import qualified Ethereum.FeeSchedule              as F
import           Ethereum.SimpleTypes
import           Ethereum.State.Account
import           Ethereum.State.Address
import           Ethereum.State.Block
import           Ethereum.Storage.Context

data Termination = OutOfGasException
                 | InvalidInstruction
                 | StackUnderflow
                 | NormalHalt B.ByteString
                 deriving (Show, Eq)

data ExecResult = OutOfGas
                | Result Context MachineState ExecutionEnvironment (Maybe B.ByteString)

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

putContext :: Context -> ExecMonad ()
putContext c' = ExecMonad $ \(_,ms,ee) -> (Right (), (c', ms, ee))

getMachineState :: ExecMonad MachineState
getMachineState = ExecMonad $ \s@(_,ms,_) -> (Right ms, s)

putMachineState :: MachineState -> ExecMonad ()
putMachineState ms' = ExecMonad $ \(c,_,ee) -> (Right (), (c, ms', ee))

getEE :: ExecMonad ExecutionEnvironment
getEE = ExecMonad $ \s@(_,_,ee) -> (Right ee, s)

invalidInstruction :: Word8 -> ExecMonad a
invalidInstruction _b = ExecMonad $ \s -> (Left InvalidInstruction, s)

stackUnderflow :: ExecMonad a
stackUnderflow = ExecMonad $ \s -> (Left StackUnderflow, s)

normalHalt :: B.ByteString -> ExecMonad ()
normalHalt bs = ExecMonad $ \s -> (Left $ NormalHalt bs, s)

outOfGasException :: ExecMonad ()
outOfGasException = ExecMonad $ (\s -> (Left OutOfGasException, s))

-- | Equation 64
runMessageCall :: BlockHeader -> Context -> Address -> Address -> Address -> Integer -> Integer -> Integer -> B.ByteString -> ExecResult
runMessageCall bh c s o r g gp v dat =
        let ch = fromJust $ getAccount c r >>= return . codeHash
            cod = lookupCodeHash c ch
            ee = EE r o gp dat s v cod bh
        in executeCode c g ee

lookupCodeHash :: Context -> CodeHash -> B.ByteString
lookupCodeHash c ch = case ch of
        NullCodeHash -> B.empty
        CodeHash h -> fromMaybe B.empty $ lookupInStorage c h

executeCode :: Context -> Integer -> ExecutionEnvironment -> ExecResult
executeCode c g ee = execute' c (MS g 0 initMem 0 []) ee

execute' :: Context -> MachineState -> ExecutionEnvironment -> ExecResult
execute' c ms ee = case runState execStep (c, ms, ee) of
        (Left OutOfGasException, _)                -> OutOfGas
        (Left InvalidInstruction, (c', ms', ee'))  -> Result c' ms' ee' Nothing
        (Left StackUnderflow, (c', ms', ee'))      -> Result c' ms' ee' Nothing
        (Left (NormalHalt bs), (c', ms', ee'))     -> Result c' ms' ee' (Just bs)
        (Right (), (c', ms', ee'))                 -> execute' c' ms' ee'

execStep :: ExecMonad ()
execStep = do op <- nextOp
              chargeOpFee op
              chargeMemFee (runOp op)
              updatePC op

nextOp :: ExecMonad Instruction
nextOp = do ee <- getEE
            ms <- getMachineState
            if pc ms > (fromIntegral $ clength ee)
               then (return STOP) -- Eq. 86.
               else let b = cbyte (fromIntegral $ pc ms) ee
                    in case fromOpcode b of
                        Nothing -> invalidInstruction b
                        Just w -> return w

chargeOpFee :: Instruction -> ExecMonad ()
chargeOpFee SSTORE      = return () -- Handled in SSTORE's runOp
chargeOpFee CALL        = return () -- Handled in CALL's runOp
chargeOpFee CREATE      = undefined -- FIXME: handle this
chargeOpFee SHA3        = chargeFee F.sha3
chargeOpFee SLOAD       = chargeFee F.sload
chargeOpFee BALANCE     = chargeFee F.balance
chargeOpFee STOP        = chargeFee F.stop
chargeOpFee SUICIDE     = chargeFee F.suicide
chargeOpFee _           = chargeFee F.step

runOp :: Instruction -> ExecMonad ()
-- 0s: Stop and Arithmetic Operations
runOp STOP    = normalHalt emptyMemSlice

runOp ADD     = stackBinOp (+)
runOp MUL     = stackBinOp (*)
runOp SUB     = stackBinOp (-)
runOp DIV     = stackBinOp safeDiv
runOp SDIV    = stackBinOp (withSign safeDiv)
runOp MOD     = stackBinOp mod
runOp SMOD    = stackBinOp (withSign mod)
runOp EXP     = stackBinOp (^)
runOp NEG     = stackUnOp ((1+).complement)
runOp E.LT    = stackBinOp (unbool2 (<))
runOp E.GT    = stackBinOp (unbool2 (>))
runOp SLT     = stackBinOp (unbool2 (\a b -> (a - b) `testBit` 255))
runOp SGT     = stackBinOp (unbool2 (\a b -> (b - a) `testBit` 255))
runOp E.EQ    = stackBinOp (unbool2 (==))
runOp NOT     = stackUnOp (unbool2 (==) 0)
runOp AND     = stackBinOp (.&.)
runOp OR      = stackBinOp (.|.)
runOp XOR     = stackBinOp xor
runOp BYTE    = stackBinOp (\a b -> fromIntegral $ byteIndex a b)

-- 20s: SHA3
runOp SHA3    = do a <- pop
                   len <- pop
                   bytes <- memLoad a len
                   push (hashAsWord $ memToByteString bytes)

-- 30s: Environment
runOp ADDRESS         = getEE >>= push . fromAddress . address
runOp BALANCE         = do c <- getContext
                           addr <- getEE >>= return . address
                           push $ fromMaybe 0 (getAccount c addr >>= return . fromIntegral . balance)
runOp ORIGIN          = getEE >>= push . fromAddress . origin
runOp CALLER          = getEE >>= push . fromAddress . caller
runOp CALLVALUE       = getEE >>= push . fromEther . value
runOp CALLDATALOAD    = do a <- pop
                           v <- dataLoad a 32
                           push (fromBytes v)
runOp CALLDATASIZE    = dataLength >>= push . fromIntegral
runOp CALLDATACOPY    = do maddr <- pop
                           daddr <- pop
                           len <- pop
                           bs <- dataLoad daddr len
                           memStore maddr bs
runOp CODESIZE        = getEE >>= push . fromIntegral . clength
runOp CODECOPY        = do maddr <- pop
                           caddr <- pop
                           len <- pop
                           bs <- codeLoad caddr len
                           memStore maddr bs
runOp GASPRICE        = getEE >>= push . fromEther . gasPrice

-- 40s: Block Information
runOp PREVHASH        = getEE >>= push . parentHash . blockHeader
runOp COINBASE        = getEE >>= push . fromAddress . coinbase . blockHeader
runOp TIMESTAMP       = getEE >>= push . fromIntegral . timestamp . blockHeader
runOp NUMBER          = getEE >>= push . fromIntegral . number . blockHeader
runOp DIFFICULTY      = getEE >>= push . fromIntegral . difficulty . blockHeader
runOp GASLIMIT        = getEE >>= push . fromIntegral . gasLimit . blockHeader

-- 50s: Stack, Memory, Storage and Flow Operations
runOp POP       = do { _ <- pop; return () }
runOp DUP       = do { x <- pop; push x; push x }
runOp SWAP      = do { x <- pop; y <- pop; push x; push y }
runOp MLOAD     = do x <- pop
                     w <- memLoadWord x
                     push w
runOp MSTORE    = do a <- pop
                     w <- pop
                     memStoreWord a w
runOp MSTORE8   = do a <- pop
                     w <- pop
                     memStoreByte a w
runOp SLOAD     = do k <- pop
                     c <- getContext
                     addr <- getEE >>= return . address
                     push $ accountLoad c addr k
runOp SSTORE    = do k <- pop
                     v <- pop
                     c <- getContext
                     addr <- getEE >>= return . address
                     let orig = accountLoad c addr k
                     let c' = accountStore c addr (k,v)
                     putContext c'
                     when (orig == 0) (chargeFee F.sstore)
                     when (v /= 0) (chargeFee F.sstore)
runOp JUMP      = pop >>= setPC
runOp JUMPI     = do a <- pop
                     c <- pop
                     if (c == 0)
                        then setPC a
                        else incrementPC
runOp PC        = getMachineState >>= push . fromIntegral . pc
runOp MSIZE     = getMachineState >>= push . fromIntegral . memsize
runOp GAS       = getMachineState >>= push . fromIntegral . gas

-- 60s and 70s: Push Operations
runOp PUSH1     = pushOp PUSH1
runOp PUSH2     = pushOp PUSH2
runOp PUSH3     = pushOp PUSH3
runOp PUSH4     = pushOp PUSH4
runOp PUSH5     = pushOp PUSH5
runOp PUSH6     = pushOp PUSH6
runOp PUSH7     = pushOp PUSH7
runOp PUSH8     = pushOp PUSH8
runOp PUSH9     = pushOp PUSH9
runOp PUSH10    = pushOp PUSH10
runOp PUSH11    = pushOp PUSH11
runOp PUSH12    = pushOp PUSH12
runOp PUSH13    = pushOp PUSH13
runOp PUSH14    = pushOp PUSH14
runOp PUSH15    = pushOp PUSH15
runOp PUSH16    = pushOp PUSH16
runOp PUSH17    = pushOp PUSH17
runOp PUSH18    = pushOp PUSH18
runOp PUSH19    = pushOp PUSH19
runOp PUSH20    = pushOp PUSH20
runOp PUSH21    = pushOp PUSH21
runOp PUSH22    = pushOp PUSH22
runOp PUSH23    = pushOp PUSH23
runOp PUSH24    = pushOp PUSH24
runOp PUSH25    = pushOp PUSH25
runOp PUSH26    = pushOp PUSH26
runOp PUSH27    = pushOp PUSH27
runOp PUSH28    = pushOp PUSH28
runOp PUSH29    = pushOp PUSH29
runOp PUSH30    = pushOp PUSH30
runOp PUSH31    = pushOp PUSH31
runOp PUSH32    = pushOp PUSH32

-- f0s: System operations
runOp CREATE    = undefined
runOp CALL      = undefined
runOp RETURN    = do start <- pop
                     len <- pop
                     r <- memLoad start len
                     normalHalt r
runOp SUICIDE   = normalHalt emptyMemSlice  -- FIXME: not right

updatePC :: Instruction -> ExecMonad ()
updatePC JUMP         = return ()  -- Handled it earlier.
updatePC JUMPI        = return ()  -- Handled it earlier.
updatePC p | isPush p = addPC (pushLen p + 1)
updatePC _            = incrementPC

remitFee :: Integer -> ExecMonad ()
remitFee f = do ms <- getMachineState
                let g' = gas ms + f
                let ms' = ms{gas=g'}
                putMachineState ms'

chargeFee :: Integer -> ExecMonad ()
chargeFee f = do ms <- getMachineState
                 let g' = gas ms - f
                 let ms' = ms{gas=g'}
                 when (outOfGas ms') outOfGasException
                 putMachineState ms'

chargeMemFee :: ExecMonad a -> ExecMonad a
chargeMemFee x = do s <- getMachineState >>= return . memsize
                    a <- x
                    s' <- getMachineState >>= return . memsize
                    chargeFee $ F.memory * (fromIntegral $ s' - s)
                    return a

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

memLoad :: Word256 -> Word256 -> ExecMonad B.ByteString
memLoad a len = do ms <- getMachineState
                   let (ms', bytes) = mloadrange a len ms
                   putMachineState ms'
                   return bytes

memLoadWord :: Word256 -> ExecMonad Word256
memLoadWord a = memLoad a 32 >>= return . fromBytes

memStore :: Word256 -> B.ByteString -> ExecMonad ()
memStore a bs = do ms <- getMachineState
                   putMachineState $ mstorerange a bs ms

memStoreWord :: Word256 -> Word256 -> ExecMonad ()
memStoreWord a w = memStore a (toBytes w)

memStoreByte :: Word256 -> Word256 -> ExecMonad ()
memStoreByte a w = memStore a (B.singleton $ B.last $ toBytes w)

dataLoad :: Word256 -> Word256 -> ExecMonad B.ByteString
dataLoad a len = getEE >>= return . drange (a, len)

dataLength :: ExecMonad Int
dataLength = getEE >>= return . dlength

codeLoad :: Word256 -> Word256 -> ExecMonad B.ByteString
codeLoad a len = getEE >>= return . crange (a, len)

stackBinOp :: (Word256 -> Word256 -> Word256) -> ExecMonad ()
stackBinOp f = do a1 <- pop
                  a2 <- pop
                  push (f a1 a2)

stackUnOp :: (Word256 -> Word256) -> ExecMonad ()
stackUnOp f = do a1 <- pop
                 push (f a1)

pushOp :: Instruction -> ExecMonad ()
pushOp op = do ms <- getMachineState
               let s = (pc ms) + 1
               let len = pushLen op
               v <- codeLoad (fromIntegral s) (fromIntegral len)
               push $ fromBytes v

setPC :: Word256 -> ExecMonad ()
setPC pc' = do ms <- getMachineState
               putMachineState ms{pc=(fromIntegral pc')}

addPC :: Integer -> ExecMonad ()
addPC x = do ms <- getMachineState
             putMachineState ms{pc=(pc ms)+x}

incrementPC :: ExecMonad ()
incrementPC = addPC 1

callOp :: Integer -> Address -> Integer -> Word256 -> Word256 -> Word256 -> Word256 -> ExecMonad Word256
callOp gl toAddr v dStart dLen retStart retLen = do
        chargeFee F.call
        chargeFee gl
        ee <- getEE
        c <- getContext
        dat <- memLoad dStart dLen
        case runMessageCall (blockHeader ee) c (address ee) (origin ee) toAddr gl (gasPrice ee) v dat of
                OutOfGas -> return 0
                Result c' ms' _ ret ->
                        do let g' = gas ms'
                           let ret' = fromMaybe B.empty ret
                           memStore retStart (B.take (fromIntegral retLen) ret')
                           putContext c'
                           remitFee g'
                           return 1

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
                          resign s1 s2 x = if s1 /= s2 then neg x else x

extractByte :: (Integral b) => Word256 -> Int -> b
extractByte b i = fromIntegral $ (b .&. (0xff `shiftL` i)) `shiftR` i

byteIndex ::  Word256 -> Word256 -> Word8
byteIndex i w = if i < 32
                   then extractByte w (fromIntegral i)
                   else 0

lowestByte :: Integral a => Word256 -> a
lowestByte = fromIntegral . (0xff .&.)
