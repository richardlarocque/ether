module EVM where

import Data.Word
import Data.LargeWord
import Data.Maybe
import qualified Data.Map as M
import Control.Monad

type Gas = Integer

data Instruction =
  -- 0s: Stop and Arithmetic Operations
    STOP
  | ADD
  | MUL
  | SUB
  | DIV
  | SDIV
  | MOD
  | SMOD
  | EXP
  | NEG
  | LT
  | GT
  | SLT
  | SGT
  | EQ
  | NOT
  | AND
  | OR
  | XOR
  | BYTE

  -- 20s: SHA3
  | SHA3

  -- 30s: Environment
  | ADDRESS
  | BALANCE
  | ORIGIN
  | CALLER
  | CALLVALUE
  | CALLDATALOAD
  | CALLDATASIZE
  | CALLDATACOPY
  | CODESIZE
  | CODECOPY
  | GASPRICE

  -- 40s: Block Information
  | PREVHASH
  | COINBASE
  | TIMESTAMP
  | NUMBER
  | DIFFICULTY
  | GASLIMIT

  -- 50s: Stack, Memory, Storage and Flow Operations
  | POP
  | DUP
  | SWAP
  | MLOAD
  | MSTORE
  | MSTORE8
  | SLOAD
  | SSTORE
  | JUMP
  | JUMPI
  | PC
  | MSIZE
  | GAS

  -- 60s and 70s: Push Operations
  | PUSH1
  | PUSH2
  | PUSH3
  | PUSH4
  | PUSH5
  | PUSH6
  | PUSH7
  | PUSH8
  | PUSH9
  | PUSH10
  | PUSH11
  | PUSH12
  | PUSH13
  | PUSH14
  | PUSH15
  | PUSH16
  | PUSH17
  | PUSH18
  | PUSH19
  | PUSH20
  | PUSH21
  | PUSH22
  | PUSH23
  | PUSH24
  | PUSH25
  | PUSH26
  | PUSH27
  | PUSH28
  | PUSH29
  | PUSH30
  | PUSH31
  | PUSH32

  -- f0s: System operations
  | CREATE
  | CALL
  | RETURN
  | SUICIDE

  -- Non-existent sort of value
  | INVALID
  deriving Eq

stackGet :: Instruction -> Int
stackGet STOP = 0
stackGet ADD = 2
stackGet MUL = 2
stackGet SUB = 2
stackGet DIV = 2
stackGet MOD = 2
stackGet EXP = 2
stackGet NEG = 1
stackGet RETURN = 2
stackGet SUICIDE = 1
stackGet INVALID = 0

fromOpcode :: Word8 -> Intstruction
fromOpcode STOP
fromOpcode ADD
fromOpcode MUL
fromOpcode SUB
fromOpcode DIV
fromOpcode MOD
fromOpcode EXP
fromOpcode NEG

-- 50s: Stack, Memory, Storage and Flow Operations
fromOpcode POP
fromOpcode DUP
fromOpcode SWAP

-- 60s and 70s: Push Operations
fromOpcode PUSH1
fromOpcode PUSH2
fromOpcode PUSH3
fromOpcode PUSH4
fromOpcode PUSH5
fromOpcode PUSH6
fromOpcode PUSH7
fromOpcode PUSH8
fromOpcode PUSH9
fromOpcode PUSH10
fromOpcode PUSH11
fromOpcode PUSH12
fromOpcode PUSH13
fromOpcode PUSH14
fromOpcode PUSH15
fromOpcode PUSH16
fromOpcode PUSH17
fromOpcode PUSH18
fromOpcode PUSH19
fromOpcode PUSH20
fromOpcode PUSH21
fromOpcode PUSH22
fromOpcode PUSH23
fromOpcode PUSH24
fromOpcode PUSH25
fromOpcode PUSH26
fromOpcode PUSH27
fromOpcode PUSH28
fromOpcode PUSH29
fromOpcode PUSH30
fromOpcode PUSH31
fromOpcode PUSH32

-- f0s: System operations
fromOpcode CREATE
fromOpcode CALL
fromOpcode RETURN
fromOpcode SUICIDE

fromOpcode INVALID -- pseudo-value

-- Appendix B: Fee schedule
fee_step = 0
fee_stop = 1
fee_suicide = 0
fee_sha3 = 20
fee_sload = 20
fee_sstore = 100
fee_balance = 20
fee_create = 100
fee_call = 20
fee_memory = 1
fee_txdata = 5
fee_transaction = 500

type Memory = M.Map Word256 Word256
type MemSlice = [Word256]

type Stack = [Word256]

stackAtLeast :: Int -> Stack -> Bool
stackAtLeast n s = drop (n-1) s /= []

data Address = Address

type Ether = Integer

data SystemState = SystemState

type ByteArray = [Word8]

data MachineState = MS {
  gas :: Gas,
  pc :: Int,
  memory :: Memory,
  stack :: Stack
}

memRange :: MachineState -> (Word256, Word256) -> MemSlice
memRange ms (startAddr, endAddr) =
  map (memWord ms) [startAddr .. endAddr]

memWord :: MachineState -> Word256 -> Word256
memWord ms addr = fromMaybe 0 $ M.lookup addr (memory ms)

codeRange :: MachineState -> (Word256, Word256) -> MemSlice
codeRange ms (startAddr, endAddr) =
  map (codeByte ms) 

data ExecutionEnvironment = EE {
  owner :: Address,
  sender :: Address,
  gasPrice :: Ether,
  input :: ByteArray,
  execCause :: Address,
  value :: Ether,
  code :: [Instruction]
}

getNextCost :: ExecutionEnvironment -> MachineState -> Gas
getNextCost ee ms =
  (getMemoryCost ee ms) + (getInstructionCost ee ms)

getMemoryCost ee ms = 0

getInstructionCost ee ms =
  let instr = getNextInstr ee ms in
  case instr of
    -- TODO: Lots more to define.
    STOP -> fee_stop
    _ -> fee_step

-- Equation 86
getNextInstr :: ExecutionEnvironment -> MachineState -> Instruction
getNextInstr (EE {code=is}) (MS {pc=counter}) =
  if counter < length is
    then is !! counter
    else STOP

data RunTimeError =
  OutOfGas
  | InvalidInstruction
  | StackUnderflow
  deriving Eq

-- Equation 87
checkException :: ExecutionEnvironment -> MachineState -> Maybe RunTimeError
checkException ee ms =
  let w = getNextInstr ee ms in msum $
  [ if gas ms < getNextCost ee ms then Just OutOfGas else Nothing,
    if INVALID == w then Just InvalidInstruction else Nothing,
    if stackAtLeast (stackGet w) (stack ms) then Just StackUnderflow else Nothing ]

checkHalt :: ExecutionEnvironment -> MachineState -> Maybe MemSlice
checkHalt ee ms =
  let instr = getNextInstr ee ms in
  case instr of
    RETURN -> let startAddr:endAddr:_ = stack ms in
              Just $ memRange ms (startAddr, endAddr)
    STOP -> Just []
    SUICIDE -> Just []
    _ -> Nothing

execInstruction :: ExecutionEnvironment -> MachineState -> MachineState
execInstruction ee ms =
  case (getNextInstr ee ms) of
    ADD -> execStackBinaryOp ee ms (+)
    MUL -> execStackBinaryOp ee ms (*)
    SUB -> execStackBinaryOp ee ms (-)
    DIV -> execStackBinaryOp ee ms (safeDiv)
    MOD -> execStackBinaryOp ee ms (mod)
    --EXP -> execSimpleBinOp ee ms (**)
    NEG -> execStackUnaryOp ee ms (*(-1))

    POP -> execStackOp ee ms tail
    DUP -> execStackOp ee ms (\(x:xs) -> x:x:xs)
    SWAP -> execStackOp ee ms (\(a:b:xs) -> b:a:xs)

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

execStackOp :: ExecutionEnvironment-> MachineState -> (Stack -> Stack) -> MachineState
execStackOp ee ms f = MS {
    gas= (gas ms) - getNextCost ee ms,
    pc= (pc ms) + 1,
    memory= memory ms
    stack= f (stack ms),
  }

execStackUnaryOp :: ExecutionEnvironment-> MachineState -> (Word256 -> Word256) -> MachineState
execStackUnaryOp ee ms f = execStackOp ee ms (\(x:xs) -> (f x):xs)

execStackBinaryOp :: ExecutionEnvironment-> MachineState -> (Word256 -> Word256 -> Word256) -> MachineState
execStackBinaryOp ee ms f = execStackOp ee ms (\(a:b:xs) -> (f a b):xs)

execPushOp ee ms n = MS {
  gas= (gas ms) - getNextCost ee ms,
  pc= (pc ms) + 1 + n
  memory= memory ms
  stack= codeRange
}
  

initialMachineState :: ExecutionEnvironment -> MachineState
initialMachineState ee = MS {
    gas= (value ee) `div` (gasPrice ee),
    pc=0,
    memory=M.empty,
    stack=[]
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
    Nothing -> runVM' ee (execInstruction ee ms)
