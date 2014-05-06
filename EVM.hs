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
  -- TODO: Find some way to implement SDIV
  | MOD
  -- TODO: SMOD
  | EXP
  | NEG

  -- 50s: Stack, Memory, Storage and Flow Operations
  | POP
  | DUP
  | SWAP
  -- And all the rest...

  -- f0s: System operations
  -- | CREATE
  -- | CALL
  | RETURN
  | SUICIDE

  | INVALID -- pseudo-value
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
    ADD -> execSimpleBinOp ee ms (+)
    MUL -> execSimpleBinOp ee ms (*)
    SUB -> execSimpleBinOp ee ms (-)
    DIV -> execSimpleBinOp ee ms (divOp)
    MOD -> execSimpleBinOp ee ms (mod)
    --EXP -> execSimpleBinOp ee ms (**)
    NEG -> execSimpleUnOp ee ms (*(-1))
  where divOp a 0 = 0
        divOp a b = a `div` b

execSimpleUnOp :: ExecutionEnvironment-> MachineState -> (Word256 -> Word256) -> MachineState
execSimpleUnOp ee ms op =
  let gas' = (gas ms) - getNextCost ee ms
      pc' = (pc ms) + 1
      a:ws = stack ms
      stack' = (op a):ws
      memory' = memory ms in
  MS { gas=gas', pc=pc', memory=memory', stack=stack' }

execSimpleBinOp :: ExecutionEnvironment-> MachineState -> (Word256 -> Word256 -> Word256) -> MachineState
execSimpleBinOp ee ms op =
  let gas' = (gas ms) - getNextCost ee ms
      pc' = (pc ms) + 1
      a:b:ws = stack ms
      stack' = (a `op` b):ws
      memory' = memory ms in
  MS { gas=gas', pc=pc', memory=memory', stack=stack' }

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
