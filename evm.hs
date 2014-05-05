module Main where

import Data.Word
import Data.LargeWord

type Gas = Integer

data Instruction =
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

  | SUICIDE

  | INVALID -- pseudo-value
  deriving Eq

stackReq :: Instruction -> Int
stackReq STOP = 0
stackReq ADD = 2
stackReq MUL = 2
stackReq SUB = 2
stackReq DIV = 2
stackReq INVALID = 0

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

data Memory = Memory

type Stack = [Word256]

stackAtLeast :: Int -> Stack -> Bool
stackAtLeast n s = drop (n-1) s /= []

data Address = Address

data Ether = Ether

data SystemState = SystemState

type ByteArray = [Word8]

data MachineState = MS {
  gas :: Gas,
  pc :: Int,
  memory :: Memory,
  stack :: Stack
}

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

-- Equation 87
checkException :: ExecutionEnvironment -> MachineState -> Bool
checkException ee ms =
  let w = getNextInstr ee ms in
  (gas ms < getNextCost ee ms) ||
  (INVALID == w) ||
  (stackAtLeast (stackReq w) (stack ms))

checkHalt :: ExecutionEnvironment -> MachineState -> Maybe ByteArray
checkHalt ee ms =
  -- TODO: Do something about H return
  let instr = getNextInstr ee ms in
  case instr of
    -- RETURN -> ...
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

execSimpleUnOp ee ms op =
  let gas' = (gas ms) - getNextCost ee ms
      pc' = (pc ms) + 1
      a:ws = stack ms
      stack' = (op a):ws
      memory' = memory ms in
  MS { gas=gas', pc=pc', memory=memory', stack=stack' }

execSimpleBinOp ee ms op =
  let gas' = (gas ms) - getNextCost ee ms
      pc' = (pc ms) + 1
      a:b:ws = stack ms
      stack' = (a `op` b):ws
      memory' = memory ms in
  MS { gas=gas', pc=pc', memory=memory', stack=stack' }

main = do
  print "Hello!"
