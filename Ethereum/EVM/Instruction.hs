module Ethereum.EVM.Instruction where

import qualified Data.ByteString as B
import           Data.LargeWord
import           Data.Maybe
import           Data.Word
import           Ethereum.EVM.TransactionSubstate
import           Ethereum.Storage.Context

type OpState = (B.ByteString, Integer, [Word256], TransactionSubstate, Context)

data OpData = OpData {
  opCode :: Word8,
  opInstr :: Instruction,
  opPushCount :: Int,
  opPopCount :: Int,
  opFunc :: OpState -> OpState,
  opPushLength :: Maybe Integer
}

data Instruction =
{- 0s: Stop and Arithmetic Operations -}
      STOP
    | ADD
    | MUL
    | SUB
    | DIV
    | SDIV
    | MOD
    | SMOD
    | ADDMOD
    | MULMOD
    | EXP
    | SIGNEXTEND

{- 10s: Comparison & Bitwise Logic Operations -}
    | LT
    | GT
    | SLT
    | SGT
    | EQ
    | ISZERO
    | AND
    | OR
    | XOR
    | NOT
    | BYTE

{- 20s: SHA3 -}
    | SHA3

{- 30s: Environment -}
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
    | EXTCODESIZE
    | EXTCODECOPY

{- 40s: Block Information -}
    | PREVHASH
    | COINBASE
    | TIMESTAMP
    | NUMBER
    | DIFFICULTY
    | GASLIMIT

{- 50s: Stack, Memory, Storage and Flow Operations -}
    | POP
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
    | JUMPDEST

{- 60s and 70s: Push Operations -}
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

{- 80s: Duplication operations -}
    | DUP1
    | DUP2
    | DUP3
    | DUP4
    | DUP5
    | DUP6
    | DUP7
    | DUP8
    | DUP9
    | DUP10
    | DUP11
    | DUP12
    | DUP13
    | DUP14
    | DUP15
    | DUP16

{- 90s: Exchange operations -}
    | SWAP1
    | SWAP2
    | SWAP3
    | SWAP4
    | SWAP5
    | SWAP6
    | SWAP7
    | SWAP8
    | SWAP9
    | SWAP10
    | SWAP11
    | SWAP12
    | SWAP13
    | SWAP14
    | SWAP15
    | SWAP16

{- f0s: System operations -}
    | LOG0
    | LOG1
    | LOG2
    | LOG3
    | LOG4

{- f0s: System operations -}
    | CREATE
    | CALL
    | CALLCODE
    | RETURN
    | SUICIDE

{- Other -}
    | UNDEFINED
    deriving (Show,Eq)

stackOp :: ([Word256] -> [Word256]) -> OpState -> OpState
stackOp f (mem, memsize, stack, a, sigma) = (mem, memsize, f stack, a, sigma)

binArithOp :: Word8 -> Instruction -> (Word256 -> Word256 -> Word256) -> OpData
binArithOp b i f = OpData {
  opCode = b,
  opInstr = i,
  opFunc = stackOp $ stackBinOp f,
  opPushCount = 1,
  opPopCount = 2,
  opPushLength = Nothing
}

stackBinOp :: (Word256 -> Word256 -> Word256) -> [Word256] -> [Word256]
stackBinOp f (a1:a2:xs) = (f a1 a2):xs
stackBinOp _ _ = error "Stack error"

ops :: [OpData]
ops = [
  binArithOp 0x01 ADD (+),
  binArithOp 0x02 MUL (*),
  binArithOp 0x03 SUB (-),
  binArithOp 0x03 DIV (div)]

opsByOpcode :: [(Word8, OpData)]
opsByOpcode = zip (map opCode ops) ops

opsByInstruction :: [(Instruction, OpData)]
opsByInstruction = zip (map opInstr ops) ops

fromOpcode :: Word8 -> Instruction
fromOpcode b = maybe UNDEFINED opInstr (lookup b opsByOpcode)

instrData :: Instruction -> OpData
instrData i = fromJust $ lookup i opsByInstruction

toOpcode :: Instruction -> Word8
toOpcode = opCode . instrData

isPush :: Instruction -> Bool
isPush = isJust . opPushLength . instrData

pushLength :: Instruction -> Integer
pushLength = fromMaybe undefined . opPushLength . instrData

popCount :: Instruction -> Int
popCount = opPopCount . instrData
