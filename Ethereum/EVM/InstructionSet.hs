{- |
Module      :  Ethereum.EVM.InstructionSet
Description :  Type Declarations for Ethereum
Copyright   :  (c) Richard Larocque
License     :  GPL-3.0+

Maintainer  :  richard.larocque@gmail.com
Stability   :  unstable
Portability :  non-portable (Unknown portability)

Translation of Ethereum Yellow Paper, Proof-of-Concept V, Appendix G.2
-}

module Ethereum.EVM.InstructionSet(
  Instruction(..),
  fromOpcode,
  stackPopCount) where

import Data.Word

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

stackPopCount :: Instruction -> Int
stackPopCount STOP = 0
stackPopCount ADD = 2
stackPopCount MUL = 2
stackPopCount SUB = 2
stackPopCount DIV = 2
stackPopCount MOD = 2
stackPopCount EXP = 2
stackPopCount NEG = 1
stackPopCount RETURN = 2
stackPopCount SUICIDE = 1
stackPopCount INVALID = 0

fromOpcode :: Word8 -> Instruction
fromOpcode 0x00 = STOP
fromOpcode 0x01 = ADD
fromOpcode 0x02 = MUL
fromOpcode 0x03 = SUB
fromOpcode 0x04 = DIV
fromOpcode 0x05 = SDIV
fromOpcode 0x06 = MOD
fromOpcode 0x07 = SMOD
fromOpcode 0x08 = EXP
fromOpcode 0x09 = NEG
fromOpcode 0x0a = Ethereum.EVM.InstructionSet.LT
fromOpcode 0x0b = Ethereum.EVM.InstructionSet.GT
fromOpcode 0x0c = SLT
fromOpcode 0x0d = SGT
fromOpcode 0x0e = Ethereum.EVM.InstructionSet.EQ
fromOpcode 0x0f = NOT
fromOpcode 0x10 = AND
fromOpcode 0x11 = OR
fromOpcode 0x12 = XOR
fromOpcode 0x13 = BYTE

-- 20s: SHA3
fromOpcode 0x20 = SHA3

-- 30s: Environmental Information
fromOpcode 0x30 = ADDRESS
fromOpcode 0x31 = BALANCE
fromOpcode 0x32 = ORIGIN
fromOpcode 0x33 = CALLER
fromOpcode 0x34 = CALLVALUE
fromOpcode 0x35 = CALLDATALOAD
fromOpcode 0x36 = CALLDATASIZE
fromOpcode 0x37 = CALLDATACOPY
fromOpcode 0x38 = CODESIZE
fromOpcode 0x39 = CODECOPY
fromOpcode 0x3a = GASPRICE

-- 40s: Block Information
fromOpcode 0x40 = PREVHASH
fromOpcode 0x41 = COINBASE
fromOpcode 0x42 = TIMESTAMP
fromOpcode 0x43 = NUMBER
fromOpcode 0x44 = DIFFICULTY
fromOpcode 0x45 = GASLIMIT

-- 50s: Stack, Memory, Storage and Flow Operations
fromOpcode 0x50 = POP
fromOpcode 0x51 = DUP
fromOpcode 0x52 = SWAP
fromOpcode 0x53 = MLOAD
fromOpcode 0x54 = MSTORE
fromOpcode 0x55 = MSTORE8
fromOpcode 0x56 = SLOAD
fromOpcode 0x57 = SSTORE
fromOpcode 0x58 = JUMP
fromOpcode 0x59 = JUMPI
fromOpcode 0x5a = PC
fromOpcode 0x5b = MSIZE
fromOpcode 0x5c = GAS

-- 60s and 70s: Push Operations
fromOpcode 0x60 = PUSH1
fromOpcode 0x61 = PUSH2
fromOpcode 0x62 = PUSH3
fromOpcode 0x63 = PUSH4
fromOpcode 0x64 = PUSH5
fromOpcode 0x65 = PUSH6
fromOpcode 0x66 = PUSH7
fromOpcode 0x67 = PUSH8
fromOpcode 0x68 = PUSH9
fromOpcode 0x69 = PUSH10
fromOpcode 0x6a = PUSH11
fromOpcode 0x6b = PUSH12
fromOpcode 0x6c = PUSH13
fromOpcode 0x6d = PUSH14
fromOpcode 0x6e = PUSH15
fromOpcode 0x6f = PUSH16
fromOpcode 0x70 = PUSH17
fromOpcode 0x71 = PUSH18
fromOpcode 0x72 = PUSH19
fromOpcode 0x73 = PUSH20
fromOpcode 0x74 = PUSH21
fromOpcode 0x75 = PUSH22
fromOpcode 0x76 = PUSH23
fromOpcode 0x77 = PUSH24
fromOpcode 0x78 = PUSH25
fromOpcode 0x79 = PUSH26
fromOpcode 0x7a = PUSH27
fromOpcode 0x7b = PUSH28
fromOpcode 0x7c = PUSH29
fromOpcode 0x7d = PUSH30
fromOpcode 0x7e = PUSH31
fromOpcode 0x7f = PUSH32

-- f0s: System operations
fromOpcode 0xf0 = CREATE
fromOpcode 0xf1 = CALL
fromOpcode 0xf2 = RETURN

fromOpcode 0xff= SUICIDE

fromOpcode _ = INVALID -- pseudo-value
