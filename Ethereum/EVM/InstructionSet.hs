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
        toOpcode,
        isPush,
        pushLen) where

import Data.Word

-- | All the opcodes declared in Appendix G.2, plus INVALID as a placeholder.
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

{- 40s: Block Information -}
    | PREVHASH
    | COINBASE
    | TIMESTAMP
    | NUMBER
    | DIFFICULTY
    | GASLIMIT

{- 50s: Stack, Memory, Storage and Flow Operations -}
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

{- f0s: System operations -}
    | CREATE
    | CALL
    | RETURN
    | SUICIDE
    deriving (Show,Eq)

-- | Convert a byte to an instruction.
fromOpcode :: Word8 -> Maybe Instruction
{- 0s: Stop and Arithmetic Operations -}
fromOpcode 0x00 = Just STOP
fromOpcode 0x01 = Just ADD
fromOpcode 0x02 = Just MUL
fromOpcode 0x03 = Just SUB
fromOpcode 0x04 = Just DIV
fromOpcode 0x05 = Just SDIV
fromOpcode 0x06 = Just MOD
fromOpcode 0x07 = Just SMOD
fromOpcode 0x08 = Just EXP
fromOpcode 0x09 = Just NEG
fromOpcode 0x0a = Just Ethereum.EVM.InstructionSet.LT
fromOpcode 0x0b = Just Ethereum.EVM.InstructionSet.GT
fromOpcode 0x0c = Just SLT
fromOpcode 0x0d = Just SGT
fromOpcode 0x0e = Just Ethereum.EVM.InstructionSet.EQ
fromOpcode 0x0f = Just NOT
fromOpcode 0x10 = Just AND
fromOpcode 0x11 = Just OR
fromOpcode 0x12 = Just XOR
fromOpcode 0x13 = Just BYTE

{- 20s: SHA3 -}
fromOpcode 0x20 = Just SHA3

{- 30s: Environmental Information -}
fromOpcode 0x30 = Just ADDRESS
fromOpcode 0x31 = Just BALANCE
fromOpcode 0x32 = Just ORIGIN
fromOpcode 0x33 = Just CALLER
fromOpcode 0x34 = Just CALLVALUE
fromOpcode 0x35 = Just CALLDATALOAD
fromOpcode 0x36 = Just CALLDATASIZE
fromOpcode 0x37 = Just CALLDATACOPY
fromOpcode 0x38 = Just CODESIZE
fromOpcode 0x39 = Just CODECOPY
fromOpcode 0x3a = Just GASPRICE

{- 40s: Block Information -}
fromOpcode 0x40 = Just PREVHASH
fromOpcode 0x41 = Just COINBASE
fromOpcode 0x42 = Just TIMESTAMP
fromOpcode 0x43 = Just NUMBER
fromOpcode 0x44 = Just DIFFICULTY
fromOpcode 0x45 = Just GASLIMIT

{- 50s: Stack, Memory, Storage and Flow Operations -}
fromOpcode 0x50 = Just POP
fromOpcode 0x51 = Just DUP
fromOpcode 0x52 = Just SWAP
fromOpcode 0x53 = Just MLOAD
fromOpcode 0x54 = Just MSTORE
fromOpcode 0x55 = Just MSTORE8
fromOpcode 0x56 = Just SLOAD
fromOpcode 0x57 = Just SSTORE
fromOpcode 0x58 = Just JUMP
fromOpcode 0x59 = Just JUMPI
fromOpcode 0x5a = Just PC
fromOpcode 0x5b = Just MSIZE
fromOpcode 0x5c = Just GAS

{- 60s and 70s: Push Operations -}
fromOpcode 0x60 = Just PUSH1
fromOpcode 0x61 = Just PUSH2
fromOpcode 0x62 = Just PUSH3
fromOpcode 0x63 = Just PUSH4
fromOpcode 0x64 = Just PUSH5
fromOpcode 0x65 = Just PUSH6
fromOpcode 0x66 = Just PUSH7
fromOpcode 0x67 = Just PUSH8
fromOpcode 0x68 = Just PUSH9
fromOpcode 0x69 = Just PUSH10
fromOpcode 0x6a = Just PUSH11
fromOpcode 0x6b = Just PUSH12
fromOpcode 0x6c = Just PUSH13
fromOpcode 0x6d = Just PUSH14
fromOpcode 0x6e = Just PUSH15
fromOpcode 0x6f = Just PUSH16
fromOpcode 0x70 = Just PUSH17
fromOpcode 0x71 = Just PUSH18
fromOpcode 0x72 = Just PUSH19
fromOpcode 0x73 = Just PUSH20
fromOpcode 0x74 = Just PUSH21
fromOpcode 0x75 = Just PUSH22
fromOpcode 0x76 = Just PUSH23
fromOpcode 0x77 = Just PUSH24
fromOpcode 0x78 = Just PUSH25
fromOpcode 0x79 = Just PUSH26
fromOpcode 0x7a = Just PUSH27
fromOpcode 0x7b = Just PUSH28
fromOpcode 0x7c = Just PUSH29
fromOpcode 0x7d = Just PUSH30
fromOpcode 0x7e = Just PUSH31
fromOpcode 0x7f = Just PUSH32

{- f0s: System operations -}
fromOpcode 0xf0 = Just CREATE
fromOpcode 0xf1 = Just CALL
fromOpcode 0xf2 = Just RETURN
fromOpcode 0xff = Just SUICIDE

fromOpcode _    = Nothing

-- | Convert an instruction to a byte value
toOpcode :: Instruction -> Word8
{- 0s: Stop and Arithmetic Operations -}
toOpcode STOP   = 0x00
toOpcode ADD    = 0x01
toOpcode MUL    = 0x02
toOpcode SUB    = 0x03
toOpcode DIV    = 0x04
toOpcode SDIV   = 0x05
toOpcode MOD    = 0x06
toOpcode SMOD   = 0x07
toOpcode EXP    = 0x08
toOpcode NEG    = 0x09
toOpcode Ethereum.EVM.InstructionSet.LT = 0x0a
toOpcode Ethereum.EVM.InstructionSet.GT = 0x0b
toOpcode SLT    = 0x0c
toOpcode SGT    = 0x0d
toOpcode Ethereum.EVM.InstructionSet.EQ = 0x0e
toOpcode NOT    = 0x0f
toOpcode AND    = 0x10
toOpcode OR     = 0x11
toOpcode XOR    = 0x12
toOpcode BYTE   = 0x13

{- 20s: SHA3 -}
toOpcode SHA3   = 0x20

{- 30s: Environmental Information -}
toOpcode ADDRESS        = 0x30
toOpcode BALANCE        = 0x31
toOpcode ORIGIN         = 0x32
toOpcode CALLER         = 0x33
toOpcode CALLVALUE      = 0x34
toOpcode CALLDATALOAD   = 0x35
toOpcode CALLDATASIZE   = 0x36
toOpcode CALLDATACOPY   = 0x37
toOpcode CODESIZE       = 0x38
toOpcode CODECOPY       = 0x39
toOpcode GASPRICE       = 0x3a

{- 40s: Block Information -}
toOpcode PREVHASH       = 0x40
toOpcode COINBASE       = 0x41
toOpcode TIMESTAMP      = 0x42
toOpcode NUMBER         = 0x43
toOpcode DIFFICULTY     = 0x44
toOpcode GASLIMIT       = 0x45

{- 50s: Stack, Memory, Storage and Flow Operations -}
toOpcode POP            = 0x50
toOpcode DUP            = 0x51
toOpcode SWAP           = 0x52
toOpcode MLOAD          = 0x53
toOpcode MSTORE         = 0x54
toOpcode MSTORE8        = 0x55
toOpcode SLOAD          = 0x56
toOpcode SSTORE         = 0x57
toOpcode JUMP           = 0x58
toOpcode JUMPI          = 0x59
toOpcode PC             = 0x5a
toOpcode MSIZE          = 0x5b
toOpcode GAS            = 0x5c

{- 60s and 70s: Push Operations -}
toOpcode PUSH1          = 0x60
toOpcode PUSH2          = 0x61
toOpcode PUSH3          = 0x62
toOpcode PUSH4          = 0x63
toOpcode PUSH5          = 0x64
toOpcode PUSH6          = 0x65
toOpcode PUSH7          = 0x66
toOpcode PUSH8          = 0x67
toOpcode PUSH9          = 0x68
toOpcode PUSH10         = 0x69
toOpcode PUSH11         = 0x6a
toOpcode PUSH12         = 0x6b
toOpcode PUSH13         = 0x6c
toOpcode PUSH14         = 0x6d
toOpcode PUSH15         = 0x6e
toOpcode PUSH16         = 0x6f
toOpcode PUSH17         = 0x70
toOpcode PUSH18         = 0x71
toOpcode PUSH19         = 0x72
toOpcode PUSH20         = 0x73
toOpcode PUSH21         = 0x74
toOpcode PUSH22         = 0x75
toOpcode PUSH23         = 0x76
toOpcode PUSH24         = 0x77
toOpcode PUSH25         = 0x78
toOpcode PUSH26         = 0x79
toOpcode PUSH27         = 0x7a
toOpcode PUSH28         = 0x7b
toOpcode PUSH29         = 0x7c
toOpcode PUSH30         = 0x7d
toOpcode PUSH31         = 0x7e
toOpcode PUSH32         = 0x7f

{- f0s: System operations -}
toOpcode CREATE         = 0xf0
toOpcode CALL           = 0xf1
toOpcode RETURN         = 0xf2
toOpcode SUICIDE        = 0xff

isPush :: Instruction -> Bool
isPush i = toOpcode i >= toOpcode PUSH1 && toOpcode i <= toOpcode PUSH32

pushLen :: Instruction -> Integer
pushLen i = if not $ isPush i
               then undefined
               else fromIntegral $ 1 + toOpcode i - toOpcode PUSH1
