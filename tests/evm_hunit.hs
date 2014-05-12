module Main where

import qualified Data.Vector as V
import Data.Word
import Data.Bits
import Data.LargeWord
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Ethereum.EVM.ExecutionEnvironment
import Ethereum.EVM.InstructionSet as E
import Ethereum.EVM.VM
import Ethereum.SimpleTypes

data Assemble = I Instruction
              | D Word8
              | P1 Word8
              | P32 Word256

compile :: [Assemble] -> V.Vector Word8
compile bs = V.fromList (concatMap compile' bs)
        where compile' b = case b of
                I i     -> [toOpcode i]
                D d     -> [d]
                P1 v    -> (toOpcode PUSH1):[v]
                P32 v   -> (toOpcode PUSH32):((V.toList.toBytes) v)

simpleProgram :: [Assemble] -> ExecutionEnvironment
simpleProgram instructions =
  EE { address=Address,
       origin=Address,
       gasPrice=5,
       input=emptyMemSlice,
       caller=Address,
       value=500000,
       code=compile instructions };

runCodeTest :: [Assemble] -> Either RunTimeError MemSlice -> Assertion
runCodeTest c v = assert $ (execute (simpleProgram c)) ==  v

unOpTest ::  Instruction -> Word256 -> Word256 -> Test.Framework.Test
unOpTest op a v = testCase name $ runUnOpTest op a v
        where name = show op ++ " " ++ show a

runUnOpTest op a e = Right (toBytes e) @=? (execute unOpTestWrapper)
        where unOpTestWrapper =
                simpleProgram
                [ P1 32         -- Arguments to RETURN
                , P1 0

                , P32 a         -- Argument to 'i'
                , I op

                , P1 0          -- Argument to MSTORE

                , I MSTORE
                , I RETURN
                ]

binOpTest ::  Instruction -> Word256 -> Word256 -> Word256 -> Test.Framework.Test
binOpTest op a1 a2 v = testCase name $ runBinOpTest op a1 a2 v
        where name = show a1 ++ " " ++ show op ++ " " ++ show a2

runBinOpTest ::  Instruction -> Word256 -> Word256 -> Word256 -> Assertion
runBinOpTest op a1 a2 e = Right (toBytes e) @=? (execute binOpTestWrapper) 
        where binOpTestWrapper =
                simpleProgram
                [ P1 32         -- Arguments to RETURN
                , P1 0

                , P32 a2        -- Arguments to 'i'
                , P32 a1
                , I op

                , P1 0          -- Argument to MSTORE

                , I MSTORE
                , I RETURN
                ]

-- TODO: Infinite loop into out of gas.
-- TODO: Suicide test needs a stack argument for some reason.
-- TODO: Genericize test cases.
--       Should be able to output them to file or something.

tests :: [Test.Framework.Test]
tests = [
        testGroup "Util" [
                testCase "fromBytes 1" $ 1 @=? (fromBytes (V.fromList [1])),
                testCase "fromBytes 256" $ 256 @=? (fromBytes (V.fromList [1, 0])),
                testCase "toBytes 1" $ 1 @=? ((fromBytes.toBytes) 1),
                testCase "toBytes 256" $ 256 @=? ((fromBytes.toBytes) 256)
                ],

        testGroup "Halts and Exceptions" [ 
                testCase "invalidInstruction" $ runCodeTest [D 0xfa] (Left InvalidInstruction),
                testCase "stackUnderflow" $ runCodeTest [I ADD] (Left StackUnderflow),
                testCase "stop" $ runCodeTest [I STOP] (Right emptyMemSlice)
                ],
        testGroup "Unary Operations" [
                unOpTest NEG    5 (twosComp 5),
                unOpTest NEG    0 0,
                unOpTest NEG    (twosComp 256) 256
                -- unOpTest NOT
                ],
        testGroup "Binary Operations" [
                binOpTest ADD   5 3 8,
                binOpTest MUL   5 3 15,
                binOpTest SUB   5 3 2,
                binOpTest DIV   5 3 1,
                binOpTest MOD   5 3 2,

                binOpTest E.LT  5 3 0,
                binOpTest E.LT  3 5 1,
                binOpTest E.GT  5 3 1,
                binOpTest E.GT  3 5 0,
                binOpTest E.EQ  5 3 0,
                binOpTest E.EQ  5 5 1,

                binOpTest AND   5 3 1,
                binOpTest OR    5 3 7,
                binOpTest XOR   5 3 6,
                binOpTest BYTE  5 3 0
                ]
        ]

twosComp :: Word256 -> Word256
twosComp = (1+).complement

main ::  IO ()
main = defaultMain tests
