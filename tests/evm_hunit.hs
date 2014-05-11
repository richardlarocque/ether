module Main where

import Ethereum.EVM.ExecutionEnvironment
import Ethereum.EVM.InstructionSet
import Ethereum.EVM.VM
import Ethereum.SimpleTypes

import qualified Data.Vector as V
import Data.Word
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

data CodeByte = I Instruction
              | D Word8

compile :: [CodeByte] -> V.Vector Word8
compile bs = V.fromList (map compileByte bs)
        where compileByte b = case b of
                I i -> toOpcode i
                D v -> v

simpleProgram :: [CodeByte] -> ExecutionEnvironment
simpleProgram instructions =
  EE { address=Address,
       origin=Address,
       gasPrice=5,
       input=emptyMemSlice,
       caller=Address,
       value=500000,
       code=compile instructions };

runCodeTest :: [CodeByte] -> Either RunTimeError MemSlice -> Assertion
runCodeTest c v = assert $ (execute (simpleProgram c)) ==  v

runBinOpTest :: Instruction -> Either RunTimeError MemSlice -> Assertion
runBinOpTest op v = assert $ (execute (binOpTestWrapper op)) == v
        where binOpTestWrapper i =
                simpleProgram
                [ I PUSH1   -- Arguments to RETURN
                , D 1
                , I PUSH1
                , D 0

                , I PUSH1   -- Arguments to 'i'
                , D 3
                , I PUSH1
                , D 5
                , I i

                , I PUSH1   -- Arguments to MSTORE
                , D 0

                , I MSTORE
                , I RETURN
                ]

-- TODO: Infinite loop into out of gas.
-- TODO: Suicide test needs a stack argument for some reason.

tests :: [Test.Framework.Test]
tests = [
        testGroup "Halts and Exceptions" [ 
                testCase "invalidInstruction" $ runCodeTest [D 0xfa] (Left InvalidInstruction),
                testCase "stackUnderflow" $ runCodeTest [I ADD] (Left StackUnderflow),
                testCase "stop" $ runCodeTest [I STOP] (Right emptyMemSlice)
                ],
        testGroup "Binary Operations" [
                testCase "ADD" $ runBinOpTest ADD (Right (V.fromList [8])),
                testCase "SUB" $ runBinOpTest SUB (Right (V.fromList [2]))
                ]
        ]


main ::  IO ()
main = defaultMain tests
