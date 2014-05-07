module Main where

import Test.Framework
import Test.Framework.Providers.HUnit

import Test.HUnit

import Ethereum.EVM.InstructionSet
import Ethereum.EVM.VM
import Ethereum.SimpleTypes

simpleProgram :: [Instruction] -> ExecutionEnvironment
simpleProgram instructions =
  EE { owner=Address,
       sender=Address,
       gasPrice=5,
       input=[],
       execCause=Address,
       value=500000,
       code=instructions };

runCodeTest :: [Instruction] -> Either RunTimeError MemSlice -> Assertion
runCodeTest c v = assert $ (runVM (simpleProgram c)) ==  v

-- TODO: Infinite loop into out of gas.
-- TODO: Suicide test needs a stack argument for some reason.

tests :: [Test.Framework.Test]
tests = [ testGroup "Halts and Exceptions" [ 
  testCase "invalidInstruction" $ runCodeTest [INVALID] (Left InvalidInstruction),
  testCase "stackUnderflow" $ runCodeTest [ADD] (Left StackUnderflow),
  testCase "stop" $ runCodeTest [STOP] (Right []) ] ]

main ::  IO ()
main = defaultMain tests
