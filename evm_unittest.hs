module Main where

import EVM
import Test.HUnit

simpleProgram :: [Instruction] -> ExecutionEnvironment
simpleProgram instructions =
  EE { owner=Address,
       sender=Address,
       gasPrice=5,
       input=[],
       execCause=Address,
       value=500000,
       code=instructions };

runCodeTest c v = TestCase $ assert $ (runVM (simpleProgram c)) ==  v

-- TODO: Infinite loop into out of gas.

tests = TestList [ 
  "invalidInstruction" ~: runCodeTest [INVALID] (Left InvalidInstruction),
  "stackUnderflow" ~: runCodeTest [ADD] (Left StackUnderflow),
  "stop" ~: runCodeTest [STOP] (Right []),
  "suicide" ~: runCodeTest [SUICIDE] (Right []) ]

main = runTestTT tests
