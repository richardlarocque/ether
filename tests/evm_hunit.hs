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

p32 :: Integral a => a -> [Word8]
p32 x = let x' = (fromIntegral x) :: Word256
        in op PUSH32 ++ (V.toList.toBytes) x'

p1 :: Word8 -> [Word8]
p1 x = op PUSH1 ++ [x]

op :: Instruction -> [Word8]
op o = [toOpcode o]

unOp :: Instruction -> [Word8] -> [Word8]
unOp i a = a ++ (op i)

binOp :: Instruction -> [Word8] -> [Word8] -> [Word8]
binOp i a1 a2 = a2 ++ a1 ++ (op i)

triOp :: Instruction -> [Word8] -> [Word8] -> [Word8] -> [Word8]
triOp i a1 a2 a3 = a3 ++ a2 ++ a1 ++ (op i)

-- Stores the given data in memory
-- A bit convoluted, but it saves us the trouble of setting up a symbol table.
memLiteral :: Word256 -> [Word8]  -> [Word8]
memLiteral memOffset literal =
        let len = length literal
            lenArg = p32 len
            codeAddrArg = binOp ADD (op PC) (p32 72)
            memAddrArg = p32 memOffset
            jmpAddrArg = binOp ADD (op PC) (p32 (len + 3))
        in (triOp CODECOPY memAddrArg codeAddrArg lenArg)
           ++ (unOp JUMP jmpAddrArg)
           ++ literal

-- Stores the top of the stack at memory zero.
basicMstore :: [Word8]  -> [Word8]
basicMstore xs = xs ++ p1 0 ++ [ toOpcode MSTORE ]

-- Returns the top element of the stack.
basicReturn :: [Word8]  -> [Word8]
basicReturn xs = p1 32 ++ p1 0 ++ (basicMstore xs) ++ [ toOpcode RETURN ]

simpleProgram :: [Word8] -> ExecutionEnvironment
simpleProgram is =
  EE { address=Address,
       origin=Address,
       gasPrice=5,
       input=emptyMemSlice,
       caller=Address,
       value=500000,
       code= V.fromList is };

simpleExec ::  [Word8] -> Either RunTimeError MemSlice
simpleExec = execute.simpleProgram

runCodeTest :: [Word8] -> Either RunTimeError MemSlice -> Assertion
runCodeTest c v = assert $ (execute (simpleProgram c)) ==  v

unOpTest ::  Instruction -> Word256 -> Word256 -> Test.Framework.Test
unOpTest op a v = testCase name (expect @=? result)
        where name = show op ++ " " ++ show a
              expect = Right (toBytes v)
              result = simpleExec ( basicReturn $ unOp op (p32 a) )

binOpTest ::  Instruction -> Word256 -> Word256 -> Word256 -> Test.Framework.Test
binOpTest op a1 a2 v = testCase name (expect @=? result)
        where name = show a1 ++ " " ++ show op ++ " " ++ show a2
              expect = Right (toBytes v)
              result = simpleExec ( basicReturn $ binOp op (p32 a1) (p32 a2) )

sha3Test ::   String -> [Word8] -> Word256 -> Test.Framework.Test
sha3Test name val e = testCase name (expect @=? result)
        where expect = Right (toBytes e)
              result = simpleExec $ basicReturn (putMem ++ hashMem)
              memAddr = 100
              memLen = length val
              putMem = memLiteral memAddr val
              hashMem = binOp SHA3 (p32 memAddr) (p32 (length val))

-- TODO: Infinite loop into out of gas.
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
                testCase "invalidInstruction" $ runCodeTest [0xfa] (Left InvalidInstruction),
                testCase "stackUnderflow" $ runCodeTest (op ADD) (Left StackUnderflow),
                testCase "stop" $ runCodeTest (op STOP) (Right emptyMemSlice)
                ],
        testGroup "Unary Operations" [
                unOpTest NEG    5 (twosComp 5),
                unOpTest NEG    0 0,
                unOpTest NEG    (twosComp 256) 256,

                unOpTest NOT    0 1,
                unOpTest NOT    1 0,
                unOpTest NOT    3 0
                ],
        testGroup "Binary Operations" [
                binOpTest ADD   5 3 8,
                binOpTest MUL   5 3 15,
                binOpTest SUB   5 3 2,

                binOpTest DIV   5 3 1,

                binOpTest SDIV  5 3 1,
                binOpTest SDIV  (twosComp 5) 3 (twosComp 1),
                binOpTest SDIV  (twosComp 5) (twosComp 3) 1,

                binOpTest MOD   5 3 2,
                binOpTest SMOD  5 3 2,
                binOpTest SMOD  (twosComp 5) 3 (twosComp 2),
                binOpTest SMOD  (twosComp 5) (twosComp 3) 2,

                binOpTest EXP   5 3 125,
                binOpTest EXP   4 0 1,

                binOpTest E.LT  5 3 0,
                binOpTest E.LT  3 5 1,
                binOpTest E.LT  (twosComp 1) 1 0,

                binOpTest SLT   5 3 0,
                binOpTest SLT   3 5 1,
                binOpTest SLT   (twosComp 1) 1 1,
                binOpTest SLT   (twosComp 3) (twosComp 2) 1,
                binOpTest SLT   1 1 0,

                binOpTest E.GT  5 3 1,
                binOpTest E.GT  3 5 0,
                binOpTest E.GT  (twosComp 1) 1 1,

                binOpTest SGT   5 3 1,
                binOpTest SGT   3 5 0,
                binOpTest SGT   (twosComp 1) 1 0,
                binOpTest SGT   (twosComp 3) (twosComp 2) 0,
                binOpTest SGT   1 1 0,

                binOpTest E.EQ  5 3 0,
                binOpTest E.EQ  5 5 1,

                binOpTest AND   5 3 1,
                binOpTest OR    5 3 7,
                binOpTest XOR   5 3 6,
                binOpTest BYTE  5 3 0
                ],
        testGroup "sha3" [
                -- According to Wikipedia, this is the hash for Kekkak-256.
                sha3Test "()" [] 89477152217924674838424037953991966239322087453347756267410168184682657981552
                ]
        ]

twosComp :: Word256 -> Word256
twosComp = (1+).complement

main ::  IO ()
main = defaultMain tests
