module Tests.HUnit.EVM(tests) where

import           Data.Bits
import qualified Data.ByteString                   as B
import           Data.LargeWord
import           Data.Monoid
import           Data.Serialize
import           Data.Word
import           Test.Tasty
import           Test.Tasty.HUnit

import           Ethereum.Crypto.Hash
import           Ethereum.Encoding.RLP
import           Ethereum.EVM.ExecutionEnvironment
import           Ethereum.EVM.InstructionSet       as E
import           Ethereum.EVM.MachineState
import           Ethereum.EVM.VM
import           Ethereum.Lang.Ops                 as L
import           Ethereum.SimpleTypes
import           Ethereum.State.Account            hiding (stateRoot)
import           Ethereum.State.Address
import           Ethereum.State.Block
import           Ethereum.Storage.Context

-- Stores the top of the stack at memory zero.
basicMstore :: Builder  -> Builder
basicMstore = mstore (p1 0)

-- Returns the top element of the stack.
basicReturn :: Builder  -> Builder
basicReturn xs = basicMstore xs <> returnOp (p1 0) (p1 32)

ownerAddr ::  Address
ownerAddr = A 0xAAAA

ownerAccount :: Account
ownerAccount = Account 10 1337 nullStateRoot NullCodeHash

originAddr ::  Address
originAddr = A 0x0011

callerAddr ::  Address
callerAddr = A 0xCCCC

inputData :: B.ByteString
inputData = toBytes (1337 :: Word256)

callValue :: Ether
callValue = 5000000

gasPriceValue :: Ether
gasPriceValue = 1

testBlockHeader :: BlockHeader
testBlockHeader = BlockHeader {
        parentHash = 1234,
        unclesHash = hashAsWord $ runPut $ put $ Group [],
        coinbase = A 0xCBCB,
        stateRoot = rootHash testContext,
        transactionsTrie = rootHash initContext,
        difficulty = 2 ^ (22 :: Integer),
        timestamp = 1999,
        number = 42,
        minGasPrice = 1,
        gasLimit = 10000,
        gasUsed = 0,
        extraData = B.empty,
        blockNonce = 0
}

testExecutionEnv :: B.ByteString -> ExecutionEnvironment
testExecutionEnv x =
  EE { address=ownerAddr,
       origin=originAddr,
       gasPrice=gasPriceValue,
       input=inputData,
       caller=callerAddr,
       value=callValue,
       code=x,
       blockHeader=testBlockHeader }

runCodeTest :: Builder -> Termination -> Assertion
runCodeTest c v = v @=? simpleRun c

testContext :: Context
testContext =
        let c0 = initContext
            c1 = updateAccount c0 (ownerAddr, ownerAccount)
        in c1

simpleRun :: Builder -> Termination
simpleRun c =
        let ms = initWithGas 10000
            context = testContext
            ee = testExecutionEnv (compile c)
        in runUntilDone context ms ee

runUntilDone :: Context -> MachineState -> ExecutionEnvironment -> Termination
runUntilDone c ms ee = case runState execStep (c, ms, ee) of
        (Left t, _) -> t
        (Right _, (c', ms', ee')) -> runUntilDone c' ms' ee'

returnTest :: String -> Builder -> Word256 -> TestTree
returnTest name codes v = testCase name (expect @=? result)
        where expect = NormalHalt (toBytes v)
              result = simpleRun $ basicReturn codes

opTest :: Instruction -> Word256 -> TestTree
opTest o v = testCase name (expect @=? result)
        where name = show o
              expect = NormalHalt (toBytes v)
              result = simpleRun ( basicReturn $ op o )

unOpTest ::  Instruction -> Word256 -> Word256 -> TestTree
unOpTest o a v = testCase name (expect @=? result)
        where name = show o ++ " " ++ show a
              expect = NormalHalt (toBytes v)
              result = simpleRun ( basicReturn $ unOp o (p32 a) )

binOpTest ::  Instruction -> Word256 -> Word256 -> Word256 -> TestTree
binOpTest o a1 a2 v = testCase name (expect @=? result)
        where name = show a1 ++ " " ++ show o ++ " " ++ show a2
              expect = NormalHalt (toBytes v)
              result = simpleRun ( basicReturn $ binOp o (p32 a1) (p32 a2) )

sha3Test ::   String -> B.ByteString -> Word256 -> TestTree
sha3Test name val e = testCase name (expect @=? result)
        where expect = NormalHalt (toBytes e)
              result = simpleRun $ basicReturn (putMem <> hashMem)
              memAddr = 100
              memLen = B.length val
              putMem = memLiteral memAddr val
              hashMem = binOp SHA3 (p32 memAddr) (p32i memLen)

memTest :: TestName -> (Word8 -> Builder) -> Word256 -> TestTree
memTest name putMemFunc e = testCase name (expect @=? result)
        where expect = NormalHalt (toBytes e)
              result = simpleRun $ putMem <> binOp RETURN (p1 memAddr) (p1 32)
              memAddr = 100
              putMem = putMemFunc memAddr

-- TODO: Infinite loop into out of gas.
-- TODO: Genericize test cases.
--       Should be able to output them to file or something.

tests :: TestTree
tests = testGroup "EVM" [
        testGroup "Util" [
                testCase "fromBytes 1" $ 1 @=? fromBytes (B.pack [1]),
                testCase "fromBytes 256" $ 256 @=? fromBytes (B.pack [1, 0]),
                testCase "toBytes 1" $ 1 @=? (fromBytes.toBytes) 1,
                testCase "toBytes 256" $ 256 @=? (fromBytes.toBytes) 256,

                testCase "safeBrange in"   $ B.pack [2,3] @=?
                             safeBrange (1,2) (B.pack [1,2,3]),
                testCase "safeBrange out"  $ B.pack [0,0] @=?
                             safeBrange (5,2) (B.pack [1,2,3]),
                testCase "safeBrange edge" $ B.pack [3,0] @=?
                             safeBrange (2,2) (B.pack [1,2,3])
                ],

        testGroup "Halts and Exceptions" [
                testCase "invalidInstruction" $
                         runCodeTest (asOp 0xfa) InvalidInstruction,
                testCase "stackUnderflow" $
                         runCodeTest (op ADD) StackUnderflow,
                testCase "stop" $
                         runCodeTest (op STOP) (NormalHalt emptyMemSlice),
                -- FIXME: This does not halt.
                -- testCase "outOfGas step" $
                --          runCodeTest (p32 0 <> op JUMP) OutOfGasException,
                testCase "outOfGas mem" $
                         runCodeTest (binOp MSTORE (p32 1000000000) (p32 1))
                                     OutOfGasException
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
                sha3Test "()" B.empty 89477152217924674838424037953991966239322087453347756267410168184682657981552
                ],
        testGroup "environment" [
                opTest ADDRESS (fromAddress ownerAddr),
                opTest BALANCE (fromIntegral $ balance ownerAccount),
                opTest ORIGIN (fromAddress originAddr),
                opTest CALLER (fromAddress callerAddr),
                opTest CALLVALUE (fromInteger callValue),
                unOpTest CALLDATALOAD 0 (fromBytes inputData),
                opTest CALLDATASIZE ((fromIntegral.B.length) inputData),
                memTest (show CALLDATACOPY)
                        (\memAddr -> triOp CALLDATACOPY (p32i memAddr) (p1 0) (p1 32))
                        (fromBytes inputData),
                opTest CODESIZE 9,  -- FIXME: brittle.
                memTest (show CODECOPY)  -- FIXME: also brittle.
                        (\memAddr -> triOp CODECOPY (p32i memAddr) (p1 0) (p1 1))
                        (fromBytes $ B.pack $ toOpcode PUSH1 : replicate 31 0),
                opTest GASPRICE (fromEther gasPriceValue)
                ],
        testGroup "block" [
                opTest PREVHASH (parentHash testBlockHeader),
                opTest COINBASE (fromAddress $ coinbase testBlockHeader),
                opTest TIMESTAMP (fromIntegral $ timestamp testBlockHeader),
                opTest NUMBER (fromIntegral $ number testBlockHeader),
                opTest DIFFICULTY (fromIntegral $ difficulty testBlockHeader),
                opTest GASLIMIT (fromIntegral $ gasLimit testBlockHeader)
                ],
        testGroup "stack" [
                returnTest (show POP)  (p32 400 <> p32 300 <> op POP) 400,
                returnTest (show SWAP) (p32 400 <> p32 300 <> op SWAP <> op POP) 300,
                returnTest (show MLOAD)
                           (memLiteral 200 (toBytes 1234) <> unOp MLOAD (p32 200))
                           1234,
                memTest (show MSTORE)
                        (\memAddr -> binOp MSTORE (p1 memAddr) (p32 123))
                        123,
                memTest (show MSTORE8)
                        (\memAddr -> mstore8 (p1 memAddr) (p32 $ 0x100 + 132))
                        (132 `shiftL` (256 - 8)),
                returnTest "SLOAD none" (unOp SLOAD (p1 10)) 0,
                returnTest "SSTORE" (binOp SSTORE (p1 10) (p1 42) <> unOp SLOAD (p1 10)) 42,
                returnTest "JUMP" ((unOp JUMP (p1 4)) <> (op STOP) <> (p1 10)) 10,
                returnTest "JUMPI 0" ((binOp JUMPI (p1 6) (p1 0)) <> (op STOP) <> (p1 10)) 10,
                returnTest "JUMPI 1" ((binOp JUMPI (p32 0xDEAD) (p1 1)) <> (p1 10)) 10,
                returnTest "PC" ((unOp JUMP (p1 4)) <> (op STOP) <> (op PC)) 4,
                returnTest "MSIZE 0" msize 0,
                returnTest "MSIZE 32" (mstore8 (p1 32) (p1 10) <> msize) 2,
                returnTest "MSIZE 1023" (mstore8 (p32 1023) (p1 10) <> msize) 32,
                returnTest "MSIZE 1024" (mstore8 (p32 1024) (p1 10) <> msize) 33
                ]
        -- PUSH is implicitly tested quite well already.
        -- TODO: system operations
        ]

twosComp :: Word256 -> Word256
twosComp = (1+).complement
