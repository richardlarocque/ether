module Tests.Upstream.VM(tests) where

import           Control.Monad
import qualified Data.ByteString               as B
import           Ethereum.EVM.BlockEnvironment
import           Ethereum.State.Account
import           Ethereum.State.Address
import           Ethereum.Storage.Context
import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.Upstream.Common
import           Text.JSON

testPath :: String
testPath = "VMTests/"

mkPath :: String -> String
mkPath = (++) (testDataRoot ++ testPath)

tests :: IO (Either String TestTree)
tests = liftM (groupDataTests "VM") $ sequence [
  readTestsFromFile "vmtests" (mkPath "vmtests.json")
  (liftM makeTest . parseVMTest)]

type ExecParams = (Address, Address, Address,
                   B.ByteString,
                   Integer, Integer, Integer,
                   B.ByteString,
                   Integer)

data VMTest = VMTest {
  env     :: BlockEnvironment,
  exec    :: ExecParams,

  pre     :: [(Address, Account)],
  post    :: [(Address, Account)],

  postGas :: Integer,
  out     :: B.ByteString
}

parseVMTest :: JSValue -> Maybe VMTest
parseVMTest val =
  do obj <- asObject val
     let assoc = fromJSObject obj
     _ <- lookup "callcreates" assoc
     env <- parseBlockEnv =<< lookup "env" assoc
     exec <- parseExec =<< lookup "exec" assoc
     gas <- parseInteger =<< lookup "gas" assoc
     _ <- lookup "logs" assoc
     out <- parseSimpleType =<< lookup "out" assoc
     post <- parseAccounts =<< lookup "post" assoc
     pre <- parseAccounts =<< lookup "pre" assoc
     return $ VMTest env exec pre post gas out

parseBlockEnv :: JSValue -> Maybe BlockEnvironment
parseBlockEnv val =
  do obj <- asObject val
     let assoc = fromJSObject obj
     return BE
       `ap` (parseAddress =<< lookup "currentCoinbase" assoc)
       `ap` (parseInteger =<< lookup "currentDifficulty" assoc)
       `ap` (parseInteger =<< lookup "currentGasLimit" assoc)
       `ap` (parseInteger =<< lookup "currentNumber" assoc)
       `ap` (parseInteger =<< lookup "currentTimestamp" assoc)
       `ap` (parseWord256 =<< lookup "previousHash" assoc)

parseExec :: JSValue -> Maybe ExecParams
parseExec val =
  do assoc <- liftM fromJSObject $ asObject val
     r <- parseAddress =<< lookup "address" assoc
     s <- parseAddress =<< lookup "caller" assoc
     o <- parseAddress =<< lookup "originator" assoc
     code <- parseSimpleType =<< lookup "code" assoc
     d <- parseSimpleType =<< lookup "data" assoc
     g <- parseInteger =<< lookup "gas" assoc
     p <- parseInteger =<< lookup "gasPrice" assoc
     v <- parseInteger =<< lookup "value" assoc
     return (s, o, r, code, g, p, v, d, 0)

parseAccounts :: JSValue -> Maybe [(Address, Account)]
parseAccounts val =
  do assoc <- liftM fromJSObject $ asObject val
     mapM parseEntry assoc
  where parseEntry (addrStr, accObj) =
          do addr <- parseAddress' addrStr
             acc <- parseAccount accObj
             return (addr, acc)

parseAccount :: JSValue -> Maybe Account
parseAccount val =
  do assoc <- liftM fromJSObject $ asObject val
     return Account
       `ap` (parseInteger =<< lookup "nonce" assoc)
       `ap` (parseInteger =<< lookup "balance" assoc)
  -- FIXME: These last two are wrong.
       `ap` return nullStateRoot
       `ap` return NullCodeHash

makeTest :: VMTest -> Assertion
makeTest = undefined
