module Ethereum.Main where

import           Crypto.Random
import           Ethereum.Crypto.Pubkey
import           Ethereum.State.Account     as A
import           Ethereum.State.Transaction as T
import           Ethereum.Storage.Context

import qualified Data.ByteString            as B

main :: IO ()
main = do
        pool <- createEntropyPool
        let cprg = cprgCreate pool :: SystemRNG

        let c0 = initContext

        let pr = makePrivateAccount 1234
        let addr = addressFromPriv pr
        let acc = Account 0 90000 nullStateRoot NullCodeHash
        let c1 = updateAccount c0 (addr, acc)

        let cc1 = initContractCreation cprg pr (A.nonce acc) 10 1 1000 B.empty

        print $ verifyTransaction c1 cc1

        print "Hello"

verifyTransaction ::  Context -> Transaction -> Bool
verifyTransaction c t =
        let addr = (sender t)
        in case getAccount c addr of
                Nothing -> False
                Just acc -> isTransactionValid t acc
