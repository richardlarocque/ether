module Ethereum.Main where

import Crypto.Random
import Data.LargeWord
import Ethereum.State.Transaction
import Ethereum.Storage.Context

import qualified Data.ByteString as B

main :: IO ()
main = do
        pool <- createEntropyPool
        let cprg = cprgCreate pool :: SystemRNG

        let context = initContext

        let pr = 1234 :: Word256

        let cc1 = initContractCreation cprg pr 0 10 1 1000 B.empty

        print "Hello"
