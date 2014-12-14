module Main where

import           Crypto.Secp256k1
import           Test.Tasty
import qualified Tests.HUnit.Account     as Account
import qualified Tests.HUnit.Block       as Block
import qualified Tests.HUnit.EVM         as EVM
import qualified Tests.HUnit.HexPrefix   as HexPrefix
import qualified Tests.HUnit.Integration as Integration
import qualified Tests.HUnit.Interop     as Interop
import qualified Tests.HUnit.RLP         as RLP
import qualified Tests.HUnit.Transaction as Transaction
import qualified Tests.HUnit.Trie        as Trie

tests :: TestTree
tests = testGroup "HUnit" [
        Account.tests,
        EVM.tests,
        RLP.tests,
        HexPrefix.tests,
        Trie.tests,
        Transaction.tests,
        Integration.tests,
        Block.tests,
        Interop.tests
        ]

main ::  IO ()
main = withSecp256k1Initialized $ defaultMain tests
