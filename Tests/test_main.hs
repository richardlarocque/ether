
module Main where

import Test.Framework
import qualified Tests.HUnit.Account as Account
import qualified Tests.HUnit.EVM as EVM
import qualified Tests.HUnit.RLP as RLP
import qualified Tests.HUnit.HexPrefix as HexPrefix
import qualified Tests.HUnit.Trie as Trie
import qualified Tests.HUnit.Transaction as Transaction
import qualified Tests.HUnit.Integration as Integration

tests :: [Test.Framework.Test]
tests = [
        testGroup "Account" Account.tests,
        testGroup "EVM" EVM.tests,
        testGroup "RLP" RLP.tests,
        testGroup "HexPrefix" HexPrefix.tests,
        testGroup "Trie" Trie.tests,
        testGroup "Transaction" Transaction.tests,
        testGroup "Integration" Integration.tests
        ]

main ::  IO ()
main = defaultMain tests
