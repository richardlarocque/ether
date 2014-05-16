
module Main where

import Test.Framework
import qualified Tests.HUnit.EVM as EVM
import qualified Tests.HUnit.RLP as RLP
import qualified Tests.HUnit.HexPrefix as HexPrefix

tests :: [Test.Framework.Test]
tests = [
        testGroup "EVM" EVM.tests,
        testGroup "RLP" RLP.tests,
        testGroup "HexPrefix" HexPrefix.tests
        ]

main ::  IO ()
main = defaultMain tests
