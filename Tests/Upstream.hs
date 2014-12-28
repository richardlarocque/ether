module Tests.Upstream(ioGroup) where

import           Control.Monad
import           Test.Tasty
import           Tests.Upstream.Common
import qualified Tests.Upstream.HexEncode
import qualified Tests.Upstream.Trie
import qualified Tests.Upstream.RLP

ioTests :: [IO (Either String TestTree)]
ioTests = [ Tests.Upstream.Trie.tests,
            Tests.Upstream.HexEncode.tests,
            Tests.Upstream.RLP.tests ]

ioGroup :: IO (Either String TestTree)
ioGroup = liftM (groupDataTests "Upstream") $ sequence ioTests
