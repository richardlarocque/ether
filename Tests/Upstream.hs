module Tests.Upstream where

import           Test.Tasty
import Tests.Upstream.Common
import qualified Tests.Upstream.Trie

ioTests :: IO [Either String TestTree]
ioTests = [
 Tests.Upstream.Trie.tests
]

ioTree :: IO (Either String TestTree)
ioTree = groupDataTests "Upstream" ioTests
