module Tests.Upstream(ioGroup) where

import           Control.Monad
import           Test.Tasty
import           Tests.Upstream.Common
import qualified Tests.Upstream.Trie

ioTests :: [IO (Either String TestTree)]
ioTests = [ Tests.Upstream.Trie.tests ]

ioGroup :: IO (Either String TestTree)
ioGroup = liftM (groupDataTests "Upstream") $ sequence ioTests
