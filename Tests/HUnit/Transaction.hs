module Tests.HUnit.Transaction where

import Control.Monad
import Crypto.Random
import Data.LargeWord
import Ethereum.State.Transaction
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

roundTripTest :: (Show a, Eq a) => (a -> L.ByteString) -> (L.ByteString -> a) -> a -> Test.Framework.Test
roundTripTest e d x = testCase (show x) $ x @=? (d.e) x

-- serialization_tests :: [Test.Framework.Test]
-- serialization_tests = [
--         testGroup "Serialization" [


defaultContractCreation :: Word256 -> B.ByteString -> IO Transaction
defaultContractCreation k init = do
        pool <- createEntropyPool
        let cprg = cprgCreate pool :: SystemRNG
        return $ initContractCreation cprg k 1 1000 1 1000 init

-- Signatures: Completely fucked.
-- sign_tests :: [Test.Framework.Test]
-- sign_tests = [
--         testCase "Signature" $ do cc <- defaultContractCreation 10 B.empty
--                                   unless (isSignatureValid cc) (assertFailure "bad sig") ]

tests = sign_tests
