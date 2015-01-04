module Tests.Upstream.KeyAddress(tests) where

import           Control.Monad
import           Crypto.Secp256k1       as S
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B8
import           Data.LargeWord
import           Data.Maybe
import           Ethereum.Common
import           Ethereum.Crypto.Hash
import           Ethereum.Crypto.Pubkey
import           Ethereum.State.Address
import           Test.Tasty
import           Test.Tasty.HUnit
import           Tests.Upstream.Common
import           Text.JSON

import           Debug.Trace

-- FIXME: These are failing for unknown reasons.
-- Not even the privkey to address conversion is working...
-- On the other hand, it seems none of the other clients are even running them.

testPath :: String
testPath = "BasicTests/"

mkPath :: String -> String
mkPath = (++) (testDataRoot ++ testPath)

tests :: IO (Either String TestTree)
tests = liftM (groupDataTests "KeyAddr") $ sequence [
  readTestsFromFile "KeyAddr" (mkPath "keyaddrtest.json")
  (liftM makeTest . parseKeyAddrTest)]

parseKeyAddrTest :: JSValue -> Maybe (B.ByteString, PrivateKey, Address, (S.CompactSignature, Int))
parseKeyAddrTest val =
  do obj <- asObject val
     let assoc = fromJSObject obj
     seed <- lookup "seed" assoc >>= asString
     key0 <- lookup "key" assoc >>= parseWord256
     key <- either (const Nothing) Just (asPrivateKey key0)
     addr <- lookup "addr" assoc >>= parseAddress
     sigEmpty <- lookup "sig_of_emptystring" assoc >>= parseSignature
     return (B8.pack seed, key, addr, sigEmpty)

parseAddress :: JSValue -> Maybe Address
parseAddress val = liftM (A . decode160be) $ parseHexString val

parseSignature :: JSValue -> Maybe (S.CompactSignature, Int)
parseSignature val =
  do assoc <- liftM fromJSObject $ asObject val
     v <- parseInteger =<< lookup "v" assoc
     r <- parseInteger =<< lookup "r" assoc
     s <- parseInteger =<< lookup "s" assoc
     return $ vrsToSignature (fromIntegral v, fromIntegral r, fromIntegral s)

decode160be :: B.ByteString -> Word160
decode160be = fromNByteBigEndian 20

makeTest :: (B.ByteString, PrivateKey, Address, (S.CompactSignature, Int)) -> Assertion
makeTest (seed, key, addr, (cSig, rId)) =
  do Right key @=? asPrivateKey (hashAsWord seed)
     addr @=? privateToAddress key
     let emptyHash = hashAsBytes B.empty
     let recovered = S.recoverPublic emptyHash cSig rId
     assertBool "KeyFromSig" $ isJust recovered
     --Just addr @=? liftM pubkeyToAddress recovered
