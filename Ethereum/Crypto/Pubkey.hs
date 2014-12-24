module Ethereum.Crypto.Pubkey(
  isSignatureValid,
  transactionSender,
  signTransaction,
  PrivateKey
  ) where

import           Control.Monad
-- import           Crypto.Secp256k1           as S
import           Data.Bits
import qualified Data.ByteString            as B
import           Data.LargeWord
import           Data.Maybe
import           Data.Serialize
import           Ethereum.Common
import           Ethereum.Crypto.Hash
import           Ethereum.State.Address
import           Ethereum.State.Transaction

import Data.Serialize

--data PrivateKey = Priv S.SecretKey
data PrivateKey = FakePrivKey

data CompressedPublicKey = FakePubKey

data CompactSignature = FakeSignature

data Nonce = FakeNonce

initSecretKey :: B.ByteString -> PrivateKey
initSecretKey _ = FakePrivKey

createPublicKey :: PrivateKey -> CompressedPublicKey
createPublicKey _ = FakePubKey

initNonce :: B.ByteString -> Nonce
initNonce _ = FakeNonce

asPrivateKey :: Word256 -> Either String PrivateKey
asPrivateKey _ = return FakePrivKey

privateToAddress :: PrivateKey -> Address
privateToAddress _ = zeroAddress

pubkeyToAddress :: CompressedPublicKey -> Address
pubkeyToAddress _ =
    A $ fromIntegral $ (mask .&.) $ hashAsWord $ B.empty
    where mask = (1 `shiftL` 20) - 1

signature :: Transaction -> (CompactSignature, Int)
signature (T _ _ _ _ _ w _ _) =
    let compactSig = FakeSignature
        rId = case w of
                -- FIXME: This first case shouldn't be supported...
                x | x >= 27 && x <= 30 -> x - 27
                x | x >= 0  && x <= 4  -> x
                _ -> error "Bad RecoveryID"
    in (compactSig, fromInteger rId)

transactionHash :: Transaction -> B.ByteString
transactionHash t = hashAsBytes $ runPut $ put $ unsignedTransactionRLP t

transactionSender :: Transaction -> Maybe Address
transactionSender _ = Just zeroAddress

-- Works because successful recovery implies valid signature.
-- TODO: A better implementation should be possible...
isSignatureValid :: Transaction -> Bool
isSignatureValid = isJust . transactionSender

-- FIXME: THIS IS UNACCEPTABLE HACK!!!!!!
badNonce :: Nonce
(Right badNonce) = Right FakeNonce

signTransaction :: PrivateKey -> Transaction -> Maybe (Integer, Integer, Integer)
signTransaction _ t = Just (0, 0, 0)
