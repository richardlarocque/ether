module Ethereum.Crypto.Pubkey where

import           Control.Monad
import           Crypto.Secp256k1           as S
import           Data.Bits
import qualified Data.ByteString            as B
import           Data.LargeWord
import           Data.Maybe
import           Data.Serialize
import           Ethereum.Common
import           Ethereum.Crypto.Hash
import           Ethereum.State.Address
import           Ethereum.State.Transaction

data PrivateKey = Priv S.SecretKey
                  deriving (Show, Eq)

asPrivateKey :: Word256 -> Either String PrivateKey
asPrivateKey = liftM Priv . S.initSecretKey . encode256be

privateToAddress :: PrivateKey -> Address
privateToAddress (Priv priv) =
    pubkeyToAddress $ fromJust $ S.createPublicKey priv

-- TODO(spec): Why is B.tail necessary?
-- PyEthereum does it this way, but I don't know why.
pubkeyToAddress :: S.PublicKey -> Address
pubkeyToAddress pub =
    A $ fromIntegral $ (mask .&.) $ hashAsWord $ B.tail $ runPut $ put pub
    where mask = (1 `shiftL` (20*8)) - 1

signature :: Transaction -> (S.CompactSignature, Int)
signature (T _ _ _ _ _ w r s) =
  vrsToSignature (fromIntegral w, fromIntegral r, fromIntegral s)

vrsToSignature :: (Int, Word256, Word256) -> (S.CompactSignature, Int)
vrsToSignature (v, r, s) =
    let sigBytes = toNByteBigEndian 32 r `B.append` toNByteBigEndian 32 s
        compactSig = case runGet get sigBytes of
                       Left _ -> error "Parse failed"
                       Right x -> x
        rId = case v of
                -- FIXME: This first case shouldn't be supported...
                x | x >= 27 && x <= 30 -> x - 27
                x | x >= 0  && x <= 4  -> x
                _ -> error "Bad RecoveryID"
    in (compactSig, rId)

transactionHash :: Transaction -> B.ByteString
transactionHash t = hashAsBytes $ runPut $ put $ unsignedTransactionRLP t

transactionSender :: Transaction -> Maybe Address
transactionSender t =
    do let (cSig, rId) = signature t
       let h = transactionHash t
       pub <- S.recoverPublic h cSig rId
       return $ pubkeyToAddress pub

-- Works because successful recovery implies valid signature.
-- FIXME: Except that the upstream secp library seems to not do this.
-- TODO: A better implementation should be possible...
isSignatureValid :: Transaction -> Bool
isSignatureValid = isJust . transactionSender

signTransaction :: PrivateKey -> Transaction -> Maybe (Integer, Integer, Integer)
signTransaction (Priv pr) t =
    do let h = transactionHash t
       (cSig, rId) <- S.signCompact h pr
       let cSigBytes = runPut $ put cSig
       let (rBytes, sBytes) = B.splitAt 32 cSigBytes
       let (r, s) = (fromNByteBigEndian 32 rBytes, fromNByteBigEndian 32 sBytes)
       return (fromIntegral rId, r, s)
