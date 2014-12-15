module Ethereum.Crypto where

import           Control.Monad
import           Crypto.Secp256k1           as S
import           Data.Bits
import qualified Data.ByteString            as B
import           Data.LargeWord
import           Data.Maybe
import           Data.Serialize
import           Ethereum.Common
import           Ethereum.State.Address
import           Ethereum.State.Transaction

import           Debug.Trace

data PrivateKey = Priv S.SecretKey

-- TODO: Don't use this helper ever.
ignoreFailure :: Either a b -> b
ignoreFailure (Right r) = r

asPrivateKey :: Word256 -> Either String PrivateKey
asPrivateKey = liftM Priv . S.initSecretKey . encode256be

privateToAddress :: PrivateKey -> Address
privateToAddress (Priv priv) =
    pubkeyToAddress $ fromJust $ S.createPublicKeyC priv

pubkeyToAddress :: S.CompressedPublicKey -> Address
pubkeyToAddress pub =
    A $ fromIntegral $ (mask .&.) $ hashAsWord $ runPut $ put pub
    where mask = (1 `shiftL` 20) - 1

signature :: Transaction -> (S.CompactSignature, Int)
signature (T _ _ _ _ _ w r s) =
    let sigBytes = toNByteBigEndian 32 r `B.append` toNByteBigEndian 32 s
        compactSig = case runGet get sigBytes of
                       Left _ -> error "Parse failed"
                       Right x -> x
        rId = case w of
                -- FIXME: This first case shouldn't be supported...
                x | x >= 27 && x <= 30 -> x - 27
                x | x >= 0  && x <= 4  -> x
                _ -> error "Bad RecoveryID"
    in traceShow (compactSig, rId) (compactSig, fromInteger rId)

transactionSender :: Transaction -> Maybe Address
transactionSender t =
    do let (cSig, rId) = signature t
       let h = hashAsBytes $ runPut $ putUnsignedTransaction t
       pub <- S.recoverPublicC h cSig rId
       return $ pubkeyToAddress pub

-- Works because successful recovery implies valid signature.
-- TODO: A better implementation should be possible...
isSignatureValid :: Transaction -> Bool
isSignatureValid = isJust . transactionSender

-- FIXME: THIS IS UNACCEPTABLE HACK!!!!!!
badNonce :: S.Nonce
(Right badNonce) = S.initNonce (B.replicate 32 0xab)

signTransaction :: PrivateKey -> Transaction -> Maybe (Integer, Integer, Integer)
signTransaction (Priv pr) t =
    do let h = hashAsBytes $ runPut $ putUnsignedTransaction t
       (cSig, rId) <- S.signCompact h pr badNonce
       let cSigBytes = runPut $ put cSig
       let (rBytes, sBytes) = B.splitAt 32 cSigBytes
       let (r, s) = (fromNByteBigEndian 32 rBytes, fromNByteBigEndian 32 sBytes)
       return (fromIntegral rId, r, s)
