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
    A $ fromIntegral $ (mask .&.) $ hashPut $ put pub
    where mask = (1 `shiftL` 20) - 1

signature :: Transaction -> (S.CompactSignature, Int)
signature (T _ _ _ _ _ w r s) =
    let sigBytes = toNByteBigEndian 4 r `B.append` toNByteBigEndian 4 s
        compactSig = case runGet get sigBytes of
                       Left _ -> error "Parse failed"
                       Right x -> x
        rId = case w of
                x | x >= 27 && x <= 30 -> x
                x | x >= 0  && x <= 4  -> x + 27
                _ -> error "Bad RecoveryID"
    in (compactSig, fromInteger rId)

transactionSender :: Transaction -> Maybe Address
transactionSender t =
    do let (cSig, rId) = signature t
       let msg = runPut $ putUnsignedTransaction t
       pub <- S.recoverPublicC msg cSig rId
       return $ pubkeyToAddress pub

-- Works because successful recovery implies valid signature.
-- TODO: A better implementation should be possible...
isSignatureValid :: Transaction -> Bool
isSignatureValid = isJust . transactionSender

-- makePrivateAccount :: Word160 -> PrivateAccount
-- makePrivateAccount w = PrivateAccount (A w)

-- hashBytesToBytes :: B.ByteString -> B.ByteString
-- hashBytesToBytes bs = Data.Byteable.toBytes (hash bs :: Digest SHA3_256)

-- privToPublic :: PrivateKey -> PublicKey
-- privToPublic PrivateKey{private_curve=c, private_d=d} = toPublicKey $ KeyPair c (Point 1 2) d

-- publicToBytes :: PublicKey -> B.ByteString
-- publicToBytes PublicKey { public_q=(Point a b) } =
--         runPut $ do { put a; put b }

-- publicFromBytes :: B.ByteString -> PublicKey
-- publicFromBytes bs =
--         ignoreFailure $ runGet getPub $ bs
--         where getPub = do a <- get
--                           b <- get
--                           return $ PublicKey curve (Point a b)

-- publicAsAddress :: B.ByteString -> Address
-- publicAsAddress pu = A $ decode160be $ B.drop 44 $ pu
--
-- signTransaction :: CPRG g => g -> PrivateKey -> B.ByteString -> TSignature
-- signTransaction cprg pk bs =
--         let (s, _) = sign cprg pk hashBytesToBytes bs
--         in TSignature (publicToBytes $ privToPublic pk) (sign_r s) (sign_s s)
--
--senderAddress :: TSignature -> Address
--senderAddress (TSignature pub _ _) = publicAsAddress pub
--senderAddress (NonSig a) = a
