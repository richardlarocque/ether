module Ethereum.Crypto where

-- I have no idea what I'm doing here.  I am not a cryptographer.  The
-- libraries I'm using and random number generators they rely on probably
-- weren't written by cryptographers either.  Do not trust this.

import           Control.Applicative
import           Control.Monad
import           Crypto.Hash
import           Crypto.Random
import           Crypto.Secp256k1       as S
import           Data.Bits
import           Data.Byteable
import qualified Data.ByteString        as B
import           Data.LargeWord
import           Data.Serialize
import           Ethereum.Common
import           Ethereum.Encoding.RLP
import           Ethereum.State.Address

data PrivateAccount = PrivateAccount S.SecretKey

-- TODO: Don't use this helper ever.
ignoreFailure :: Either a b -> b
ignoreFailure (Right r) = r

addressFromPriv :: PrivateAccount -> Address
addressFromPriv (PrivateAccount a) =
    A $ fromIntegral $ (mask .&.) $ hashPut $ put a
    where mask = (1 `shiftL` 20) - 1

-- makePrivateAccount :: Word160 -> PrivateAccount
-- makePrivateAccount w = PrivateAccount (A w)

hashBytesToBytes :: B.ByteString -> B.ByteString
hashBytesToBytes bs = Data.Byteable.toBytes (hash bs :: Digest SHA3_256)

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

verifyTSig :: B.ByteString -> TSignature -> Bool
verifyTSig _ (NonSig _) = True
verifyTSig _ _ = False

sig :: Transaction -> (Integer, Integer)
sig (T _ _ _ _ _ v r s) = ((r `shiftL` 32) .&. s, v)

transactionSender :: Transaction -> Maybe Address
transactionSender t =
    let (sig,recov) = vrs t
        msg = putUnsignedTransaction t
        S.recoverCompactC toSign sig recov
