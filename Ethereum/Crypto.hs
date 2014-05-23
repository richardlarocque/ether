module Ethereum.Crypto where

-- I have no idea what I'm doing here.  I am not a cryptographer.  The
-- libraries I'm using and random number generators they rely on probably
-- weren't written by cryptographers either.  Do not trust this.

import Crypto.Hash
import Crypto.PubKey.ECC.ECDSA
import Crypto.Random
import Crypto.Types.PubKey.ECC
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Byteable
import Data.LargeWord
import Ethereum.Encoding.RLP
import Ethereum.SimpleTypes

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- FIXME: This is completely wrong.
data TSignature = TSignature B.ByteString Integer Integer
                | NonSig

putSignature :: TSignature -> Put
putSignature (TSignature pub r s) = do putArray pub
                                       putScalar r
                                       putScalar s
putSignature (NonSig) = putScalar 0

curve :: Curve
curve = getCurveByName SEC_p256k1

hashBytesToBytes :: B.ByteString -> B.ByteString
hashBytesToBytes bs = Data.Byteable.toBytes (hash bs :: Digest SHA3_256)

makePrivateKey :: Word256 -> PrivateKey
makePrivateKey i = toPrivateKey $ KeyPair curve (Point 1 2) (fromIntegral i)

privToPublic :: PrivateKey -> PublicKey
privToPublic PrivateKey{private_curve=c, private_d=d} = toPublicKey $ KeyPair c (Point 1 2) d

publicToBytes :: PublicKey -> B.ByteString
publicToBytes PublicKey { public_q=(Point a b) } =
        L.toStrict $ runPut $ do { put a; put b }

publicFromBytes :: B.ByteString -> PublicKey
publicFromBytes bs =
        runGet getPub $ L.fromStrict bs
        where getPub = do a <- get
                          b <- get
                          return $ PublicKey curve (Point a b)

publicAsAddress :: B.ByteString -> Address
publicAsAddress pu = A $ runGet get $ L.fromStrict $ B.drop 44 $ pu

signTransaction :: CPRG g => g -> PrivateKey -> B.ByteString -> TSignature
signTransaction cprg pk bs =
        let (s, _) = sign cprg pk hashBytesToBytes bs
        in TSignature (publicToBytes $ privToPublic pk) (sign_r s) (sign_s s)

senderAddress :: TSignature -> Address
senderAddress (TSignature pub _ _) = publicAsAddress pub

verifyTSig :: B.ByteString -> TSignature -> Bool
verifyTSig bs (TSignature pub r s) =
        verify hashBytesToBytes (publicFromBytes pub) (Signature r s) bs
