module Ethereum.Crypto where

-- I have no idea what I'm doing here.  I am not a cryptographer.  The
-- libraries I'm using and random number generators they rely on probably
-- weren't written by cryptographers either.  Do not trust this.

import           Control.Applicative
import           Control.Monad
import           Crypto.Hash
import           Crypto.PubKey.ECC.ECDSA
import           Crypto.Random
import           Crypto.Types.PubKey.ECC
import           Data.Byteable
import qualified Data.ByteString         as B
import           Data.LargeWord
import           Data.Serialize
import           Ethereum.Common
import           Ethereum.Encoding.RLP
import           Ethereum.State.Address

-- TODO: This is where we would store the private key
data PrivateAccount = PrivateAccount Address

-- TODO: Don't use this helper ever.
ignoreFailure :: Either a b -> b
ignoreFailure (Right r) = r

addressFromPriv :: PrivateAccount -> Address
addressFromPriv (PrivateAccount a) = a

makePrivateAccount :: Word160 -> PrivateAccount
makePrivateAccount w = PrivateAccount (A w)

nonSig :: PrivateAccount -> TSignature
nonSig (PrivateAccount a) = NonSig a

-- FIXME: This is completely wrong.
data TSignature = TSignature B.ByteString Integer Integer
                | NonSig Address
                deriving (Show, Eq)

putSignature :: TSignature -> Put
putSignature (TSignature pub r s) = do putArray pub
                                       putScalar r
                                       putScalar s
putSignature (NonSig a) = do putScalar 0; putAddress a

getSignature :: Get TSignature
getSignature = getNoSig <|> getTSig
        where getNoSig = do v <- getScalar
                            unless (v == 0) (fail "not a NonSig")
                            a <- getAddress
                            return $ NonSig a
              getTSig = do pub <- getArray
                           r <- getScalar
                           s <- getScalar
                           return $ TSignature pub r s

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
        runPut $ do { put a; put b }

publicFromBytes :: B.ByteString -> PublicKey
publicFromBytes bs =
        ignoreFailure $ runGet getPub $ bs
        where getPub = do a <- get
                          b <- get
                          return $ PublicKey curve (Point a b)

publicAsAddress :: B.ByteString -> Address
publicAsAddress pu = A $ decode160be $ B.drop 44 $ pu

signTransaction :: CPRG g => g -> PrivateKey -> B.ByteString -> TSignature
signTransaction cprg pk bs =
        let (s, _) = sign cprg pk hashBytesToBytes bs
        in TSignature (publicToBytes $ privToPublic pk) (sign_r s) (sign_s s)

senderAddress :: TSignature -> Address
senderAddress (TSignature pub _ _) = publicAsAddress pub
senderAddress (NonSig a) = a

verifyTSig :: B.ByteString -> TSignature -> Bool
verifyTSig _ (NonSig _) = True
verifyTSig _ _ = False
