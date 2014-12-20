module Ethereum.State.Address(
        Address(..),
        fromAddress,
        addressAsKey,
        zeroAddress,
        generateAddress) where

import           Data.Bits
import           Data.ByteString       as B
import           Data.LargeWord
import           Data.Serialize
import           Ethereum.Common
import           Ethereum.Crypto.Hash
import           Ethereum.Encoding.RLP

data Address = A Word160 deriving (Show, Eq)

instance RLPSerialize Address where
    toRLP (A a) = (Item . encode160be) a
    fromRLP (Item a) = Just $ (A . fromIntegral . decode160be) a
    fromRLP _ = Nothing

-- Addresses are 160 bits
fromAddress :: Integral a => Address -> a
fromAddress (A w) = fromIntegral w

fromHash :: Word256 -> Address
fromHash h = A $ fromIntegral $ fromIntegral (maxBound :: Word160) .&. h

-- TODO: Should this be a scalar, or Word160 serialization?
addressAsKey :: Address -> B.ByteString
addressAsKey = runPut . putRLP

zeroAddress :: Address
zeroAddress = A 0

-- | Equation 52: Generate an address from sender and its nonce.
-- | Unlike the recommendation in the text below the definition,
-- | we account things differently so we don't need to decrement
-- | the nonce by 1.
generateAddress :: Address -> Integer -> Address
generateAddress a n = fromHash $ hashAsWord $
                      runPut $ put (Group [ toRLP a, toRLP n ])

encode160be :: Word160 -> B.ByteString
encode160be = toNByteBigEndian 20

decode160be :: B.ByteString -> Word160
decode160be = fromNByteBigEndian 20
