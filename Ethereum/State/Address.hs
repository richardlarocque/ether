module Ethereum.State.Address(
        Address(..),
        putAddress,
        getAddress,
        fromAddress,
        addressAsKey,
        zeroAddress,
        generateAddress) where

import Control.Monad
import Data.ByteString as B
import Data.ByteString.Lazy as L
import Data.LargeWord
import Data.Binary
import Data.Binary.Put
import Data.Bits
import Ethereum.Common
import Ethereum.Encoding.RLP

data Address = A Word160 deriving (Show, Eq)

-- Addresses are 160 bits
fromAddress :: Integral a => Address -> a
fromAddress (A w) = fromIntegral w

fromHash :: Word256 -> Address
fromHash h = A $ fromIntegral $ fromIntegral (maxBound :: Word160) .&. h

addressAsKey :: Address -> B.ByteString
addressAsKey = asBE . (fromAddress :: Address -> Word160)

putAddress :: Address -> Put
putAddress = putScalar . fromAddress

getAddress :: Get Address
getAddress = liftM (A . fromIntegral) getScalar 

zeroAddress :: Address
zeroAddress = A 0

-- | Equation 52: Generate an address from sender and its nonce.
-- | See text below its definition for an explanation of the -1.
generateAddress :: Address -> Integer -> Address
generateAddress a n =
        let seed = runPut $ putSequence $ do { putAddress a; putScalar (n-1) }
        in fromHash $ hashBytes $ L.toStrict seed
