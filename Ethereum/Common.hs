module Ethereum.Common where

import           Data.Bits
import qualified Data.ByteString as B
import           Data.LargeWord

toNByteBigEndian :: (Integral a, Bits a) => Int -> a -> B.ByteString
toNByteBigEndian n i = B.pack $ reverse $ take n $
                       map fromIntegral $ iterate (`shiftR` 8) i

-- Translates a ByteString into a (possibly large) integral.
-- Forgiving version: Accepts short ByteStrings.
fromNByteBigEndian :: (Show a, Integral a, Bits a) => Int -> B.ByteString -> a
fromNByteBigEndian n bs =
    case n - B.length bs of
      badPadLen | badPadLen < 0 -> error "Invalid input length"
      padLen -> fromNByteBigEndian' n $ B.replicate padLen 0 `B.append` bs

-- Translates a ByteString into a (possibly large) integral.
-- Strict version: Fails if input is not precisely the right length.
fromNByteBigEndian' :: (Show a, Integral a, Bits a) => Int -> B.ByteString -> a
fromNByteBigEndian' n bytes | B.length bytes /= n = error "Invalid input length"
fromNByteBigEndian' n bytes =
    let bs = (map fromIntegral $ B.unpack bytes)
        shifts = map (8*) [n-1, n-2 .. 0]
        posBytes = zipWith shiftL bs shifts
        merged = foldl1 (.|.) posBytes
    in merged

decode256be :: B.ByteString -> Word256
decode256be = fromNByteBigEndian 32

encode256be :: Word256 -> B.ByteString
encode256be = toNByteBigEndian 32

ceilDiv :: (Integral a) => a -> a -> a
ceilDiv x d = (x + d - 1) `div` d
