module Ethereum.Common where

import           Crypto.Hash
import           Data.Bits
import           Data.Byteable
import qualified Data.ByteString as B
import           Data.LargeWord
import           Data.Word
import           Data.Word.Odd

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

hashAsWord :: B.ByteString -> Word256
hashAsWord = decode256be . hashAsBytes

hashAsBytes :: B.ByteString -> B.ByteString
hashAsBytes bs = toBytes (hash bs :: Digest SHA3_256)

lowNibble  :: Word8 -> Word4
lowNibble x   = fromIntegral $ 0x0f .&. x

highNibble :: Word8 -> Word4
highNibble x  = fromIntegral $ x `shiftR` 4

toHigh :: Word4 -> Word8
toHigh = (16*).fromIntegral

toLow :: Word4 -> Word8
toLow = fromIntegral

nibbleize :: [Word8] -> [Word4]
nibbleize bs = map fromIntegral $ concatMap toNibbles bs
        where toNibbles b = [highNibble b, lowNibble b]

ceilDiv :: (Integral a) => a -> a -> a
ceilDiv x d = (x + d - 1) `div` d
