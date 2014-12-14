module Ethereum.Common where

import           Crypto.Hash
import           Data.Bits
import           Data.Byteable
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.LargeWord
import           Data.List
import           Data.Serialize
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

encode256be :: Word256 -> B.ByteString
encode256be = toNByteBigEndian 32

decode256be :: B.ByteString -> Word256
decode256be = fromNByteBigEndian 32

encode160be :: Word160 -> B.ByteString
encode160be = toNByteBigEndian 20

decode160be :: B.ByteString -> Word160
decode160be = fromNByteBigEndian 20

hashBytes :: B.ByteString -> Word256
hashBytes bs = (decode256be . toBytes) (hash bs :: Digest SHA3_256)

hashLazyBytes :: L.ByteString -> Word256
hashLazyBytes = hashBytes . L.toStrict

hashPut :: Put -> Word256
hashPut = hashBytes . runPut

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

asBE :: Integral a => a -> B.ByteString
asBE x | x < 0 = undefined
asBE x = B.pack $ reverse $ unfoldr (\v ->
        if v == 0
           then Nothing
           else Just (fromIntegral $ v `mod` 256, v `div` 256)) x

ceilDiv :: (Integral a) => a -> a -> a
ceilDiv x d = (x + d - 1) `div` d
