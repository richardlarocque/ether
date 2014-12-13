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

encode256be :: Word256 -> B.ByteString
encode256be n = B.pack $ reverse $ take 32 $
                map fromIntegral $ iterate (`shiftR` 8) n

-- decode256be :: B.ByteString -> Word256
-- decode256be bytes = foldl iter 0 (B.unpack bytes)
--   where iter accum b = accum * 256 + fromIntegral b

decode256be :: B.ByteString -> Word256
decode256be bs =
    case 32 - B.length bs of
      badPadLen   | badPadLen < 0    -> error "Invalid input length"
      padLen -> decode256be' $ B.replicate padLen 0 `B.append` bs

decode256be' :: B.ByteString -> Word256
decode256be' bytes | B.length bytes /= 32 = error "Invalid input length"
decode256be' bytes =
    let bs = (map fromIntegral $ B.unpack bytes) :: [Word256]
        posBytes = zipWith shiftL bs [248,240..0] :: [Word256]
        merged = foldl1 (.|.) posBytes
    in merged

decode160be :: B.ByteString -> Word160
decode160be bytes = foldl iter 0 (B.unpack bytes)
  where iter accum b = accum * 256 + fromIntegral b

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
