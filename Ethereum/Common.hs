module Ethereum.Common where

import Crypto.Hash
import Data.Binary
import Data.Bits
import Data.Byteable
import Data.Word.Odd
import Data.LargeWord
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

hashBytes :: B.ByteString -> Word256
hashBytes bs = (decode . L.fromStrict . Data.Byteable.toBytes) (hash bs :: Digest SHA3_256)

hashLazyBytes :: L.ByteString -> Word256
hashLazyBytes = hashBytes.L.toStrict

lowNibble  :: Word8 -> Word4
lowNibble x   = (fromIntegral $ 0x0f .&. x)

highNibble :: Word8 -> Word4
highNibble x  = (fromIntegral $ x `shiftR` 4)

toHigh :: Word4 -> Word8
toHigh = (16*).fromIntegral

toLow :: Word4 -> Word8
toLow = fromIntegral

nibbleize :: [Word8] -> [Word4]
nibbleize bs = map fromIntegral $ concatMap toNibbles bs
        where toNibbles b = [highNibble b, lowNibble b]
