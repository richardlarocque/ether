module Ethereum.Common where

import Crypto.Hash
import Data.Binary
import Data.Binary.Put
import Data.Bits
import Data.Byteable
import Data.Word.Odd
import Data.LargeWord
import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

hashBytes :: B.ByteString -> Word256
hashBytes bs = (decode . L.fromStrict . Data.Byteable.toBytes) (hash bs :: Digest SHA3_256)

hashLazyBytes :: L.ByteString -> Word256
hashLazyBytes = hashBytes . L.toStrict

hashPut :: Put -> Word256
hashPut = hashBytes . L.toStrict . runPut

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

asBE :: Integral a => a -> B.ByteString
asBE x | x < 0 = undefined
asBE x = B.pack $ reverse $ unfoldr (\v ->
        if v == 0
           then Nothing
           else Just (fromIntegral $ v `mod` 256, v `div` 256)) x

unBE :: Monad m => B.ByteString -> m Integer
unBE bs = case bs of
        _ | B.length bs == 0 -> return 0
        _ | B.head bs == 0 -> fail "Unexpected leading zero(es)"
        _ -> return $ B.foldl' (\x y -> x * 256 + (fromIntegral y)) 0 bs
