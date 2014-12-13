{- |
Module      :  Ethereum.Encoding.RLP
Description :  Implementation of the Ethereum RLP encoding
Copyright   :  (c) Richard Larocque
License     :  GPL-3.0+

Maintainer  :  richard.larocque@gmail.com
Stability   :  unstable
Portability :  non-portable (Unknown portability)

See Ethereum Yellow Paper, Proof-of-Concept V, Appendix C
-}

module Ethereum.Encoding.RLP where

import           Control.Monad
import qualified Data.ByteString as B
import           Data.LargeWord
import           Data.Serialize
import           Data.Word
import           Ethereum.Common

putArray ::  B.ByteString -> Put
putArray bs = case bs of
        _ | B.length bs == 1 && B.head bs < 128 -> putWord8 (B.head bs)
        _ | B.length bs < 56 ->
                do putWord8 (fromIntegral $ 128 + B.length bs)
                   putByteString bs
        _ ->
                do putWord8 (fromIntegral $ 183 + B.length (asBE $ B.length bs))
                   putByteString (asBE $ B.length bs)
                   putByteString bs

getArray ::  Get B.ByteString
getArray =
    do b <- getWord8
       case b of
         _ | b <  128 -> return $ B.singleton b
         _ | b <= 183 -> getByteString (fromIntegral (b-128))
         _ | b <= 192 ->
               do ls <- getByteString (fromIntegral (b-183)) >>= decodeScalar
                  getByteString $ fromIntegral ls
         _            -> fail "Invalid non-sequence header"

-- Put and get integr values as scalars.
putScalar ::  Integer -> Put
putScalar = putArray . asBE

getScalar ::  Get Integer
getScalar = getArray >>= decodeScalar

-- FIXME: THIS COMMENT IS WRONG... Put and get Word256 as 32-byte arrays.
put256 :: Word256 -> Put
put256 = putScalar . fromIntegral

get256 ::  Get Word256
get256 = liftM (fromIntegral . decode256be) getArray

putSequenceHeader :: Integral a => a -> Put
putSequenceHeader len =
        if len < 56
           then putWord8 (fromIntegral $ 192 + len)
           else do putWord8 (fromIntegral $ 247 + B.length (asBE len))
                   putByteString (asBE len)

putSequenceBytes :: B.ByteString -> Put
putSequenceBytes lb =
        do putSequenceHeader (B.length lb)
           putByteString lb

putListAsSequence :: (a -> Put) -> [a] -> Put
putListAsSequence p xs = putSequence $ mapM_ p xs

getListAsSequence :: Get a -> Get [a]
getListAsSequence g = do len <- getSequenceHeader
                         isolate len (getListAsSequence' [])
        where getListAsSequence' us = do done <- isEmpty
                                         if done
                                            then return (reverse us)
                                            else do u <- g
                                                    getListAsSequence' (u:us)

putSequence ::  Put -> Put
putSequence p1 = putSequenceBytes $ runPut p1

getSequence :: Get a -> Get a
getSequence g1 =
        do l <- getSequenceHeader
           isolate l g1

getSequenceHeader ::  Get Int
getSequenceHeader =
        do b <- get :: Get Word8
           if b <= 247
              then return $ fromIntegral b - 192
              else do ls <- getByteString (fromIntegral b - 247)
                      len <- decodeScalar ls
                      return $ fromIntegral len

getSequenceBytes :: Get B.ByteString
getSequenceBytes = getSequence $ getBytes =<< remaining

getWord8s :: Integral a => a -> Get [Word8]
getWord8s x = replicateM (fromIntegral x) get

decodeScalar :: Monad m => B.ByteString -> m Integer
decodeScalar bs = case bs of
        _ | B.length bs == 0 -> return 0
        _ | B.head bs == 0 -> fail "Unexpected leading zero(es)"
        _ -> return $ B.foldl' (\x y -> x * 256 + fromIntegral y) 0 bs
