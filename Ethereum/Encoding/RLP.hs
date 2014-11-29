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
        _ | B.null bs -> putWord8 0
        _ | B.length bs == 1 && B.head bs < 128 -> putWord8 (B.head bs)
        _ | B.length bs < 56 ->
                do putWord8 (fromIntegral $ 128 + B.length bs)
                   putByteString bs
        _ ->
                do putWord8 (fromIntegral $ 183 + B.length (asBE $ B.length bs))
                   putByteString (asBE $ B.length bs)
                   putByteString bs

getArrayHeader ::  Get Integer
getArrayHeader = do
        b <- lookAhead getWord8
        case b of
                _  | b < 128   -> return 1
                _  | b <= 183  ->
                        do skip 1
                           return $ fromIntegral b - 128
                _  | b <= 192 ->
                        do skip 1
                           ls <- getByteString (fromIntegral b - 183)
                           unBE ls
                _ -> fail "Not paresable as array"

getArray ::  Get B.ByteString
getArray =
    do b <- getWord8
       case b of
         0            -> return B.empty
         _ | b <  128 -> return $ B.singleton b
         _ | b <= 183 -> getByteString (fromIntegral (b-128))
         _ | b <= 192 ->
               do ls <- getByteString (fromIntegral (b-183)) >>= unBE
                  getByteString $ fromIntegral ls
         _            -> fail "Invalid non-sequence header"

putScalar ::  Integer -> Put
putScalar = putArray . asBE

putScalar256 :: Word256 -> Put
putScalar256 = putScalar . fromIntegral

getScalar ::  Get Integer
getScalar = getArray >>= unBE

getScalar256 ::  Get Word256
getScalar256 = liftM fromIntegral getScalar

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
                      len <- unBE ls
                      return $ fromIntegral len

getSequenceBytes :: Get B.ByteString
getSequenceBytes = getSequence $ getBytes =<< remaining

getWord8s :: Integral a => a -> Get [Word8]
getWord8s x = replicateM (fromIntegral x) get
