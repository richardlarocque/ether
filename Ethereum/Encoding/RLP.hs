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

import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.LargeWord
import Ethereum.Common
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

putArray ::  B.ByteString -> Put
putArray bs = case bs of
        _ | B.length bs == 1 && B.head bs < 128 -> putWord8 (B.head bs)
        _ | B.length bs < 56 ->
                do putWord8 (fromIntegral $ 128 + B.length bs)
                   putByteString bs
        _ ->
                do putWord8 (fromIntegral $ 183 + (B.length (asBE $ B.length bs)))
                   putByteString (asBE $ B.length bs)
                   putByteString bs

getArrayHeader ::  Get Integer
getArrayHeader = do
        b <- lookAhead $ getWord8
        case b of
                _  | b < 128   -> return 1
                _  | b <= 183  -> do
                        skip 1
                        return $ (fromIntegral b) - 128
                _  | b <= 192 -> do
                        skip 1
                        ls <- getByteString ((fromIntegral b) - 183)
                        len <- unBE ls
                        return len
                _ -> fail "Not paresable as array"

getArray ::  Get B.ByteString
getArray = do len <- getArrayHeader
              getByteString (fromIntegral len)

putScalar ::  Integer -> Put
putScalar = (putArray . asBE)

putScalar256 :: Word256 -> Put
putScalar256 = putScalar . fromIntegral

getScalar ::  Get Integer
getScalar = do getArray >>= unBE

getScalar256 ::  Get Word256
getScalar256 = (liftM fromIntegral) getScalar

putSequenceHeader :: Integral a => a -> Put
putSequenceHeader len =
        do if len < 56
              then putWord8 (fromIntegral $ 192 + len)
              else do putWord8 (fromIntegral $ 247 + (B.length (asBE $ len)))
                      putByteString (asBE $ len)

putSequenceBytes :: B.ByteString -> Put
putSequenceBytes lb =
        do putSequenceHeader (B.length lb)
           putByteString lb

putListAsSequence :: (a -> Put) -> [a] -> Put
putListAsSequence p xs = putSequence $ mapM_ p xs

getListAsSequence :: Get a -> Get [a]
getListAsSequence g = getListAsSequence' []
        where getListAsSequence' us = do done <- isEmpty
                                         if done
                                            then return (reverse us)
                                            else do u <- g
                                                    getListAsSequence' (u:us)

putSequence ::  Put -> Put
putSequence p1 = putSequenceBytes $ L.toStrict $ (runPut p1)

getSequence :: Get a -> Get a
getSequence g1 =
        do l <- getSequenceHeader
           isolate l g1

getSequenceHeader ::  Get Int
getSequenceHeader =
        do b <- get :: Get Word8
           if b <= 247
              then return $ (fromIntegral b) - 192
              else do ls <- getByteString ((fromIntegral b) - 247)
                      len <- unBE ls
                      return $ fromIntegral len

getSequenceBytes :: Get B.ByteString
getSequenceBytes = (liftM L.toStrict) $ getSequence getRemainingLazyByteString

getWord8s :: Integral a => a -> Get [Word8]
getWord8s x = replicateM (fromIntegral x) get
