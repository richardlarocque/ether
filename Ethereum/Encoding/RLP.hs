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
import Data.List
import qualified Data.ByteString.Lazy as L

putArray ::  [Word8] -> Put
putArray [b] | b < 128           = putWord8 b
putArray bs  | length bs < 56    =
        do putWord8 (fromIntegral $ 128 + length bs)
           mapM_ putWord8 bs
putArray bs                      =
        do putWord8 (fromIntegral $ 183 + (length (asBE $ length bs)))
           mapM_ putWord8 (asBE $ length bs)
           mapM_ putWord8 bs

getArray ::  Get [Word8]
getArray = do b <- get :: Get Word8
              case b of
                      v  | v < 128   -> return [v]
                      l  | l <= 183  -> getWord8s (l - 128)
                      ll             ->
                              do ls <- getWord8s (ll - 183)
                                 len <- unBE ls
                                 getWord8s len

putScalar ::  Integer -> Put
putScalar = (putArray . asBE)

putScalarI :: Integral a => a -> Put
putScalarI = putScalar.fromIntegral

getScalar ::  Get Integer
getScalar = do getArray >>= unBE

putSequence ::  Put -> Put
putSequence p1 =
        let encodings = runPut p1
            len = L.length encodings
        in do if len < 56
                 then do putWord8 (fromIntegral $ 192 + len)
                         putLazyByteString encodings
                 else do putWord8 (fromIntegral $ 247 + (length (asBE $ len)))
                         mapM_ putWord8 (asBE $ len)
                         putLazyByteString encodings

getSequence :: Get a -> Get a
getSequence g1 =
        do bs <- getSequence'
           case runGetOrFail g1 bs of
                   (Left (_,_,msg))       -> fail msg
                   (Right (_,_,a))        -> return a

getSequence' ::  Get L.ByteString
getSequence' = liftM L.pack $
        do b <- get :: Get Word8
           if b <= 247
              then getWord8s (b - 192)
              else do ls <- getWord8s (b - 247)
                      len <- unBE ls
                      getWord8s len


getWord8s :: Integral a => a -> Get [Word8]
getWord8s x = replicateM (fromIntegral x) get

asBE :: Integral a => a -> [Word8]
asBE = reverse . (unfoldr (\v ->
        if v == 0
           then Nothing
           else Just (fromIntegral $ v `mod` 256, v `div` 256)))

unBE :: Monad m => [Word8] -> m Integer
unBE bs = case bs of 
        0:_ -> fail "Unexpected leading zero(es)"
        x -> return $ unBE' x

unBE' :: [Word8] -> Integer
unBE' = foldl' (\x y -> x * 256 + (fromIntegral y)) 0
