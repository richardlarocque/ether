{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

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

module Ethereum.Encoding.RLP
    (
     RLP(..),
     RLPSerialize,
     isItem,
     nullRLP,
     toRLP,
     fromRLP,
     getRLP,
     putRLP,
    )where

import           Control.Monad
import           Data.Bits
import qualified Data.ByteString as B
import           Data.LargeWord
import           Data.List
import           Data.Serialize
import           Data.Word
import           Ethereum.Common

-- | The basic structure of RLP serialization format.
data RLP = Group [RLP]
         | Item B.ByteString
           deriving (Show, Eq)

isItem :: RLP -> Bool
isItem (Item _) = True
isItem _        = False

-- | A class for types that can be serialized as RLP.
class RLPSerialize a where
    toRLP :: a -> RLP
    fromRLP :: RLP -> Maybe a

instance RLPSerialize Integer where
    toRLP = Item . encodeScalar
    fromRLP (Item i) = decodeScalar i
    fromRLP _ = Nothing

instance RLPSerialize Word256 where
    toRLP = Item . encode256be
    fromRLP (Item i) = Just $ decode256be i
    fromRLP _ = Nothing

instance RLPSerialize B.ByteString where
    toRLP = Item
    fromRLP (Item i) = Just i
    fromRLP _ = Nothing

putRLP :: (RLPSerialize a) => a -> Put
putRLP = put . toRLP

getRLP :: (RLPSerialize a) => Get a
getRLP = do x <- get
            maybe (fail "RLP decode failed") return (fromRLP x)

instance Serialize RLP where
    put (Group s) = putSequence (mapM_ put s)
    put (Item a)  = putArray a
    get = do b <- get :: Get Word8
             case b of
               _ | b <  128 -> return $ Item $ B.singleton b
               _ | b <= 183 ->
                    liftM Item $ getByteString $ fromIntegral (b - 128)
               x | x <  192 ->
                    do yb <- getByteString $ fromIntegral (x - 183)
                       zl <- liftM fromIntegral $ decodeScalar yb
                       liftM Item $ getByteString zl
               _ | b <= 247 ->
                    do let sl = fromIntegral (b - 192)
                       liftM Group $ isolate sl getSequenceElements
               _            ->
                    do slb <- getByteString $ fromIntegral (b - 247)
                       sl <- liftM fromIntegral $ decodeScalar slb
                       liftM Group $ isolate sl getSequenceElements
        where getSequenceElements :: Get [RLP]
              getSequenceElements =
                  do r <- remaining
                     if r == 0
                     then return []
                     else do x <- get :: Get RLP
                             xs <- getSequenceElements
                             return (x:xs)

encodeScalar :: (Integral a, Bits a) => a -> B.ByteString
encodeScalar i = B.pack $ reverse $
                 map fromIntegral $
                 unfoldr (\x -> if x == 0
                                then Nothing
                                else Just (x, x `shiftR` 8)) i

decodeScalar :: MonadPlus m => B.ByteString -> m Integer
decodeScalar bs | B.null bs = return 0
decodeScalar bs =
    when (B.head bs == 0) mzero
    >> return (fromNByteBigEndian (B.length bs) bs)

putArray ::  B.ByteString -> Put
putArray bs = case bs of
        _ | B.length bs == 1 && B.head bs < 128 -> putWord8 (B.head bs)
        _ | B.length bs < 56 ->
                do putWord8 (fromIntegral $ 128 + B.length bs)
                   putByteString bs
        _ ->
                do putWord8 (fromIntegral $ 183 + B.length (encodeScalar $ B.length bs))
                   putByteString (encodeScalar $ B.length bs)
                   putByteString bs

putSequenceHeader :: Int -> Put
putSequenceHeader len =
        if len < 56
           then putWord8 (fromIntegral $ 192 + len)
           else do putWord8 (fromIntegral $ 247 + B.length (encodeScalar len))
                   putByteString (encodeScalar len)

putSequenceBytes :: B.ByteString -> Put
putSequenceBytes lb =
        do putSequenceHeader (B.length lb)
           putByteString lb

putSequence ::  Put -> Put
putSequence p1 = putSequenceBytes $ runPut p1

nullRLP :: RLP
nullRLP = Item B.empty
