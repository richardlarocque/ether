{- |
Module      :  Ethereum.Encoding.HexPrefix
Description :  Implementation of the Ethereum HexPrefix encoding
Copyright   :  (c) Richard Larocque
License     :  GPL-3.0+

Maintainer  :  richard.larocque@gmail.com
Stability   :  unstable
Portability :  non-portable (Unknown portability)

See Ethereum Yellow Paper, Proof-of-Concept V, Appendix D
-}

module Ethereum.Encoding.HexPrefix
    (
     HPArray(..),
     asHexPrefix,
     unHexPrefix,
     nibbleize
    ) where

import           Control.Monad
import           Data.Bits
import qualified Data.ByteString as B
import           Data.Serialize
import           Data.Word
import           Data.Word.Odd

data HPArray = HPArray [Word4] Bool
        deriving (Show,Eq)

asHexPrefix :: [Word4] -> Bool -> B.ByteString
asHexPrefix ns b = runPut $ putHexPrefixBytes ns b

unHexPrefix :: B.ByteString -> Either String HPArray
unHexPrefix = runGet $ do
  len <- remaining
  isolate (fromIntegral len) getHexPrefix

putHexPrefixBytes :: [Word4] -> Bool -> Put
putHexPrefixBytes ns b = do
        let ft = if b then 2 else 0 :: Word8
        let (b0, rest) =
                if (even.length) ns
                then (16 * ft       +  0                    ,      ns)
                else (16 * (ft + 1) + (fromIntegral.head) ns, tail ns)
        put b0
        mapM_ putWord8 (pairBytes rest)
        where pairBytes :: [Word4] -> [Word8]
              pairBytes (n1:n2:rest) = makeByte n1 n2 : pairBytes rest
              pairBytes [] = []
              pairBytes [_] = error "Unexpected unpair input"
              makeByte h l = toHigh h + toLow l

getHexPrefix :: Get HPArray
getHexPrefix = do
        b <- getWord8
        let ft = b `testBit` 5
        let isEven = not $ b `testBit` 4
        let prefix = if isEven
                        then []
                        else [lowNibble b]
        bs <- do r <- remaining
                 replicateM r getWord8
        return $ HPArray (prefix ++ nibbleize bs) ft

nibbleize :: [Word8] -> [Word4]
nibbleize bs = map fromIntegral $ concatMap toNibbles bs
        where toNibbles b = [highNibble b, lowNibble b]

lowNibble  :: Word8 -> Word4
lowNibble x   = fromIntegral $ 0x0f .&. x

highNibble :: Word8 -> Word4
highNibble x  = fromIntegral $ x `shiftR` 4

toHigh :: Word4 -> Word8
toHigh = (16*).fromIntegral

toLow :: Word4 -> Word8
toLow = fromIntegral
