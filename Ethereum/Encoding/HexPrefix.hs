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

module Ethereum.Encoding.HexPrefix where

import Data.Bits
import Data.Word
import Data.Word.Odd
import qualified Data.ByteString.Lazy as L

data HPArray = HPArray [Word4] Bool
        deriving (Show,Eq)

encodeHexPrefix ::  HPArray -> L.ByteString
encodeHexPrefix (HPArray ns b) = L.pack $
        case even $ length ns of
                True  -> ((16 * ft)       + (0)                     ) : pairAsBytes ns
                False -> ((16 * (ft + 1)) + ((fromIntegral.head) ns)) : pairAsBytes (tail ns)
        where ft = if b then 2 else 0
              pairAsBytes :: [Word4] -> [Word8]
              pairAsBytes (n1:n2:rest)  =
                      let n1' = fromIntegral n1 :: Word8
                          n2' = fromIntegral n2 :: Word8
                      in ((n1' * 16) + n2') : (pairAsBytes rest)
              pairAsBytes [_]           = error "Expected even size list"
              pairAsBytes []            = []

decodeHexPrefix :: L.ByteString -> HPArray
decodeHexPrefix = decodeHexPrefix'.L.unpack 

decodeHexPrefix' :: [Word8] -> HPArray
decodeHexPrefix' (b:bs) =
        let ft = b `testBit` 5
            isEven = not $ b `testBit` 4
            nibbles = case isEven of
                True  -> unpairBytes bs
                False -> (fromIntegral $ 0x0f .&. b) : unpairBytes bs
            unpairBytes (x:xs) = (fromIntegral $ x `shiftR` 4) :
                                     (fromIntegral $ x .&. 0x0f) :
                                     unpairBytes xs
            unpairBytes []    = []
        in HPArray nibbles ft
decodeHexPrefix' []     = error "Require at least one byte of input."

