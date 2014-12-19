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

import           Control.Monad
import           Data.Bits
import qualified Data.ByteString       as B
import           Data.Serialize
import           Data.Word
import           Data.Word.Odd
import           Ethereum.Common
import           Ethereum.Encoding.RLP

data HPArray = HPArray [Word4] Bool
        deriving (Show,Eq)

asHexPrefix :: [Word4] -> Bool -> B.ByteString
asHexPrefix ns b = runPut $ putHexPrefix ns b

unHexPrefix :: B.ByteString -> Either String HPArray
unHexPrefix = runGet $ do
  len <- getArrayHeader
  isolate (fromIntegral len) getHexPrefix'

putHexPrefix :: [Word4] -> Bool -> Put
putHexPrefix ns f = putArray $ runPut (putHexPrefixBytes ns f)

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
                           decodeScalar ls
                _ -> fail "Not paresable as array"

getHexPrefix :: Bool -> Get [Word4]
getHexPrefix f = do
        len <- getArrayHeader
        isolate (fromIntegral len) (getHexPrefixBytes f)

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

getHexPrefixBytes :: Bool -> Get [Word4]
getHexPrefixBytes ef = do
        (HPArray ns af) <- getHexPrefix'
        if af == ef
           then return ns
           else fail "Flag did not match"

getHexPrefix' :: Get HPArray
getHexPrefix' = do
        b <- getWord8
        let ft = b `testBit` 5
        let isEven = not $ b `testBit` 4
        let prefix = if isEven
                        then []
                        else [lowNibble b]
        bs <- do r <- remaining
                 replicateM r getWord8
        return $ HPArray (prefix ++ nibbleize bs) ft
