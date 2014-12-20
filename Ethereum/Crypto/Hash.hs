module Ethereum.Crypto.Hash(hashAsWord, hashAsBytes) where

import           Crypto.Hash
import           Data.Byteable
import qualified Data.ByteString as B
import           Data.LargeWord
import           Ethereum.Common

hashAsWord :: B.ByteString -> Word256
hashAsWord = decode256be . hashAsBytes

hashAsBytes :: B.ByteString -> B.ByteString
hashAsBytes bs = toBytes (hash bs :: Digest SHA3_256)
