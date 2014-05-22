module Ethereum.State.Address where

import qualified Data.ByteString as B

import Data.LargeWord

data Address = Address Word160

asBigEndian :: Address -> B.ByteString
asBigEndian (Address w) = B.pack . reverse . take 20 $ bs
        where bs = map (fromIntegral.(\x -> x `mod` 256)) (iterate (\x -> x `div` 256) w)
