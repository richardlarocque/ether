module Ethereum.Lang.Ops where

import Data.Word
import Data.LargeWord
import Data.ByteString.Builder
import Data.Binary
import Data.Monoid
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Ethereum.EVM.InstructionSet

type TriOp = Builder -> Builder -> Builder -> Builder
type BinOp = Builder -> Builder -> Builder
type UnOp = Builder -> Builder
type ZeroOp = Builder

compile :: Builder -> B.ByteString
compile = L.toStrict . toLazyByteString

--

word256BE :: Word256 -> Builder
word256BE = lazyByteString . encode

p32 :: Word256 -> Builder
p32 x = let x' = fromIntegral x :: Word256
        in op PUSH32 <> word256BE x'

p32i :: Integral a => a -> Builder
p32i = p32.fromIntegral

p1 :: Word8 -> Builder
p1 x = op PUSH1 <> word8 x

op :: Instruction -> Builder
op o = word8 (toOpcode o)

asOp :: Word8 -> Builder
asOp = word8

unOp :: Instruction -> Builder -> Builder
unOp i a = a <> op i

binOp :: Instruction -> Builder -> Builder -> Builder
binOp i a1 a2 = a2 <> a1 <> op i

triOp :: Instruction -> Builder -> Builder -> Builder -> Builder
triOp i a1 a2 a3 = a3 <> a2 <> a1 <> op i

-- Some conveniences:

codecopy :: TriOp
codecopy = triOp CODECOPY

mstore :: BinOp
mstore = binOp MSTORE

mstore8 :: BinOp
mstore8 = binOp MSTORE8

add :: BinOp
add = binOp ADD

pc :: ZeroOp
pc = op PC

msize :: ZeroOp
msize = op MSIZE

return :: BinOp
return = binOp RETURN
