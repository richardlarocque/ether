{- |
Module      :  Ethereum.SimpleTypes
Description :  Type Declarations for Ethereum
Copyright   :  (c) Richard Larocque
License     :  GPL-3.0+

Maintainer  :  richard.larocque@gmail.com
Stability   :  unstable
Portability :  non-portable (Unknown portability)
-}

module Ethereum.SimpleTypes (
        Gas,
        Memory,
        MemSlice,
        Stack,
        Address(..),
        Ether,
        ByteArray) where

import Data.Word
import Data.LargeWord
import qualified Data.Map as M

type Gas = Integer
type Memory = M.Map Word256 Word256
type MemSlice = [Word256]
type Stack = [Word256]
data Address = Address
type Ether = Integer
type ByteArray = [Word8]
