module Ethereum.State.Block where

import Data.LargeWord
import Data.ByteString as B
import Ethereum.State.Address

data Block = Block {
        parentHash :: Word256,
        unclesHash :: Word256,
        coinbase :: Address,
        stateRoot :: Word256,
        transactionsTrie :: Word256,
        difficulty :: Integer,
        timestamp :: Integer,
        number :: Integer,
        minGasPrice :: Integer,
        gasLimit :: Integer,
        gasUsed :: Integer,
        extraData :: B.ByteString,
        nonce :: Word256
}

{-
-- Appendix I
genesisBlock :: Block
genesisBlock = Block {
        parentHash = 0,
        unclesHash = 0,
        stateRoot = 0,
        coinbase = 0, -- FIXME that's a type error.
        transactionsTrie = 0,

        difficulty = 2^22,
        timestamp = 0,
        number = 0,

        minGasPrice = 0,
        gasLimit = 0,
        gasUsed = 0,
        extraData = B.empty,
        nonce = 0
}
-}
