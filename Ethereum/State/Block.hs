module Ethereum.State.Block where

import Data.Binary
import Data.Binary.Get
import Data.LargeWord
import Ethereum.State.Address
import Ethereum.Encoding.RLP
import qualified Data.ByteString as B

data BlockHeader = BlockHeader {
        parentHash :: Word256,
        unclesHash :: [Word256],
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
} deriving (Show, Eq)

-- Appendix I
genesisBlockHeader :: BlockHeader
genesisBlockHeader = BlockHeader {
        parentHash = 0,
        unclesHash = [],
        stateRoot = 0,
        coinbase = A 0,
        transactionsTrie = 0,

        difficulty = 2 ^ (22 :: Integer),
        timestamp = 0,
        number = 0,

        minGasPrice = 0,
        gasLimit = 0,
        gasUsed = 0,
        extraData = B.singleton 42,
        nonce = 0
}

putBlockHeader :: BlockHeader -> Put
putBlockHeader b = putSequence $
        do putScalar256     $ parentHash b
           putSequence      $ mapM_ putScalar256 (unclesHash b)
           putAddress       $ coinbase b
           putScalar256     $ stateRoot b
           putScalar256     $ transactionsTrie b
           putScalar        $ difficulty b
           putScalar        $ timestamp b
           putScalar        $ number b
           putScalar        $ minGasPrice b
           putScalar        $ gasLimit b
           putScalar        $ gasUsed b
           putSequenceBytes $ extraData b
           putScalar256     $ nonce b

getBlockHeader :: Get BlockHeader
getBlockHeader = getSequence $
        do ph   <- getScalar256
           uh   <- getSequence $ getUncles []
           cb   <- getAddress
           sr   <- getScalar256
           tt   <- getScalar256
           d    <- getScalar
           t    <- getScalar
           num  <- getScalar
           mgp  <- getScalar
           gl   <- getScalar
           gu   <- getScalar
           ed   <- getSequenceBytes
           non  <- getScalar256
           return $ BlockHeader ph uh cb sr tt d t num mgp gl gu ed non
         where getUncles us = do done <- isEmpty
                                 if done
                                    then return (reverse us)
                                    else do u <- getScalar256
                                            getUncles (u:us)
