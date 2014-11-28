module Ethereum.State.Block where

import qualified Data.ByteString            as B
import           Data.LargeWord
import           Data.Serialize
import           Ethereum.Common
import           Ethereum.Encoding.RLP
import           Ethereum.State.Address
import           Ethereum.State.Transaction

data BlockHeader = BlockHeader {
        parentHash       :: Word256,
        unclesHash       :: Word256,
        coinbase         :: Address,
        stateRoot        :: Word256,
        transactionsTrie :: Word256,
        difficulty       :: Integer,
        timestamp        :: Integer,
        number           :: Integer,
        minGasPrice      :: Integer,
        gasLimit         :: Integer,
        gasUsed          :: Integer,
        extraData        :: B.ByteString,
        blockNonce       :: Word256
} deriving (Show, Eq)

data TransactionReceipt = TransactionReceipt {
        receiptTrans :: Transaction,
        receiptState :: Word256,
        receiptGas   :: Integer
} deriving (Show, Eq)

data Block = Block {
        header   :: BlockHeader,
        receipts :: [TransactionReceipt],
        uncles   :: [BlockHeader]
} deriving (Show, Eq)

-- Appendix I
genesisBlockHeader :: BlockHeader
genesisBlockHeader = BlockHeader {
        parentHash = 0,
        unclesHash = hashPut $ putSequenceBytes B.empty,
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
        blockNonce = 0
}

putBlockHeader :: BlockHeader -> Put
putBlockHeader b = putSequence $
        do putScalar256     $ parentHash b
           putScalar256     $ unclesHash b
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
           putScalar256     $ blockNonce b

putBlockHeaderWithoutNonce :: BlockHeader -> Put
putBlockHeaderWithoutNonce b = putSequence $
        do putScalar256     $ parentHash b
           putScalar256     $ unclesHash b
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

getBlockHeader :: Get BlockHeader
getBlockHeader = getSequence $
        do ph   <- getScalar256
           uh   <- getScalar256
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

putTransactionReceipt :: TransactionReceipt -> Put
putTransactionReceipt (TransactionReceipt t s gu) = putSequence $
        do putTransaction t
           putScalar256 s
           putScalar gu

getTransactionReceipt :: Get TransactionReceipt
getTransactionReceipt = getSequence $
        do t <- getTransaction
           s <- getScalar256
           gu <- getScalar
           return $ TransactionReceipt t s gu

putUncles :: [BlockHeader] -> Put
putUncles = putListAsSequence putBlockHeader

getUncles :: Get [BlockHeader]
getUncles = getListAsSequence getBlockHeader

putBlock :: Block -> Put
putBlock (Block bh ts us) = putSequence $
        do putBlockHeader bh
           putListAsSequence putTransactionReceipt ts
           putUncles us

getBlock :: Get Block
getBlock = getSequence $
        do bh <- getBlockHeader
           ts <- getListAsSequence getTransactionReceipt
           us <- getUncles
           return $ Block bh ts us
