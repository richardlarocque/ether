module Ethereum.State.Block where

import Control.Monad
import Data.Binary
import Data.Binary.Put
import Data.LargeWord
import Ethereum.Common
import Ethereum.State.Address
import Ethereum.State.Transaction
import Ethereum.Storage.Context
import Ethereum.Encoding.RLP
import Ethereum.Execution
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

data BlockHeader = BlockHeader {
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
        blockNonce :: Word256
} deriving (Show, Eq)

data TransactionReceipt = TransactionReceipt {
        receiptTrans :: Transaction,
        receiptState :: Word256,
        receiptGas :: Integer
} deriving (Show, Eq)

data Block = Block {
        header :: BlockHeader,
        receipts :: [TransactionReceipt],
        uncles :: [BlockHeader]
} deriving (Show, Eq)

-- Appendix I
genesisBlockHeader :: BlockHeader
genesisBlockHeader = BlockHeader {
        parentHash = 0,
        unclesHash = hashBytes $ L.toStrict $ runPut $ putSequenceBytes B.empty,
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

-- Equation 17.
isConsistent :: Context -> Block -> Bool
isConsistent c b = checkUncles && checkReceipts && checkState
        where checkUncles = (unclesHash . header) b == (hashPut . putUncles . uncles) b
              checkReceipts = (transactionsTrie . header) b == (rootHash . receiptsToTrie . receipts) b
              checkState = case doBlockTransactions c b of
                      Nothing -> False
                      Just c' -> rootHash c' == (stateRoot . header) b

receiptsToTrie :: [TransactionReceipt] -> Context
receiptsToTrie rs =
        let keys = map asBE ([0..] :: [Integer])
            values = map (L.toStrict . runPut . putTransactionReceipt) rs
            pairs = zip keys values
            c0 = initContext
        in foldr (\pair c -> insertToTrie c pair) c0 pairs

doBlockTransactions :: Context -> Block -> Maybe Context
doBlockTransactions c0 b =
        let ts = map receiptTrans $ receipts b
        in foldM doTransaction c0 ts

-- Equation 25.
difficultyFromParent :: Integer -> BlockHeader -> Integer
difficultyFromParent childTime p =
        let parentDifficulty = difficulty p
        in if childTime < timestamp p + 42
              then parentDifficulty + (parentDifficulty `div` 1024)
              else parentDifficulty - (parentDifficulty `ceilDiv` 1024)

-- Equation 26-27.
gasLimitFromParent :: BlockHeader -> Integer
gasLimitFromParent p = max 10000 calculated
        where calculated = (1023 * (gasLimit p)) + (6 * (gasUsed p) `div` 5) `div` 1024

-- Equation 30.
isBlockNonceValid :: BlockHeader -> Bool
isBlockNonceValid bh =
        let p = proofOfWork bh (blockNonce bh)
            minValue = (2 ^ (256 :: Integer) `div` difficulty bh)
        in fromIntegral p <= minValue

proofOfWork :: BlockHeader -> Word256 -> Word256
proofOfWork bh nonc =
        hashPut . put $ hashPut $ do { putBlockHeaderWithoutNonce bh; put nonc }

-- Equations 30-34.
isHeaderValid :: BlockHeader -> BlockHeader -> Bool
isHeaderValid b p = and [
        isBlockNonceValid b,
        difficulty b == difficultyFromParent (timestamp b) p,
        gasLimit b == gasLimitFromParent p,
        timestamp b > timestamp p,
        (B.length . extraData) b < 1024 ]

