module Ethereum.State.Block where

import           Control.Applicative
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
        number           :: Integer,
        minGasPrice      :: Integer,
        gasLimit         :: Integer,
        gasUsed          :: Integer,
        timestamp        :: Integer,
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

blockHeaderToRLPSnippet :: BlockHeader -> [RLP]
blockHeaderToRLPSnippet b =
    [toRLP $ parentHash b,
     toRLP $ unclesHash b,
     toRLP $ coinbase b,
     toRLP $ stateRoot b,
     ttToRLPHack $ transactionsTrie b,
     toRLP $ difficulty b,
     toRLP $ number b,
     toRLP $ minGasPrice b,
     toRLP $ gasLimit b,
     toRLP $ gasUsed b,
     toRLP $ timestamp b,
     toRLP $ extraData b]

blockHeaderWithoutNonceToRLP :: BlockHeader -> RLP
blockHeaderWithoutNonceToRLP = Group . blockHeaderToRLPSnippet

blockHeaderToRLP :: BlockHeader -> RLP
blockHeaderToRLP b =
    Group $ blockHeaderToRLPSnippet b ++ [toRLP $ blockNonce b]

blockHeaderFromRLP :: RLP -> Maybe BlockHeader
blockHeaderFromRLP (Group rs@[_, _, _, _, _, _, _, _, _, _, _, _, bn]) =
    do b <- blockHeaderFromRLPSnippet rs
       n <- fromRLP bn
       Just $ b{blockNonce=n}
blockHeaderFromRLP _ = Nothing

ttToRLPHack :: Word256 -> RLP
ttToRLPHack 0 = Item B.empty
ttToRLPHack x = toRLP x

blockHeaderFromRLPSnippet :: [RLP] -> Maybe BlockHeader
blockHeaderFromRLPSnippet [ph, uh, cb, sr, tt, d, n, mg, gl, gu, ts, ed, _] =
    return BlockHeader
               <*> fromRLP ph
               <*> fromRLP uh
               <*> fromRLP cb
               <*> fromRLP sr
               <*> fromRLP tt
               <*> fromRLP d
               <*> fromRLP n
               <*> fromRLP mg
               <*> fromRLP gl
               <*> fromRLP gu
               <*> fromRLP ts
               <*> fromRLP ed
               <*> return 0
blockHeaderFromRLPSnippet _ = Nothing

receiptToRLP :: TransactionReceipt -> RLP
receiptToRLP (TransactionReceipt t s gu) =
    Group [toRLP t, toRLP s, toRLP gu]

receiptFromRLPSnippet :: RLP -> Maybe TransactionReceipt
receiptFromRLPSnippet (Group [t, s, gu]) =
    return TransactionReceipt <*> fromRLP t <*> fromRLP s <*> fromRLP gu
receiptFromRLPSnippet _ = Nothing

instance RLPSerialize Block where
    toRLP (Block bh ts us) = Group [ blockHeaderToRLP bh,
                                     Group (map receiptToRLP ts),
                                     Group (map blockHeaderToRLP us) ]
    fromRLP (Group [bh, t, u]) =
        return Block
                   <*> blockHeaderFromRLP bh
                   <*> tsFromRLP t
                   <*> usFromRLP u
               where tsFromRLP (Group ts) = mapM receiptFromRLPSnippet ts
                     tsFromRLP _ = Nothing
                     usFromRLP (Group us) = mapM blockHeaderFromRLP us
                     usFromRLP _ = Nothing
    fromRLP _ = Nothing

-- Appendix I
genesisBlockHeader :: BlockHeader
genesisBlockHeader = BlockHeader {
        parentHash = 0,
        unclesHash = hashAsWord $ runPut $ putSequenceBytes B.empty,
        stateRoot = 0,
        coinbase = A 0,
        transactionsTrie = 0,

        difficulty = 2 ^ (22 :: Integer),
        number = 0,

        minGasPrice = 0,
        gasLimit = 0,
        gasUsed = 0,
        timestamp = 0,
        extraData = B.empty,
        blockNonce = 0
}
