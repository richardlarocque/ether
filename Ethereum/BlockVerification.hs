module Ethereum.BlockVerification where

import           Control.Monad
import           Crypto.Hash              (Digest, SHA3_256, hash)
import           Data.Byteable
import qualified Data.ByteString          as B
import           Data.LargeWord
import           Data.Serialize
import           Ethereum.Common
import           Ethereum.Crypto
import           Ethereum.Encoding.RLP
import           Ethereum.Execution
import           Ethereum.State.Block
import           Ethereum.Storage.Context

-- Equation 17.
isConsistent :: Context -> Block -> Bool
isConsistent c b = checkNonce && checkUncles && checkReceipts && checkState
        where checkNonce = (isBlockNonceValid . header) b
              checkUncles = isUnclesHashValid b
              checkReceipts = isReceiptHashValid b
              checkState = case doBlockTransactions c b of
                      Nothing -> False
                      Just c' -> rootHash c' == (stateRoot . header) b

receiptsToTrie :: [TransactionReceipt] -> Context
receiptsToTrie rs =
        let keys = map (runPut . putScalar) ([0..] :: [Integer])
            values = map (runPut . putTransactionReceipt) rs
            pairs = zip keys values
            c0 = initContext
        in foldr (flip insertToTrie) c0 pairs

doBlockTransactions :: Context -> Block -> Maybe Context
doBlockTransactions c0 (Block bh rs _) =
        foldM (doTransaction bh) c0 (map receiptTrans rs)

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
        where calculated = (1023 * gasLimit p) + (6 * gasUsed p `div` 5) `div` 1024

-- Equation 30.
isBlockNonceValid :: BlockHeader -> Bool
isBlockNonceValid bh =
        let p = proofOfWork bh (blockNonce bh)
            minValue = (2 ^ (256 :: Integer) `div` difficulty bh)
        in fromIntegral p <= minValue

isUnclesHashValid :: Block -> Bool
isUnclesHashValid b =
    let actualHash = hashAsWord $ runPut (putUncles (uncles b))
        headerHash = (unclesHash . header) b
    in headerHash == actualHash

isReceiptHashValid :: Block -> Bool
isReceiptHashValid b =
    let actualHash = (rootHash . receiptsToTrie . receipts) b
        headerHash = (transactionsTrie . header) b
    in headerHash == actualHash

proofOfWork :: BlockHeader -> Word256 -> Word256
proofOfWork bh nonc =
    let headerBytes = runPut $ putBlockHeaderWithoutNonce bh
        innerHash   = toBytes (hash headerBytes :: Digest SHA3_256)
        outerBytes  = innerHash `B.append` encode256be nonc
        outerHash   = hashAsWord outerBytes
    in outerHash

-- Equations 30-34.
isHeaderValid :: BlockHeader -> BlockHeader -> Bool
isHeaderValid b p = and [
        isBlockNonceValid b,
        difficulty b == difficultyFromParent (timestamp b) p,
        gasLimit b == gasLimitFromParent p,
        timestamp b > timestamp p,
        (B.length . extraData) b < 1024 ]

areSignaturesValid :: Block -> Bool
areSignaturesValid b =
    let ts = map receiptTrans $ receipts b
    in all isSignatureValid ts
