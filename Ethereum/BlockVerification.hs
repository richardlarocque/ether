module Ethereum.BlockVerification where

import Ethereum.State.Block
import Ethereum.Execution

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
