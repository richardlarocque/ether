module Ethereum.EVM.BlockEnvironment where

import Data.LargeWord
import Ethereum.State.Block
import Ethereum.State.Address

data BlockEnvironment = BE {
  blockCoinbase :: Address,
  blockDifficulty :: Integer,
  blockGasLimit :: Integer,
  blockNumber :: Integer,
  blockTimestamp :: Integer,
  blockPreviousHash :: Word256
}

toBlockEnvironment :: BlockHeader -> Word256 -> BlockEnvironment
toBlockEnvironment bh ph=
  BE (coinbase bh) (difficulty bh) (gasLimit bh) (number bh) (timestamp bh) ph
