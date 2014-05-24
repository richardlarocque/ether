module Ethereum.Execution where

import Ethereum.State.Transaction
import Ethereum.State.Context


isSignatureValid :: Transaction -> Bool
isSignatureValid _ = True -- FIXME: implement this.

isNonceValid :: Context -> Transaction -> Bool
isNonceValid _ _ = True -- FIXME: implement this.

isBalanceAvailable :: Context -> Transaction -> Bool
isBalanceAvailable _ _ = True -- FIXME: implement this.

-- Full list of checks:
-- * Sender address is not empty.
-- * Sender address is known in the state database.
-- * Nonce == sender's nonce
-- * intrinsicGas <= gasLimit
-- * upFrontCoast <= sender's balance
-- * And also a check on the block's gas limit.

-- If these checks pass, we start making irrevocable state changes.
-- Step 1: Increment the sender's nonce, and remove the up front gas.
-- Step 2: Process the message call or contract creation.
--         Side effects according to those procedure definitions.
-- Step 3:
--   - Refund remaining gas to sender.
--   - Send spent gas to this block's "coinbase".
--   - Add initial gas + spent gas towards the block's limit.
