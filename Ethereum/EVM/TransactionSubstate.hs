module Ethereum.EVM.TransactionSubstate where

import Ethereum.State.Account

data TransactionSubstate = TransactionSubstate [Account] [Integer] Integer

a_0 :: TransactionSubstate
a_0 = TransactionSubstate [] [] 0
