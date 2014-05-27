{- |
Module      :  Ethereum.FeeSchedule
Description :  Fee schedule declarations for Ethereum
Copyright   :  (c) Richard Larocque
License     :  GPL-3.0+

Maintainer  :  richard.larocque@gmail.com
Stability   :  unstable
Portability :  non-portable (Unknown portability)

Translation of Ethereum Yellow Paper, Proof-of-Concept V, Section 9.3
-}

module Ethereum.FeeSchedule where

-- Appendix B: Fee schedule
step, stop, suicide, sha3, sload, sstore, balance, create, call :: Integer
memory, txdata, transaction :: Integer

step = 1
stop = 0
suicide = 0
sha3 = 20
sload = 20
sstore = 100
balance = 20
create = 100
call = 20
memory = 1
txdata = 5
transaction = 500
