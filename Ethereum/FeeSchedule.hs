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
step, balance, stop, suicide, sload, sset, sreset, sclear, create :: Integer
createdata, call, exp, expbyte, memory, txdatazero, txdatanonzero :: Integer
transaction, log, logdata, logtopic, sha3, sha3word, copy :: Integer

step = 1
balance = 20
stop = 0
suicide = 0
sload = 20
sset = 300
sreset = 100
sclear = 0
create = 100
createdata = 5
call = 20
exp = 1
expbyte = 1
memory = 1
txdatazero = 1
txdatanonzero = 5
transaction = 500
log = 1
logdata = 1
logtopic = 1
sha3 = 10
sha3word = 10
copy = 1
