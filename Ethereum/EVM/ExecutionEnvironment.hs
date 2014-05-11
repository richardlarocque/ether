module Ethereum.EVM.ExecutionEnvironment(
       ExecutionEnvironment(..)) where

import Ethereum.SimpleTypes

-- |The execution environment tuple defined in section 9.3.
data ExecutionEnvironment = EE {
        owner :: Address,
        sender :: Address,
        gasPrice :: Ether,
        input :: ByteArray,
        execCause :: Address,
        value :: Ether,
        code :: Code
}
