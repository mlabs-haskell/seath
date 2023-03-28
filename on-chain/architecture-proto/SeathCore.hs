module SeathCore where

import Data.Functor.Identity (Identity (runIdentity))
import Types (Tx, UserAction)
import Prelude

type Contract = Identity

runContract :: Contract a -> IO a
runContract = pure . runIdentity

actionsToTxChain :: [UserAction] -> Contract [Tx]
actionsToTxChain = traverse (pure . mkTx)

mkTx :: UserAction -> Tx
mkTx ua = "Tx-from-" <> ua
