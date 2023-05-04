module Seath.Test.Types
  ( BlockchainState(BlockchainState)
  , RunnerSetup
  ) where

import Contract.Monad (ContractEnv)
import Contract.Utxos (UtxoMap)
import Ctl.Internal.Wallet.Key (KeyWallet)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\))

newtype BlockchainState s = BlockchainState
  { leaderUTXOs :: Maybe UtxoMap
  , usersUTXOs :: Array (Maybe UtxoMap)
  , sctiptState :: UtxoMap /\ s
  }

derive instance Newtype (BlockchainState s) _

type RunnerSetup =
  { contractEnv :: ContractEnv
  , adminWallet :: KeyWallet
  , leaderWallet :: KeyWallet
  , userWallets :: Array KeyWallet
  }
