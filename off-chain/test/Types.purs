module Seath.Test.Types
  ( BlockchainState(BlockchainState)
  , Leader(Leader)
  , Participant(Participant)
  , RunnerConfiguration(RunnerConfiguration)
  ) where

import Contract.Utxos (UtxoMap)
import Contract.Wallet (KeyWallet)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.BigInt (BigInt)
import Data.Log.Level (LogLevel)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\))
import Seath.Network.Types (LeaderNode, UserNode)

newtype BlockchainState s = BlockchainState
  { leaderUTXOs :: Maybe UtxoMap
  , usersUTXOs :: Array (Maybe UtxoMap)
  , sctiptState :: UtxoMap /\ s
  }

derive instance Newtype (BlockchainState s) _

newtype Leader = Leader { wallet :: KeyWallet, node :: LeaderNode }

derive instance Newtype Leader _

newtype Participant = Participant { wallet :: KeyWallet, node :: UserNode }

derive instance Newtype Participant _

newtype RunnerConfiguration (s :: Type) = RunnerConfiguration
  { admin :: KeyWallet -- wallet that will run init contract
  , leader :: Leader
  , participants :: NonEmptyArray Participant
  , minAdaRequired :: BigInt
  , expectedStateChange :: s -> s
  , logLevel :: LogLevel
  }

derive instance Newtype (RunnerConfiguration s) _
