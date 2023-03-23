module Seath.Test.Types
  ( BlockchainState(BlockchainState)
  , Leader(Leader)
  , Participant(Participant)
  , RunnerConfig(RunnerConfig)
  ) where

import Contract.Utxos (UtxoMap)
import Contract.Wallet (KeyWallet)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.BigInt (BigInt)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\))

newtype BlockchainState s = BlockchainState
  { leaderUTXOs :: Maybe UtxoMap
  , usersUTXOs :: Array (Maybe UtxoMap)
  , sctiptState :: UtxoMap /\ s
  }

derive instance Newtype (BlockchainState s) _

newtype Leader = Leader KeyWallet

derive instance Newtype Leader _

newtype Participant = Participant KeyWallet

derive instance Newtype Participant _

newtype RunnerConfig (s :: Type) = RunnerConfig
  { admin :: KeyWallet -- wallet that will run init contract
  , seathLeader :: KeyWallet
  , seathParticipants :: NonEmptyArray KeyWallet
  , minAdaRequired :: BigInt
  , expectedStateChange :: s -> s
  }

derive instance Newtype (RunnerConfig s) _

