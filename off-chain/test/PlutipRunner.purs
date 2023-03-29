module Seath.Test.PlutipRunner (run) where

import Contract.Address (getWalletAddressesWithNetworkTag)
import Contract.Config (LogLevel(Info), emptyHooks)
import Contract.Monad (Contract, launchAff_, liftedM)
import Contract.Test.Plutip (PlutipConfig, runPlutipContract)
import Contract.Wallet (KeyWallet, withKeyWallet)
import Control.Alternative (pure)
import Control.Monad (bind)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head)
import Data.Array.NonEmpty (NonEmptyArray, length, range, zip)
import Data.Array.NonEmpty as NE
import Data.BigInt as BigInt
import Data.Functor ((<$>))
import Data.Log.Level as LogLevel
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (wrap)
import Data.Ring ((*), (+))
import Data.Show (show)
import Data.Time.Duration (Seconds(Seconds))
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (fromInt) as UInt
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (error)
import Prelude (($))
import Seath.Network.Types (LeaderNode, UserNode)
import Seath.Network.Utils (getPublicKeyHash)
import Seath.Test.Examples.Addition.ContractSeath as SeathAddition
import Seath.Test.Examples.Addition.SeathSetup (stateChangePerAction)
import Seath.Test.Examples.Addition.Types (AdditionAction)
import Seath.Test.QuickCheck
  ( genLeaderNodeWith
  , genUserNodeWith
  , makeDistribution
  )
import Seath.Test.Types
  ( Leader(Leader)
  , Participant
  , RunnerConfiguration(RunnerConfiguration)
  )
import Seath.Test.Utils (gen2Contract)
import Type.Function (type ($))
import Undefined (undefined)

run :: Effect Unit
run = launchAff_
  $ runPlutipContract config (makeDistribution 4)
  $
    \((adminWallet /\ leaderWallet) /\ participantsWallets) -> do
      participantsWallets' <- liftMaybe (error "No participants found")
        (NE.fromArray participantsWallets)
      leaderNode <- makeLeaderNode
      let
        numberOfParticipants = length participantsWallets'
        indexes = range 0 numberOfParticipants
        indexedWallets = zip indexes participantsWallets'

      participants <- makeParticipantsFromIndexedWallets indexedWallets

      let
        runnerConf =
          RunnerConfiguration
            { admin: adminWallet
            , leader: Leader { wallet: leaderWallet, node: leaderNode }
            , participants
            , minAdaRequired: BigInt.fromInt 200
            , expectedStateChange: (+)
                (BigInt.fromInt numberOfParticipants * stateChangePerAction)
            , logLevel: LogLevel.Debug
            }

      SeathAddition.mainTest runnerConf

config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Info
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , kupoConfig:
      { port: UInt.fromInt 1443
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , customLogger: Nothing
  , suppressLogs: false
  , hooks: emptyHooks
  , clusterConfig:
      { slotLength: Seconds 1.0 }
  }

-- The types here can change
makeLeaderNode :: Contract $ LeaderNode AdditionAction
makeLeaderNode = undefined

-- The types here can change
makeParticpantNode :: Contract $ UserNode AdditionAction
makeParticpantNode = undefined

makeParticipantsFromIndexedWallets
  :: NonEmptyArray (Int /\ KeyWallet)
  -> Contract $ NonEmptyArray $ Participant AdditionAction
makeParticipantsFromIndexedWallets = undefined
-- traverse indexed2Participant
-- where
-- indexed2Participant (index /\ wallet) = do
--   node <- makeParticpantNodeFromKeyWallet (show index) wallet
--   pure $ wrap { wallet, node }
