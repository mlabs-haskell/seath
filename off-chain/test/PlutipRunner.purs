module Seath.Test.PlutipRunner (run) where

import Contract.Config (LogLevel(Info), emptyHooks)
import Contract.Monad (Contract, launchAff_)
import Contract.Test.Plutip (PlutipConfig, withPlutipContractEnv)
import Contract.Wallet (KeyWallet)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe(Nothing))
import Data.Time.Duration (Seconds(Seconds))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (fromInt) as UInt
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (supervise)
import Prelude (($))
import Seath.Network.Types (LeaderNode, UserNode)
import Seath.Test.Examples.Addition.Types (AdditionAction)
import Seath.Test.QuickCheck (makeDistribution)
import Seath.Test.Types (Participant)
import Test.Examples.Addition.SeathNetwork as SeathNet
import Type.Function (type ($))
import Undefined (undefined)

run :: Effect Unit
run = launchAff_
  $ withPlutipContractEnv config (makeDistribution 4)
  $
    \env ((adminWallet /\ leaderWallet) /\ participantsWallets) -> do
      ( supervise -- ! misha: I've added it here so we get the same behavior as with `withContractEnv``

          $ SeathNet.mainTest env adminWallet leaderWallet
              participantsWallets
      )

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
