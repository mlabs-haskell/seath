module Seath.HTTP.Utils (mkLeaderConfig, mkUserConfig) where

import Contract.Prelude

import Aeson (class EncodeAeson)
import Contract.PlutusData (class FromData, class ToData)
import Contract.Scripts (class DatumType, class RedeemerType)
import Contract.Transaction (Transaction)
import Seath.Core.ChainBuilder as ChainBuilder
import Seath.Core.Types (CoreConfiguration)
import Seath.HTTP.UserHandlers as HttpUser
import Seath.Network.Types
  ( LeaderConfiguration(LeaderConfiguration)
  , RunContract(RunContract)
  , UserConfiguration(UserConfiguration)
  )

mkLeaderConfig
  :: forall (actionType :: Type) (userStateType :: Type) (validatorType :: Type)
       (redeemerType :: Type)
       (datumType :: Type)
   . DatumType validatorType datumType
  => RedeemerType validatorType redeemerType
  => FromData datumType
  => ToData datumType
  => FromData redeemerType
  => ToData redeemerType
  => CoreConfiguration actionType userStateType validatorType datumType
       redeemerType
  -> RunContract
  -> LeaderConfiguration actionType
mkLeaderConfig coreConfig runContract =

  let
    (RunContract runC) = runContract
    buildChain = \actions -> runC $ fst <$> ChainBuilder.buildChain coreConfig
      actions
      Nothing
  in
    LeaderConfiguration
      { maxWaitingTimeForSignature: 3000
      , maxQueueSize: 4
      , numberOfActionToTriggerChainBuilder: 4
      , maxWaitingTimeBeforeBuildChain: 3000
      , runContract
      , buildChain
      }

mkUserConfig
  :: forall a
   . EncodeAeson a
  => String
  -> RunContract
  -> (Transaction -> Aff (Either String Transaction))
  -> UserConfiguration a
mkUserConfig leaderUrl runContract checkChainedTx =
  UserConfiguration
    { networkHandlers: HttpUser.mkHttpHandlers leaderUrl
    , runContract
    , checkChainedTx
    }
