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
  , MilliSeconds
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
  => MilliSeconds
  -> Int
  -> MilliSeconds
  -> CoreConfiguration actionType userStateType validatorType datumType
       redeemerType
  -> RunContract
  -> LeaderConfiguration actionType
mkLeaderConfig
  maxWaitingTimeBeforeBuildChain
  numberOfActionToTriggerChainBuilder
  maxWaitingTimeForSignature
  coreConfig
  runContract =

  let
    (RunContract runC) = runContract
    buildChain = \actions -> runC $ fst <$> ChainBuilder.buildChain coreConfig
      actions
      Nothing
  in
    LeaderConfiguration
      { maxWaitingTimeForSignature
      , numberOfActionToTriggerChainBuilder
      , maxWaitingTimeBeforeBuildChain
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
