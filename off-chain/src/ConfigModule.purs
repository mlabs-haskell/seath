module Seath.ConfigModule
  ( getCoreConfig
  )
  where

import Prelude

import Contract.Wallet (withKeyWallet)
import Seath.Core.Types (CoreConfiguration(CoreConfiguration))
import Seath.Network.Utils (getPublicKeyHash)
import Seath.Test.Examples.Addition.Actions as Addition
import Seath.Test.Examples.Addition.Actions as Addition


getCoreConfig leaderPublicKeyHash = do
    vaildatorHash <- Addition.fixedValidatorHash

    pure $ CoreConfiguration
        { leader: leaderPublicKeyHash
        , stateVaildatorHash: vaildatorHash
        , actionHandler: Addition.handleAction
        , queryBlockchainState: Addition.queryBlockchainState
        }

-- genAction :: KeyWallet -> Contract (UserAction AdditionAction)
-- genAction w =
--   withKeyWallet w $ do
--     ownUtxos <- liftedM "no UTxOs found" getWalletUtxos
--     publicKeyHash <- withKeyWallet w getPublicKeyHash
--     changeAddress <- liftedM "can't get Change address" $ head <$>
--       getWalletAddressesWithNetworkTag
--     pure $ UserAction
--       { action: AddAmount stateChangePerAction
--       , publicKey: publicKeyHash
--       , userUTxOs: ownUtxos
--       , changeAddress: ChangeAddress changeAddress
--       }