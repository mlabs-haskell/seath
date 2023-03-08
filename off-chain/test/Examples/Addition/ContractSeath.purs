module Seath.Test.Examples.Addition.ContractSeath (mainTest) where

import Contract.Log (logInfo')
import Contract.Monad (Aff, Contract)
import Contract.Prelude (map)
import Contract.Test.Plutip (PlutipConfig, runPlutipContract)
import Contract.Utxos (getWalletUtxos, utxosAt)
import Contract.Wallet (withKeyWallet)
import Control.Monad (bind)
import Data.Array (unzip)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Left))
import Data.Monoid ((<>))
import Data.Show (show)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unit (Unit)
import Prelude (discard, ($))
import Seath.HandleActions (actions2TransactionsChain)
import Seath.Test.Examples.Addition.Actions
  ( fixedValidatorHash
  , handleActionFromBlockChain
  , handleActionFromFinalizedTransaction
  )
import Seath.Test.Examples.Addition.Contract (initialContract)
import Seath.Test.Examples.Addition.SeathSetup
  ( Leader(Leader)
  , Participant(Participant)
  , getPublicKeyHash
  , logBlockchainState
  )
import Seath.Test.Examples.Addition.SeathSetup as SeathSetup
import Seath.Test.Examples.Utils (getScriptUtxos)
import Seath.Types
  ( ChainBuilderState(ChainBuilderState)
  , SeathConfig(SeathConfig)
  )

mainTest :: PlutipConfig -> Aff Unit
mainTest config = runPlutipContract config distribution $ \(a /\ b /\ c) ->
  do
    leaderPublicKeyHash <- getPublicKeyHash a
    let
      leader = Leader a
      namedWallets = [ a /\ "Leader", b /\ "UserB", c /\ "UserC" ]
      participants = map Participant [ b, c ]
      seathConfig = SeathConfig
        { leader: leaderPublicKeyHash
        , finalizedTxHandler: handleActionFromFinalizedTransaction
        , onchainHandler: handleActionFromBlockChain
        }
    logBlockchainState namedWallets fixedValidatorHash
    firstTransactionId /\ _ <- withKeyWallet a initialContract
    logBlockchainState namedWallets fixedValidatorHash
    actions <- SeathSetup.genUserActions participants
    logInfo' $ "test " <> show actions
    let
      firstBuilderState = ChainBuilderState
        { finalizedTransactions: []
        , lastResult: Left firstTransactionId
        , pendingActions: actions
        }
      buildChain =
        actions2TransactionsChain
          seathConfig
          firstBuilderState
    (finalizedTxsAndActions /\ _) <- withKeyWallet a buildChain
    let finalizedTxs /\ _ = unzip finalizedTxsAndActions
    -- logInfo' $ "BuildChainResult: " <> show finalizedTxs
    _ <- SeathSetup.submitChain leader participants finalizedTxs $
      logBlockchainState namedWallets fixedValidatorHash
    logBlockchainState namedWallets fixedValidatorHash
    logInfo' "end"
  where

  distribution :: Array BigInt /\ Array BigInt /\ Array BigInt
  distribution =
    [ BigInt.fromInt 1_000_000_000 ]
      /\ [ BigInt.fromInt 1_000_000_000 ]
      /\ [ BigInt.fromInt 1_000_000_000 ]

