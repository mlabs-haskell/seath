module Seath.Test.Examples.Addition.SeathSetup
  ( Leader(Leader)
  , Participant(Participant)
  , genAction
  , genUserActions
  , getPublicKeyHash
  , submitChain
  , logBlockchainState
  ) where

import Contract.Address
  ( PubKeyHash
  , getWalletAddresses
  , getWalletAddressesWithNetworkTag
  , toPubKeyHash
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM)
import Contract.Scripts (ValidatorHash)
import Contract.Transaction
  ( BalancedSignedTransaction
  , FinalizedTransaction
  , TransactionHash
  , awaitTxConfirmed
  , signTransaction
  , submit
  )
import Contract.Utxos (getWalletUtxos)
import Contract.Wallet (withKeyWallet)
import Contract.Wallet.Key (KeyWallet)
import Control.Applicative (pure)
import Control.Monad (bind)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head, zip)
import Data.BigInt as BigInt
import Data.Functor ((<$>))
import Data.Monoid ((<>))
import Data.Newtype (class Newtype, unwrap)
import Data.Show (show)
import Data.Traversable (traverse, traverse_)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unit (Unit)
import Effect.Aff (error)
import Prelude (discard, flip, ($))
import Seath.Test.Examples.Addition.Types (AdditionAction(AddAmount))
import Seath.Test.Examples.Utils (getScriptUtxos)
import Seath.Types (UserAction(UserAction))

newtype Leader = Leader KeyWallet

derive instance Newtype Leader _

newtype Participant = Participant KeyWallet

derive instance Newtype Participant _

-- todo: pass action as param?
genUserActions
  :: Array Participant -> Contract (Array (UserAction AdditionAction))
genUserActions ps =
  traverse genAction ps

genAction :: Participant -> Contract (UserAction AdditionAction)
genAction (Participant p) =
  withKeyWallet p $ do
    ownUtxos <- liftedM "no UTXOs found" getWalletUtxos
    publicKeyHash <- getPublicKeyHash p
    changeAddress <- liftedM "can't get Change address" $ head <$>
      getWalletAddressesWithNetworkTag
    pure $ UserAction
      { action: AddAmount (BigInt.fromInt 100) -- FIXME: hardcoded
      , publicKey: publicKeyHash
      , userUTxo: ownUtxos
      , changeAddress
      }

getPublicKeyHash :: KeyWallet -> Contract PubKeyHash
getPublicKeyHash kw = withKeyWallet kw do
  address <- liftedM "can't get the address of KeyWallet" $ head <$>
    getWalletAddresses
  liftMaybe (error "can't get pubKeyHash of KeyWallet") $ toPubKeyHash address

signTransactions
  :: Leader
  -> Array (Participant /\ FinalizedTransaction)
  -> Contract (Array BalancedSignedTransaction)
signTransactions leader toSign = flip traverse toSign \(participant /\ tx) -> do
  signedByParticipant <- withKeyWallet (unwrap participant) $ signTransaction tx
  withKeyWallet (unwrap leader) $ signTransaction signedByParticipant

submitChain
  :: Leader
  -> Array Participant
  -> Array FinalizedTransaction
  -> Contract Unit
  -> Contract (Array TransactionHash)
submitChain leader participants txs log = do
  allSigned <- signTransactions leader (zip participants txs)
  withKeyWallet (unwrap leader) $ traverse submitAndWait allSigned
  where
  submitAndWait balancedAndSignedTransaction = do
    log
    -- logInfo' $ "submiting: " <> show balancedAndSignedTransaction
    transactionId <- submit balancedAndSignedTransaction
    awaitTxConfirmed transactionId
    -- logInfo' $ "submited: " <> show transactionId
    pure $ transactionId

logBlockchainState
  :: Array (KeyWallet /\ String) -> Contract ValidatorHash -> Contract Unit
logBlockchainState users valHashW = do
  valHash <- valHashW
  logInfo' "------------------------- BlochainState -------------------------"
  flip traverse_ users \(wallet /\ name) ->
    ( do
        autxos <- withKeyWallet wallet getWalletUtxos
        logInfo' $ "utxosAt " <> name <> ": " <> show autxos
    )
  scriptUtxos <- getScriptUtxos valHash
  logInfo' $ "utxosAt script: " <> show scriptUtxos
