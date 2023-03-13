module Seath.Test.Examples.Addition.SeathSetup
  ( Leader(Leader)
  , Participant(Participant)
  , genAction
  , genUserActions
  , getPublicKeyHash
  , submitChain
  , logBlockchainState
  , getBlockhainState
  , BlockhainState(..)
  ) where

import Contract.Address (PubKeyHash, getWalletAddresses, getWalletAddressesWithNetworkTag, toPubKeyHash)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM)
import Contract.Prelude (Maybe, Tuple, for_)
import Contract.Scripts (ValidatorHash)
import Contract.Transaction (BalancedSignedTransaction, FinalizedTransaction, TransactionHash, awaitTxConfirmed, signTransaction, submit)
import Contract.Utxos (UtxoMap, getWalletUtxos)
import Contract.Wallet (withKeyWallet)
import Contract.Wallet.Key (KeyWallet)
import Control.Applicative (pure)
import Control.Monad (bind)
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head, length, zip, zipWith, (..))
import Data.BigInt as BigInt
import Data.Functor ((<$>))
import Data.Monoid ((<>))
import Data.Newtype (class Newtype, unwrap)
import Data.Show (show)
import Data.Traversable (traverse)
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
    -- awaitTxConfirmed transactionId
    -- logInfo' $ "submited: " <> show transactionId
    pure $ transactionId

newtype BlockhainState = BlockhainState
  { leaderUTXOs :: Maybe UtxoMap
  , usersUTXOs :: Array (Maybe UtxoMap)
  , sctiptUTXOs :: UtxoMap

  }

getBlockhainState
  :: Leader -> Array Participant -> ValidatorHash -> Contract BlockhainState
getBlockhainState leader participants valHash = do
  leaderUTXOs <- withKeyWallet (unwrap leader) $ getWalletUtxos
  usersUTXOs :: _ <- traverse (\p -> withKeyWallet (unwrap p) getWalletUtxos)
    participants
  sctiptUTXOs <- getScriptUtxos valHash
  pure $ BlockhainState { leaderUTXOs, usersUTXOs, sctiptUTXOs }

-- getBlockhainState = 
logBlockchainState
  :: Leader -> Array Participant -> ValidatorHash -> Contract Unit
logBlockchainState leader participants valHash = do
  (BlockhainState bchState) <- getBlockhainState leader participants valHash
  logInfo' "------------------------- BlochainState -------------------------"
  logInfo' $ "utxosAt LEADER: " <> show bchState.leaderUTXOs
  for_ (enumUsers bchState.usersUTXOs) $ \(i /\ us) ->
    logInfo' $ "utxosAt " <> i <> ": " <> show us
  logInfo' $ "utxosAt script: " <> show bchState.sctiptUTXOs
  where
  enumUsers :: Array (Maybe UtxoMap) -> Array (Tuple String (Maybe UtxoMap))
  enumUsers ps = zipWith
    (\p i -> (("user-" <> show i) /\ p))
    ps
    (1 .. (length participants))

