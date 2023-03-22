module Seath.Test.Examples.Addition.SeathSetup
  ( Leader(..)
  , Participant(..)
  , genAction
  , genUserActions
  , getBlockhainState
  , getPublicKeyHash
  , getWalletAddress
  , logBlockchainState
  , stateChangePerAction
  , submitChain
  ) where

import Contract.Address
  ( PubKeyHash
  , getNetworkId
  , getWalletAddresses
  , getWalletAddressesWithNetworkTag
  , toPubKeyHash
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM)
import Contract.Prelude (class Show, Maybe, Tuple, for_)
import Contract.Transaction
  ( BalancedSignedTransaction
  , FinalizedTransaction
  , TransactionHash
  , signTransaction
  , submit
  )
import Contract.Utxos (UtxoMap, getWalletUtxos)
import Contract.Wallet (withKeyWallet)
import Contract.Wallet.Key (KeyWallet)
import Control.Applicative (pure)
import Control.Monad (bind)
import Control.Monad.Error.Class (liftMaybe)
import Ctl.Internal.Serialization.Address (addressBech32)
import Data.Array (head, length, range, zip, zipWith)
import Data.BigInt (BigInt)
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
import Seath.Types (BlockhainState(BlockhainState), UserAction(UserAction))
import Test.Examples.DemoShow (dShow)

newtype Leader = Leader KeyWallet

derive instance Newtype Leader _

newtype Participant = Participant KeyWallet

derive instance Newtype Participant _

-- todo: pass action as param?
genUserActions
  :: Array Participant -> Contract (Array (UserAction AdditionAction))
genUserActions ps =
  traverse genAction ps

stateChangePerAction ∷ BigInt
stateChangePerAction = BigInt.fromInt 100

genAction :: Participant -> Contract (UserAction AdditionAction)
genAction (Participant p) =
  withKeyWallet p $ do
    ownUtxos <- liftedM "no UTXOs found" getWalletUtxos
    publicKeyHash <- getPublicKeyHash p
    changeAddress <- liftedM "can't get Change address" $ head <$>
      getWalletAddressesWithNetworkTag
    pure $ UserAction
      { action: AddAmount stateChangePerAction
      , publicKey: publicKeyHash
      , userUTxo: ownUtxos
      , changeAddress
      }

getPublicKeyHash :: KeyWallet -> Contract PubKeyHash
getPublicKeyHash kw = withKeyWallet kw do
  address <- liftedM "can't get the address of KeyWallet" $ head <$>
    getWalletAddresses
  liftMaybe (error "can't get pubKeyHash of KeyWallet") $ toPubKeyHash address

getWalletAddress :: KeyWallet -> Contract String
getWalletAddress kw = withKeyWallet kw do
  netId <- getNetworkId
  let serializableAddr = (unwrap kw).address netId
  pure $ addressBech32 serializableAddr

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
submitChain leader participants txs _log = do
  allSigned <- signTransactions leader (zip participants txs)
  withKeyWallet (unwrap leader) $ traverse submitAndWait allSigned
  where
  submitAndWait balancedAndSignedTransaction = do
    transactionId <- submit balancedAndSignedTransaction
    logInfo' $ "Submited chaned Tx ID: " <> dShow transactionId
    pure $ transactionId

getBlockhainState
  :: forall s
   . Leader
  -> Array Participant
  -> Contract (UtxoMap /\ s)
  -> Contract (BlockhainState s)
getBlockhainState leader participants stateQuery = do
  leaderUTXOs <- withKeyWallet (unwrap leader) $ getWalletUtxos
  usersUTXOs :: _ <- traverse (\p -> withKeyWallet (unwrap p) getWalletUtxos)
    participants
  (sctiptState :: UtxoMap /\ s) <- withKeyWallet (unwrap leader) $ stateQuery
  pure $ BlockhainState { leaderUTXOs, usersUTXOs, sctiptState }

-- getBlockhainState = 
logBlockchainState
  :: forall s. Show s => BlockhainState s -> Contract Unit
logBlockchainState (BlockhainState bchState) = do
  logInfo' "------------------------- BlochainState -------------------------"
  logInfo' $ "utxosAt LEADER: " <> show bchState.leaderUTXOs
  for_ (enumUsers bchState.usersUTXOs) $ \(i /\ us) ->
    logInfo' $ "utxosAt " <> i <> ": " <> show us
  logInfo' $ "utxosAt script: " <> show bchState.sctiptState
  where
  enumUsers :: Array (Maybe UtxoMap) -> Array (Tuple String (Maybe UtxoMap))
  enumUsers ps = zipWith
    (\p i -> (("user-" <> show i) /\ p))
    ps
    (range 1 (length bchState.usersUTXOs))

