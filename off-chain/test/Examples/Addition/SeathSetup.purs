module Seath.Test.Examples.Addition.SeathSetup
  ( genAction
  , genUserActions
  , submitChain
  , logBlockchainState
  , getBlockchainState
  , stateChangePerAction
  ) where

import Contract.Address (getWalletAddressesWithNetworkTag)
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
import Control.Applicative (pure)
import Control.Monad (bind)
import Data.Array (head, length, range, zip, zipWith)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Functor ((<$>))
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unit (Unit)
import Prelude (discard, flip, ($))
import Seath.Core.Types (ChangeAddr(ChangeAddr), UserAction(UserAction))
import Seath.Network.Utils (getPublicKeyHash)
import Seath.Test.Examples.Addition.Types (AdditionAction(AddAmount))
import Seath.Test.Types
  ( BlockchainState(BlockchainState)
  , Leader
  , Participant(Participant)
  )

-- todo: pass action as param?
genUserActions
  :: Array (Participant AdditionAction)
  -> Contract (Array (UserAction AdditionAction))
genUserActions ps =
  traverse genAction ps

stateChangePerAction ∷ BigInt
stateChangePerAction = BigInt.fromInt 100

genAction :: Participant AdditionAction -> Contract (UserAction AdditionAction)
genAction (Participant p) =
  withKeyWallet p.wallet $ do
    ownUtxos <- liftedM "no UTxOs found" getWalletUtxos
    publicKeyHash <- withKeyWallet p.wallet getPublicKeyHash
    changeAddress <- liftedM "can't get Change address" $ head <$>
      getWalletAddressesWithNetworkTag
    pure $ UserAction
      { action: AddAmount stateChangePerAction
      , publicKey: publicKeyHash
      , userUTxOs: ownUtxos
      , changeAddress: ChangeAddr changeAddress
      }

signTransactions
  :: forall a
   . Leader a
  -> Array (Participant a /\ FinalizedTransaction)
  -> Contract (Array BalancedSignedTransaction)
signTransactions leader toSign = flip traverse toSign \(participant /\ tx) -> do
  signedByParticipant <- withKeyWallet (unwrap participant).wallet $
    signTransaction tx
  withKeyWallet (unwrap leader).wallet $ signTransaction signedByParticipant

submitChain
  :: forall a
   . Leader a
  -> Array (Participant a)
  -> Array FinalizedTransaction
  -> Contract Unit
  -> Contract (Array TransactionHash)
submitChain leader participants txs log = do
  -- We don't really need leader to sign transactions right now.
  -- This can change if we add a fee to the leader.
  allSigned <- signTransactions leader (zip participants txs)
  withKeyWallet (unwrap leader).wallet $ traverse submitAndWait allSigned
  where
  submitAndWait balancedAndSignedTransaction = do
    log
    -- logInfo' $ "submiting: " <> show balancedAndSignedTransaction
    transactionId <- submit balancedAndSignedTransaction
    -- awaitTxConfirmed transactionId
    logInfo' $ "Submited chaned Tx ID: " <> show transactionId
    pure $ transactionId

getBlockchainState
  :: forall s a
   . Leader a
  -> Array (Participant a)
  -> Contract (UtxoMap /\ s)
  -> Contract (BlockchainState s)
getBlockchainState leader participants stateQuery = do
  leaderUTXOs <- withKeyWallet (unwrap leader).wallet $ getWalletUtxos
  usersUTXOs :: _ <- traverse
    (\p -> withKeyWallet (unwrap p).wallet getWalletUtxos)
    participants
  (sctiptState :: UtxoMap /\ s) <- withKeyWallet (unwrap leader).wallet $
    stateQuery
  pure $ BlockchainState { leaderUTXOs, usersUTXOs, sctiptState }

-- getBlockchainState = 
logBlockchainState
  :: forall s. Show s => BlockchainState s -> Contract Unit
logBlockchainState (BlockchainState bchState) = do
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

