module Seath.Test.Examples.Addition.SeathSetup
  ( Leader(Leader)
  , Participant(Participant)
  , genAction
  , genUserActions
  , getPublicKeyHash
  , submitChain
  ) where

import Contract.Address (PubKeyHash, getWalletAddresses, toPubKeyHash)
import Contract.Monad (Contract, liftedM)
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
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (error)
import Prelude (discard, flip, ($))
import Seath.Test.Examples.Addition.Types (AdditionAction(AddAmount))
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
    pure $ UserAction
      { action: AddAmount (BigInt.fromInt 1) -- FIXME: hardcoded
      , publicKey: publicKeyHash
      , userUTxo: ownUtxos
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
  -> Contract (Array TransactionHash)
submitChain leader participants txs = do
  allSigned <- signTransactions leader (zip participants txs)
  withKeyWallet (unwrap leader) $ traverse submitAndWait allSigned
  where
  submitAndWait balancedAndSignedTransaction = do
    transactionId <- submit balancedAndSignedTransaction
    awaitTxConfirmed transactionId
    pure $ transactionId

