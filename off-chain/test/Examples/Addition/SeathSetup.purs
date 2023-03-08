module Seath.Test.Examples.Addition.SeathSetup
  ( Leader(Leader)
  , Participant(Participant)
  , genAction
  , genUserActions
  , getPublicKeyHash
  , submitChain
  ) where

import Prelude

import Contract.Address
  ( PubKeyHash
  , getWalletAddresses
  , toPubKeyHash
  )
import Contract.Monad (Contract, liftedM)
import Contract.Transaction (FinalizedTransaction)
import Contract.Utxos (getWalletUtxos)
import Contract.Wallet (withKeyWallet)
import Contract.Wallet.Key
  ( KeyWallet
  )
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head)
import Data.BigInt as BigInt
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Effect.Aff (error)
import Seath.Test.Examples.Addition.Types (AdditionAction(AddAmount))
import Seath.Types (UserAction(..))

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

-- todo
submitChain :: Leader -> Array FinalizedTransaction -> Contract Unit
submitChain (Leader w) _txs =
  withKeyWallet w do
    pure unit
