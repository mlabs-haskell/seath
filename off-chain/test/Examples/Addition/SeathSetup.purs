module Test.Examples.Addition.SeathSetup
  ( Leader
  , Participant
  , genAction
  , genUserActions
  , mkSetup
  , submitChain
  ) where

import Prelude

import Contract.Monad (Contract, liftedM)
import Contract.Prelude (Unit)
import Contract.Transaction (FinalizedTransaction(..), PublicKey)
import Contract.Utxos (getWalletUtxos)
import Contract.Wallet (PrivatePaymentKey(..), withKeyWallet)
import Contract.Wallet.Key (KeyWallet, keyWalletPrivatePaymentKey, publicKeyFromPrivateKey)
import Ctl.Internal.Cardano.Types.Transaction (mkFromCslPubKey)
import Data.BigInt as BigInt
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Seath.Test.Examples.Addition.Types (AdditionAction(AddAmount))
import Seath.Types (UserAction(..))

newtype Leader = Leader KeyWallet
-- derive instance Newtype Leader _

newtype Participant = Participant KeyWallet

type SeathSetup =
  { leader :: Leader
  , participants :: Array Participant
  }

mkSetup :: (KeyWallet /\ KeyWallet /\ KeyWallet) -> SeathSetup
mkSetup (a /\ b /\ c) =
  { leader: Leader a
  , participants: map Participant [ b, c ]
  }

-- todo: pass action as param?
genUserActions :: SeathSetup -> Contract (Array (UserAction AdditionAction))
genUserActions setup =
  traverse genAction setup.participants

genAction :: Participant -> Contract (UserAction AdditionAction)
genAction (Participant p) =
  withKeyWallet p $ do
    ownUtxos <- liftedM "no UTXOs found" getWalletUtxos
    pure $ UserAction
      { action: AddAmount (BigInt.fromInt 1) -- FIXME: hardcoded
      , publicKey: getPubKey p
      , userUTxo: ownUtxos
      }

getPubKey :: KeyWallet -> PublicKey
getPubKey kw =
  let
    (PrivatePaymentKey prv) = keyWalletPrivatePaymentKey kw
  in
    mkFromCslPubKey $ publicKeyFromPrivateKey prv


-- todo
submitChain :: Leader -> Array FinalizedTransaction -> Contract Unit
submitChain (Leader w ) _txs = 
    withKeyWallet w do
      pure unit