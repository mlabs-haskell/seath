module Test.Examples.DemoShow (class DemoShow, dShow) where

import Contract.Prelude

import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.Transaction
  ( TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  , TransactionOutputWithRefScript
  )
import Contract.Utxos (UtxoMap)
import Contract.Value (valueToCoin')
import Ctl.Internal.Plutus.Types.Transaction (_amount)
import Data.Array as Arr
import Data.Array.NonEmpty as NE
import Data.BigInt as BigInt
import Data.Lens ((^.))
import Data.Map as Map
import Data.UInt (toInt)
import Seath.Test.Examples.Addition.SeathSetup (BlockhainState(BlockhainState))
import Seath.Test.Examples.Addition.Types (AdditionDatum)
import Seath.Test.Examples.Utils (getTypedDatum)
import Seath.Test.TestSetup (RunnerConfig(RunnerConfig))

class DemoShow a where
  dShow :: a -> String

instance runnerConfig :: DemoShow (RunnerConfig s) where
  dShow (RunnerConfig cfg) =
    "leaders: 1, participants: " <> show (NE.length cfg.seathParticipants)

instance blockChainState :: Show s => DemoShow (BlockhainState s) where
  dShow (BlockhainState st) = mconcat
    [ "\n--- Seath state ---"
    , "\nLeader UTXOs:" <> showUtxos st.leaderUTXOs
    , showUsers
    , "\nScript state:" <> showScript
    ]
    where
    showUsers = (flip foldMap) enumUsers $
      \(s /\ utxos) -> "\n" <> s <> showUtxos utxos

    showScript = "\n  " <> "protocol state: " <> show (snd st.sctiptState)
      <> "\n  UTXOs:"
      <> showUtxos (Just $ fst st.sctiptState)

    showUtxos matbeUtxos = case matbeUtxos of
      Nothing -> "Empty"
      Just utxos -> foldMap showTuple
        ( Map.toUnfoldable utxos
            :: Array (TransactionInput /\ TransactionOutputWithRefScript)
        )

    showTuple :: (TransactionInput /\ TransactionOutputWithRefScript) -> String
    showTuple (inp /\ outp) = "\n  - " <> dShow inp <> "\n    " <> dShow outp

    enumUsers :: Array (Tuple String (Maybe UtxoMap))
    enumUsers = Arr.zipWith
      (\p i -> (("user-" <> show i <> " UTXOs:") /\ p))
      st.usersUTXOs
      (Arr.range 1 (Arr.length st.usersUTXOs))

instance txInput :: DemoShow TransactionInput where
  dShow (TransactionInput txInp) =
    let
      (TransactionHash hash) = txInp.transactionId
    in
      byteArrayToHex hash <> " - " <> show (toInt txInp.index)

instance outRef :: DemoShow TransactionOutputWithRefScript where
  dShow to =
    "lovelace: " <> BigInt.toString lovelaces
      <> ", datum: "
      <> datum

    where
    lovelaces = valueToCoin' $ (unwrap to).output ^. _amount
    datum = case getTypedDatum to of
      Left _ -> "Nothing"
      Right (d :: AdditionDatum) -> show d