module Seath.Network.Types
  ( Request(Request)
  , Response(Response)
  , SignatureRequestContent(SignatureRequestContent)
  , SignatureResponseContent(SignatureResponseContent)
  , NetworkConfiguration(NetworkConfiguration)
  , SeathMonad
  , SeathHandlers(RealNetworkHandlers, PlutipNetworkHandlers)
  , NetworkError(SubmitError)
  , SignedTransaction(SignedTransaction)
  ) where

import Contract.Transaction (FinalizedTransaction)
import Control.Monad.Reader.Trans (ReaderT)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)

newtype Request a = Request
  { controlNumber :: Int
  , ip :: String
  , body :: a
  }

derive instance Newtype (Request a) _

newtype Response a = Response
  { controlNumber :: Int
  , ip :: String
  , body :: a
  }

derive instance Newtype (Response a) _

-- TODO: Define Handler types
data SeathHandlers = RealNetworkHandlers {} | PlutipNetworkHandlers {}

data NetworkError = SubmitError

derive instance Generic NetworkError _

instance Show NetworkError where
  show = genericShow

newtype SignatureRequestContent = SignatureRequestContent
  {
    -- | Seath `leader` maintains a control of the number of built chains
    -- | Every response corresponding to a chain below a threshold are 
    -- | ignored by the leader.
    chainNumber :: Int
  -- | Index of the transaction in the chain built.
  , index :: Int
  -- | Transaction ready to be signed by the user.
  , transaction :: FinalizedTransaction
  }

newtype SignatureResponseContent = SignatureResponseContent
  { chainNumber :: Int
  , index :: Int
  , transaction :: FinalizedTransaction
  }

newtype NetworkConfiguration = NetworkConfiguration
  { numberOfRequests :: Int
  , timeout :: Int
  , builtChains :: Int
  , handlers :: SeathHandlers
  }

type SeathMonad a = ReaderT NetworkConfiguration Aff a

newtype SignedTransaction = SignedTransaction FinalizedTransaction

derive instance Newtype SignedTransaction _
