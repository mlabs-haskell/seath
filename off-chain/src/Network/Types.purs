module Seath.Network.Types
  ( Request(Request)
  , Response(Response)
  , SignatureRequestContent(SignatureRequestContent)
  , SignatureResponseContent(SignatureResponseContent)
  , NetworkConfiguration(NetworkConfiguration)
  , SeathMonad
  , SeathHandlers(RealNetworkHandlers, PlutipNetworkHandlers)
  , NetworkError
  ) where

import Contract.Transaction (FinalizedTransaction)
import Control.Monad.Reader.Trans (ReaderT)
import Effect.Aff (Aff)

newtype Request a = Request
  { controlNumber :: Int
  , ip :: String
  , body :: a
  }

newtype Response a = Response
  { controlNumber :: Int
  , ip :: String
  , body :: a
  }

-- TODO: Define Handler types
data SeathHandlers = RealNetworkHandlers | PlutipNetworkHandlers

data NetworkError = SubmitError

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
