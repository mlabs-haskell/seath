module Seath.Network.Types
  ( Request(Request)
  , Response(Response)
  , SignatureRequestContent(SignatureRequestContent)
  , SignatureResponseContent(SignatureResponseContent)
  , SeathMonad
  , SeathHandlers(RealNetworkHandlers, PlutipNetworkHandlers)
  , NetworkError(SubmitError)
  , SignedTransaction(SignedTransaction)
  , NodeStateLeader(NodeStateLeader)
  , NodeStateUser(NodeStateUser)
  , NodeInformation(NodeInformation)
  , NodeConfiguration(NodeConfiguration)
  , Node(NodeUser, NodeLeader)
  , Ip
  ) where

import Contract.Address (PubKeyHash)
import Contract.Transaction (FinalizedTransaction)
import Control.Alternative (pure)
import Control.Monad (bind)
import Control.Monad.Reader.Trans (ReaderT)
import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt)

-- TODO: Find the right type for this even is it's just a newtype over string
type Ip = String

newtype Request a = Request
  { controlNumber :: Int
  , ip :: Ip
  , body :: a
  }

derive instance Newtype (Request a) _

newtype Response a = Response
  { controlNumber :: Int
  , ip :: Ip
  , body :: a
  }

derive instance Newtype (Response a) _

-- TODO: This maybe need to be just a record of functions, but 
-- for now we haven't defined it.
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

-- | We split data in three categories
-- | - Configuration, most functions won't perform changes on it.
-- | - State, we would keep changing it a lot in functions.
-- | - Information, read only things. 
newtype NodeConfiguration = NodeConfiguration
  { timeout :: Int
  , handlers :: SeathHandlers
  , port :: Int
  , pubKeyHash :: PubKeyHash
  }

derive instance Newtype NodeConfiguration _
derive instance Generic NodeConfiguration _

newtype NodeInformation = NodeInformation { ip :: Ip }

derive instance Newtype NodeInformation _
derive instance Generic NodeInformation _

instance Show NodeInformation where
  show = genericShow

newtype NodeStateLeader = NodeStateLeader
  { numberOfRequests :: Int
  , numberOfBuiltChains :: Int
  }

derive instance Newtype NodeStateLeader _
derive instance Generic NodeStateLeader _

instance Show NodeStateLeader where
  show = genericShow

instance Arbitrary NodeStateLeader where
  arbitrary = do
    numberOfRequests <- chooseInt 0 100000
    numberOfBuiltChains <- chooseInt 0 100000
    pure $ NodeStateLeader { numberOfRequests, numberOfBuiltChains }

newtype NodeStateUser = NodeStateUser
  { numberOfRequests :: Int
  }

derive instance Newtype NodeStateUser _
derive instance Generic NodeStateUser _

instance Show NodeStateUser where
  show = genericShow

instance Arbitrary NodeStateUser where
  arbitrary = do
    numberOfRequests <- chooseInt 0 100000
    pure $ NodeStateUser { numberOfRequests }

data Node
  = NodeUser
      { configuration :: NodeConfiguration
      , state :: NodeStateUser
      , information :: NodeInformation
      }
  | NodeLeader
      { configuration :: NodeConfiguration
      , state :: NodeStateLeader
      , information :: NodeInformation
      }

newtype SignedTransaction = SignedTransaction FinalizedTransaction

derive instance Newtype SignedTransaction _

type SeathMonad a = ReaderT Node Aff a
