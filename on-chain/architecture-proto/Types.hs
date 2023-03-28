module Types
  ( SignedTx (..),
    Tx,
    SingRequest (..),
    ActionRequest,
    UserId,
    UserAction,
    userId,
    action,
    newAction,
    Network (..),
    NetMessage (..),
  )
where

import Control.Concurrent (Chan, MVar)
import Data.Map (Map)
import Prelude

type UserId = String

type UserAction = String

type Tx = String

data SignedTx = SignedTx UserId Tx
  deriving stock (Show)

data ActionRequest = ActionRequest
  { userId :: UserId,
    action :: UserAction
  }
  deriving stock (Show)

newAction :: UserId -> UserAction -> ActionRequest
newAction = ActionRequest

data SingRequest = SingRequest UserId Tx
  deriving stock (Show)

-- network simulation
data NetMessage
  = AcceptActionReq (MVar NetMessage) ActionRequest
  | AcceptActionResp (Either String ())
  | SignTxReq (MVar NetMessage) SingRequest
  | SignTxResp (Either String SignedTx)
  deriving stock (Show)

data Network = Network
  { toLeader :: Chan NetMessage,
    toUsers :: Map UserId (Chan NetMessage)
  }

instance Show (MVar NetMessage) where
  show _ = "<sender>"
