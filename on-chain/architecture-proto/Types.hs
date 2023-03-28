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
  )
where

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

newAction = ActionRequest

data SingRequest = SingRequest UserId Tx
  deriving stock (Show)