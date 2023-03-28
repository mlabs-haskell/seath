module Types (ActionRequest, userId, action, newAction) where

import Prelude

type UserId = String

type Action = String

data ActionRequest = ActionRequest
  { userId :: UserId,
    action :: Action
  }
  deriving stock (Show)

newAction = ActionRequest