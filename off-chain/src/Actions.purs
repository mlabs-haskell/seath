module Actions (class SeathAction, seathToData) where

import Prelude (Unit)

-- TODO : Define what kind of constraints does we need to put here
class SeathAction a where
  seathToData :: a -> Unit
