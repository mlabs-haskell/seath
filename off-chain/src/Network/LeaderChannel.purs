module Seath.Network.LeaderChannel where

import Control.Applicative (pure)
import Control.Monad (bind)
import Control.Parallel (parSequence_)
import Data.Array (range)
import Data.Either (Either(..))
import Data.Function (($), (<<<))
import Data.Functor ((<$>))
import Data.Monoid ((<>))
import Data.Newtype (unwrap, wrap)
import Data.Ord ((<))
import Data.Ring ((+))
import Data.Show (show)
import Data.UUID (UUID, genUUID)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_, runAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import IOQueues (IOQueues, callAsync, new, registerSync)
import Prelude (discard)
import Queue.One (Queue) as One
import Seath.Core.Types (UserAction)
import Seath.Network.OrderedMap (OrderedMap, length, push)
import Seath.Network.Types
  ( IncludeActionError(RejectedServerBussy)
  , LeaderNode
  , LeaderServerStateInfo
  , LeaderState
  )
import Undefined (undefined)

type ResponseQueue = IOQueues One.Queue (Ref.Ref Int) Int

newLeaderStateInfo :: forall a. LeaderNode a -> Effect LeaderServerStateInfo
newLeaderStateInfo = undefined
