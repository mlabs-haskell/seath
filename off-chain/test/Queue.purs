module Seath.Test.Queue (main) where

import Control.Applicative (pure)
import Control.Monad (bind)
import Control.Parallel (parSequence_)
import Data.Array (range)
import Data.Either (Either)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Monoid ((<>))
import Data.Newtype (wrap)
import Data.Ring ((+))
import Data.Show (show)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_, runAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import IOQueues (IOQueues, callAsync, new, registerSync)
import Prelude (discard)
import Queue.One (Queue) as One

-- doAction :: Str -> IOQueues One.Queue Int Int -> 

type Stack = IOQueues One.Queue (Ref.Ref Int) Int

handler :: Ref.Ref Int -> Effect Int
handler valueRef = do
  value <- Ref.read valueRef
  log $ "handler called with: " <> show value
  launchAff_ $ delay $ wrap 1000.0
  newValue <- Ref.modify ((+) 1) valueRef
  launchAff_ $ delay $ wrap 1000.0
  log $ "handler new value: " <> show newValue
  pure newValue

makeProcess :: String -> Ref.Ref Int -> Stack -> Aff Unit
makeProcess tag valueRef io = do
  previousValue <- liftEffect $ Ref.read valueRef
  liftEffect $ log $ "calling from: " <> tag <> " with value: " <> show
    previousValue
  result <- callAsync io valueRef
  afterValue <- liftEffect $ Ref.read valueRef
  liftEffect $ log $ tag <> ", previousValue: " <> show previousValue
    <> " , result: "
    <> show result
    <> " , afterValue: "
    <> show afterValue

main :: Effect Unit
main = do
  (io :: Stack) <- new
  -- "IOQueues queue input output" means "using 'queue', take 'input' and make 'output'."

  liftEffect $ registerSync io handler -- attach the handler in Effect
  valueRef <- Ref.new 0

  -- `resolveAff` does nothing - it's needed by `runAff_` - see `Effect.Aff` for details
  let
    resolveAff :: Either _ Unit -> Effect Unit
    resolveAff _ = pure unit

  runAff_ resolveAff do
    parSequence_ $ (\x -> makeProcess (show x) valueRef io) <$> range 0 10
