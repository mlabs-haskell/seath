module Handlers where

import Control.Concurrent (newEmptyMVar, takeMVar, writeChan)
import Data.Map qualified as Map
import Types
import Prelude

-- handlers required by user
_sendActionToLeader :: Network -> ActionRequest -> IO (Either String ())
_sendActionToLeader (Network leadChan _) ar = do
  response <- newEmptyMVar
  writeChan leadChan (AcceptActionReq response ar)
  resp <- takeMVar response
  case resp of
    AcceptActionResp r -> pure r
    other -> error $ "Unexpected response while sending action to leader: " <> show other

-- handlers required by leader
_sendToUserToSign :: Network -> SingRequest -> IO (Either String SignedTx)
_sendToUserToSign (Network _ userMap) sr = do
  let netUserId = let (SingRequest uid _) = sr in uid
  userChan <-
    maybe
      (error "User not found in network")
      pure
      (Map.lookup netUserId userMap)
  forResponse <- newEmptyMVar
  writeChan userChan (SignTxReq forResponse sr)
  resp <- takeMVar forResponse
  case resp of
    SignTxResp signedTx -> pure signedTx
    other -> error $ "Unexpected response while asking user to sign: " <> show other
