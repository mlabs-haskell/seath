module Seath.HTTP.UserHandlers (mkHttpHandlers) where

import Contract.Prelude

import Aeson (class EncodeAeson, decodeJsonString)
import Data.UUID (UUID, parseUUID)
import Effect.Aff (Aff, error, throwError)
import Payload.ResponseTypes (Response(Response))
import Seath.Common.Types (UID(UID))
import Seath.Core.Types (UserAction)
import Seath.HTTP.Client (UserClient)
import Seath.HTTP.Types
  ( IncludeRequest(IncludeRequest)
  , SendSignedRequest(SendSignedRequest)
  )
import Seath.Network.Types
  ( AcceptSignedTransactionError
  , ActionStatus
  , IncludeActionError
  , NetworkHandlers
  , SendSignedTransaction
  )

mkHttpHandlers
  :: forall a
   . EncodeAeson a
  => UserClient a
  -> NetworkHandlers a
mkHttpHandlers client =
  { submitToLeader: handleSendAction client
  , sendSignedToLeader: handleSendSignedToLeader client
  , refuseToSign: handleRefuseToSign client
  , getActionStatus: handleGetStatus client
  }

handleSendAction
  :: forall a
   . EncodeAeson a
  => UserClient a
  -> UserAction a
  -> Aff (Either IncludeActionError UUID)
handleSendAction client action = do
  res <- client.leader.includeAction
    { body: IncludeRequest action }
  case res of
    Right resp -> do
      convertResonse resp
    Left r -> throwError
      (error $ "Leader failed to respond to send action: " <> show r)
  where
  convertResonse (Response r) =
    if (r.body.status == "success") then
      maybe (throwError $ error "Can't parse request ID") (Right >>> pure)
        (parseUUID r.body.data)
    else either (show >>> error >>> throwError) (Left >>> pure)
      (decodeJsonString r.body.data)

handleSendSignedToLeader
  :: forall a
   . UserClient a
  -> SendSignedTransaction
  -> Aff (Either AcceptSignedTransactionError Unit)
handleSendSignedToLeader client sendSig = do
  res <- client.leader.acceptSignedTransaction
    { body: SendSignedRequest sendSig }
  case res of
    -- FIXME: leader responds witn Unit always
    -- but client expects AcceptSignedTransactionError as well
    Right _resp -> pure $ Right unit
    Left e -> throwError (error $ show e)

handleRefuseToSign
  :: forall a
   . UserClient a
  -> UUID
  -> Aff Unit
handleRefuseToSign client uuid = do
  res <-
    client.leader.refuseToSign
      { params: { uid: UID uuid } }
  case res of
    Right _ -> pure unit
    Left r -> throwError (error $ show r)

handleGetStatus
  :: forall a
   . UserClient a
  -> UUID
  -> Aff ActionStatus
handleGetStatus client uuid = do
  res <-
    client.leader.actionStatus
      { params: { uid: UID uuid } }
  case res of
    Right resp -> convertResonse resp
    Left r -> throwError (error $ "Leader failed to respond: " <> show r)
  where
  convertResonse (Response r) =
    if (r.body.status == "success") then
      either (show >>> error >>> throwError) pure
        (decodeJsonString r.body.data)
    else either (show >>> error >>> throwError) (error >>> throwError)
      (decodeJsonString r.body.data)
