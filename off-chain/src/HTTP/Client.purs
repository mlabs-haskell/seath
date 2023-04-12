module Seath.HTTP.Client
  ( UserClient
  , mkUserClient
  ) where

import Aeson (class EncodeAeson)
import Data.Either (Either)
import Effect.Aff (Aff)
import Payload.Client (ClientError, Options)
import Payload.Client as Client
import Payload.Headers (Headers)
import Payload.ResponseTypes (Empty, Response)
import Payload.Spec (Spec)
import Seath.HTTP.Spec (LeaderRoutes)
import Seath.HTTP.Spec as Spec
import Seath.HTTP.Types (IncludeRequest, JSend, SendSignedRequest, UID)
import Seath.Network.Types (ActionStatus, GetStatusError, IncludeActionError)
import Type.Proxy (Proxy)

type UserClient a =
  { leader ::
      { includeAction ::
          { body :: IncludeRequest a }
          -> Aff
               (Either ClientError (Response (JSend IncludeActionError UID)))
      , includeAction_ ::
          { extraHeaders :: Headers }
          -> { body :: IncludeRequest a }
          -> Aff
               (Either ClientError (Response (JSend IncludeActionError UID)))
      , actionStatus ::
          { params :: { uid :: UID } }
          -> Aff
               ( Either ClientError
                   (Response (JSend GetStatusError ActionStatus))
               )
      , actionStatus_ ::
          { extraHeaders :: Headers }
          -> { params :: { uid :: UID } }
          -> Aff
               ( Either ClientError
                   (Response (JSend GetStatusError ActionStatus))
               )
      , acceptSignedTransaction ::
          { body :: SendSignedRequest }
          -> Aff
               (Either ClientError (Response (JSend String String)))
      , acceptSignedTransaction_ ::
          { extraHeaders :: Headers }
          -> { body :: SendSignedRequest }
          -> Aff
               (Either ClientError (Response (JSend String String)))
      , refuseToSign ::
          { params :: { uid :: UID } }
          -> Aff
               (Either ClientError (Response Empty))
      , refuseToSign_ ::
          { extraHeaders :: Headers }
          -> { params :: { uid :: UID } }
          -> Aff
               (Either ClientError (Response Empty))
      }
  }

mkUserClient
  :: forall a
   . EncodeAeson a
  => Proxy a
  -> String
  -> UserClient a
mkUserClient _ serverUrl =
  let
    opts :: Options
    opts = Client.defaultOpts { baseUrl = serverUrl }

    -- Do not erase type here, if things go wrong with types, things can go quite 
    -- dark without this.
    mkClient
      :: ( Options
           -> Spec
                { leader :: LeaderRoutes a
                }
           -> { leader ::
                  { includeAction ::
                      { body :: IncludeRequest a }
                      -> Aff
                           ( Either ClientError
                               (Response (JSend IncludeActionError UID))
                           )
                  , includeAction_ ::
                      { extraHeaders :: Headers }
                      -> { body :: IncludeRequest a }
                      -> Aff
                           ( Either ClientError
                               (Response (JSend IncludeActionError UID))
                           )
                  , actionStatus ::
                      { params :: { uid :: UID } }
                      -> Aff
                           ( Either ClientError
                               (Response (JSend GetStatusError ActionStatus))
                           )
                  , actionStatus_ ::
                      { extraHeaders :: Headers }
                      -> { params :: { uid :: UID } }
                      -> Aff
                           ( Either ClientError
                               (Response (JSend GetStatusError ActionStatus))
                           )
                  , acceptSignedTransaction ::
                      { body :: SendSignedRequest }
                      -> Aff
                           (Either ClientError (Response (JSend String String)))
                  , acceptSignedTransaction_ ::
                      { extraHeaders :: Headers }
                      -> { body :: SendSignedRequest }
                      -> Aff
                           (Either ClientError (Response (JSend String String)))
                  , refuseToSign ::
                      { params :: { uid :: UID } }
                      -> Aff
                           (Either ClientError (Response Empty))
                  , refuseToSign_ ::
                      { extraHeaders :: Headers }
                      -> { params :: { uid :: UID } }
                      -> Aff
                           (Either ClientError (Response Empty))
                  }
              }
         )
    mkClient = Client.mkClient
  in
    mkClient opts (Spec.spec)
