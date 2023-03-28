module User where

import Types
  ( ActionRequest,
    SignedTx (SignedTx),
    SingRequest (SingRequest),
    UserAction,
    UserId,
    newAction,
  )
import Prelude

data UserHandlers = UserHandlers
  { sendActionToLeader :: ActionRequest -> IO (Either String ())
  }

data User = User
  { uId :: UserId,
    uHandlers :: UserHandlers
  }

instance Show User where
  show = uId

newUser :: UserHandlers -> UserId -> IO User
newUser hs uid = pure $ User uid hs

emitAction :: User -> UserAction -> IO ()
emitAction (User uid hs) act = do
  putStrLn $ "User " <> uid <> ": performing action"
  res <- sendActionToLeader hs (newAction uid act)
  case res of
    Right _ -> putStrLn $ "User " <> uid <> ": sent action successfully"
    Left err ->
      putStrLn $ "User " <> uid <> ": error sending action, leader response: " <> err

signChainedTx :: User -> SingRequest -> IO (Either String SignedTx)
signChainedTx u sr = do
  if uId u == "user-2"
    then pure (Left "Rejecting to sign")
    else do
      let (SingRequest _ tx) = sr
      putStrLn $ show u <> ": signing " <> show sr
      pure $ Right (SignedTx (uId u) tx)
