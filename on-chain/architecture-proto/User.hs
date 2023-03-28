module User where

import Types
  ( ActionRequest,
    SignedTx (..),
    SingRequest (..),
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

sendAction :: User -> UserAction -> IO ()
sendAction (User uid hs) act = do
  res <- sendActionToLeader hs (newAction uid act)
  case res of
    Right _ -> putStrLn $ "User " <> uid <> ": sent action successfully"
    Left err ->
      putStrLn $ "User " <> uid <> ": error sending action, leader response: " <> err

acceptSignReq :: UserHandlers -> User -> SingRequest -> IO (Either String SignedTx)
acceptSignReq _hs u sr = do
  if uId u == "user-2"
    then pure (Left "User: user-2 failed to sign")
    else do
      let (SingRequest _ tx) = sr
      putStrLn $ show u <> ": signing " <> show sr
      pure $ Right (SignedTx (uId u) tx)