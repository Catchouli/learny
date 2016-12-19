{-# LANGUAGE OverloadedStrings #-}

module Util where

import Application
import Control.Conditional
import Snap.Snaplet.Auth
import Snap
import Heist
import Snap.Snaplet.Heist
import qualified Heist.Interpreted as I
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.String
import Data.String.Conversions
import Snap.Snaplet.Auth.Backends.Persistent ( SnapAuthUserId, userDBKey )

-- |
-- Requires the user to log in before running the specified handler
-- TODO: make this redirect the user and run the handler afterwards
requireLogin :: Handler App (AuthManager App) () -> Handler App (AuthManager App) ()
requireLogin action = ifM isLoggedIn action (redirect "/")


-- | Render a template with a success message
renderWithSuccess :: BS.ByteString -> [T.Text] -> Handler App (AuthManager App) ()
renderWithSuccess url errs = heistLocal (I.bindSplices splices) (render url)
  where
    splices = "success" ## I.mapSplices (I.runChildrenWith . splice) errs
    splice e = "message" ## I.textSplice e


-- | Render a template with an error message

renderWithErrors :: BS.ByteString -> [T.Text] -> Handler App (AuthManager App) ()
renderWithErrors url errs = heistLocal (I.bindSplices splices) (render url)
  where
    splices = "errors" ## I.mapSplices (I.runChildrenWith . splice) errs
    splice e = "message" ## I.textSplice e


-- | Render a template with an error or a success message depending on an Either value

renderWithEither :: BS.ByteString -> Either [T.Text] [T.Text] -> Handler App (AuthManager App) ()
renderWithEither url res = heistLocal (I.bindSplices splices) (render url)
  where
    t = case res of Left _ -> "errors"; Right _ -> "success"
    messages = case res of Left a -> a; Right b -> b
    splices = t ## I.mapSplices (I.runChildrenWith . splice) messages
    splice e = "message" ## I.textSplice e


-- |
-- Render a template with an error or a success message, evaluating a different handler in each case
-- CW: Unused, and I can't remember the use. Possibly queued for removal.

renderEither :: BS.ByteString
                 -> Either
                      (Handler App (AuthManager App) ([T.Text]))
                      (Handler App (AuthManager App) ([T.Text]))
                 -> Handler App (AuthManager App) ()
renderEither url res = do result <- case res of Left a -> a; Right b -> b
                          let t = case res of Left _ -> "errors"; Right _ -> "success"
                          renderPage t result
  where
    renderPage t r = heistLocal (I.bindSplices (splices t r)) (render url)
    splices t r = t ## I.mapSplices (I.runChildrenWith . splice) r
    splice e = "message" ## I.textSplice e


-- | Converts a maybe value to an either value with the specified left value for Nothing

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a b = case b of
                         Just c  -> Right c
                         Nothing -> Left a


-- | Converts a maybe value to an eitherT value with the specified left value for Nothing

maybeToEitherT :: a -> Maybe b -> EitherT a (Handler App (AuthManager App)) b
maybeToEitherT a b = case b of
                         Just c  -> right c
                         Nothing -> left a


-- | Exit an eitherT with the specified left value depending on a boolean value

require :: Monad m => Bool -> e -> EitherT e m ()
require b str = if b then return () else left str


-- | Generates a message splice (t = "errors" or "success" usually), which is a list splice
-- with a single nested message

messageSplice :: Monad m => k -> String -> MapSyntax k (HeistT m m Template)
messageSplice t message = t ## I.mapSplices (I.runChildrenWith . ("message" ##)) [I.textSplice . T.pack $ message]


-- | Generates a message splice of type "errors" or "success" depending on the specified either value

messageSpliceE :: (IsString s, Monad m) => Either String String -> MapSyntax s (HeistT m m Template)
messageSpliceE message = t ## I.mapSplices (I.runChildrenWith . ("message" ##)) [I.textSplice . T.pack $ msg]
  where
    msg = case message of Left a -> a; Right b -> b
    t = case message of Left _ -> "errors"; Right _ -> "success"


-- Turns 'message' into a splice based on whether it's empty (no message),
-- or whether it contans an error (left) or a success message (right)
-- Builds the splices for listing decks
messageSplices :: (Monad m, IsString k) => Maybe (Either String String) -> MapSyntax k (HeistT m m Template)
messageSplices message = case message of
                            Nothing -> mempty
                            Just (Left err) -> messageSplice "errors" err
                            Just (Right msg) -> messageSplice "success" msg


-- | Gets the current user ID or nothing if there's no user logged in

currentUserId :: Handler App (AuthManager App) (Maybe SnapAuthUserId)
currentUserId = do user <- currentUser
                   return $ user >>= userDBKey


-- | Gets the current user id as an eitherT

currentUserIdE :: EitherT String (Handler App (AuthManager App)) SnapAuthUserId
currentUserIdE = (lift currentUserId) >>= maybeToEitherT "Not logged in."


-- | Get a parameter, returning the specified left value if not found

getParamE :: String -> String -> EitherT String (Handler App (AuthManager App)) String
getParamE n s = (lift . getParam $ cs n) >>= maybeToEitherT s >>= return . cs
