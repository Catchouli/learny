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
import Control.Monad.Trans.Either
import Data.String

requireLogin :: Handler App (AuthManager App) () -> Handler App (AuthManager App) ()
requireLogin action = ifM isLoggedIn action (redirect "/")

renderWithSuccess :: BS.ByteString -> [T.Text] -> Handler App (AuthManager App) ()
renderWithSuccess url errs = heistLocal (I.bindSplices splices) (render url)
  where
    splices = "success" ## I.mapSplices (I.runChildrenWith . splice) errs
    splice e = "message" ## I.textSplice e

renderWithErrors :: BS.ByteString -> [T.Text] -> Handler App (AuthManager App) ()
renderWithErrors url errs = heistLocal (I.bindSplices splices) (render url)
  where
    splices = "errors" ## I.mapSplices (I.runChildrenWith . splice) errs
    splice e = "message" ## I.textSplice e

renderWithEither :: BS.ByteString -> Either [T.Text] [T.Text] -> Handler App (AuthManager App) ()
renderWithEither url res = heistLocal (I.bindSplices splices) (render url)
  where
    t = case res of Left _ -> "errors"; Right _ -> "success"
    messages = case res of Left a -> a; Right b -> b
    splices = t ## I.mapSplices (I.runChildrenWith . splice) messages
    splice e = "message" ## I.textSplice e

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

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a b = case b of
                         Just c  -> Right c
                         Nothing -> Left a

maybeToEitherT :: a -> Maybe b -> EitherT a (Handler App (AuthManager App)) b
maybeToEitherT a b = case b of
                         Just c  -> right c
                         Nothing -> left a

require :: Monad m => Bool -> e -> EitherT e m ()
require b str = if b then return () else left str

messageSplice :: Monad m => k -> String -> MapSyntax k (HeistT m m Template)
messageSplice t message = t ## I.mapSplices (I.runChildrenWith . ("message" ##)) [I.textSplice . T.pack $ message]

messageSpliceE :: (IsString s, Monad m) => Either String String -> MapSyntax s (HeistT m m Template)
messageSpliceE message = t ## I.mapSplices (I.runChildrenWith . ("message" ##)) [I.textSplice . T.pack $ msg]
  where
    msg = case message of Left a -> a; Right b -> b
    t = case message of Left _ -> "errors"; Right _ -> "success"
