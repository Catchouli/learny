{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

import Data.ByteString (ByteString)
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.PostgresqlSimple
import Snap.Snaplet.Heist
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Snaplet.PostgresqlSimple
import Snap.Util.FileServe
import Config
import Application
import Authentication


-- The application's routes
routes :: [(ByteString, Handler App App ())]
routes = [
           ("/login",    with auth handleLogin)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/static",   serveDirectory "static")
         ]


-- The application snaplet
app :: SnapletInit App App
app = makeSnaplet "learny" "Learny description" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    d <- nestSnaplet "db" db pgsInit
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager siteKeyPath "sess" (Just authExpiry)
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d

    addRoutes routes
    addAuthSplices h auth

    return $ App h d s a

