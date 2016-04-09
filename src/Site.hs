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
import Snap.Util.FileServe
import Application
import Authentication
import Snap.Snaplet.PostgresqlSimple


-- The application's routes
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLogin)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/static",   serveDirectory "static")
         ]


-- The application's slices
--slices :: HasHeist b => [(Text, HeistNoClass.SnapletSplice b v)]
--slices = [
--         ]


-- The application snaplet
app :: SnapletInit App App
app = makeSnaplet "learny" "Learny description" Nothing $ do
    -- Initialise heist
    h <- nestSnaplet "" heist $ heistInit "templates"

    -- Initialise database
    d <- nestSnaplet "db" db pgsInit

    -- Initialise session manager
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "private/site_key.txt" "sess" (Just 3600)

    -- Initialise auth manager
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    addAuthSplices h auth

    -- Initialise routes
    addRoutes routes

    return $ App h d s a

