{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

import Control.Lens
import Data.ByteString (ByteString)
import Database.Persist.Sql
import Snap.Snaplet
import Snap.Snaplet.Persistent
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.Persistent
import Snap.Snaplet.Heist
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe
import Config
import Application
import Authentication
import Cards


-- The application's routes
routes :: [(ByteString, Handler App App ())]
routes = [ -- Authentication
           ("/login",    with auth handleLogin)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
           -- Cards
         , ("/cards/new", with auth handleNewCard)
         , ("/cards/list", with auth handleShowCards)
           -- Static data
         , ("/static",   serveDirectory "static")
         ]


-- The application snaplet
app :: SnapletInit App App
app = makeSnaplet "learny" "Learny description" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    d <- nestSnaplet "db" db $ initPersist (mapM_ runMigrationUnsafe [migrateAuth, migrateCards])
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager siteKeyPath "sess" (Just authExpiry)
    a <- nestSnaplet "auth" auth $ initPersistAuthManager sess (persistPool $ view snapletValue d)

    addRoutes routes
    addAuthSplices h auth

    return $ App h d s a
