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
           -- Decks
         , ("/review", with auth handleReviewDeck)
         , ("/review/:id", with auth handleReviewDeck)
         , ("/decks/new", with auth handleNewDeck)
         , ("/decks/list", with auth (handleListDecks Nothing))
         , ("/decks/edit/:id", with auth handleEditDeck)
         , ("/decks/remove/:id", with auth handleRemoveDeck)
           -- Facts
         , ("/facts/new", with auth handleNewFact)
         , ("/facts/list", with auth handleListFacts)
         , ("/facts/edit/:id", with auth handleEditFact)
         , ("/facts/remove/:id", with auth handleRemoveFact)
           -- Fact types
         , ("/fact_types/new", with auth handleNewFactType)
         , ("/fact_types/list", with auth handleListFactTypes)
         , ("/fact_types/edit/:id", with auth handleEditFactType)
         , ("/fact_types/remove/:id", with auth handleRemoveFactType)
           -- Card types
         , ("/fact_types/:fact_type_id/card_types/new", with auth handleNewCardType)
         , ("/fact_types/:fact_type_id/card_types/list", with auth handleListCardTypes)
         , ("/fact_types/:fact_type_id/card_types/edit/:id", with auth handleEditCardType)
         , ("/fact_types/:fact_type_id/card_types/remove/:id", with auth handleRemoveCardType)
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
