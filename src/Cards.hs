{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Cards
  ( handleShowCards
  , handleNewCard
  , migrateCards
  )
where

import Application
import Util
import Database.Persist.TH
import Control.Monad.IO.Class (MonadIO)
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Heist
import Snap
import Snap.Snaplet.Persistent
import Control.Applicative
import qualified Heist.Interpreted as I
import qualified Data.Text as T
import Database.Persist
import qualified Database.Esqueleto as E
import Data.String.Conversions


-- | The cards table / structure

share [mkPersist sqlSettings, mkMigrate "migrateCards"] [persistLowerCase|
UserSrsCard
  user_id T.Text
  front T.Text
  back T.Text
  ease Int
  interval Int
|]


-- | A handler that allows the current user to add new cards

handleNewCard :: Handler App (AuthManager App) ()
handleNewCard = method GET (renderNewCard Nothing) <|> method POST addNewCard
  where
    addNewCard = do
      user <- currentUser
      let user_id = user >>= userId
      front <- getParam "front"
      back <- getParam "back"
      let card = UserSrsCard
                    <$> (unUid <$> user_id)
                    <*> (cs <$> front)
                    <*> (cs <$> back)
                    <*> return 250
                    <*> return 0
      maybe (return ()) (runPersist . insertNewCard) card
      renderNewCard (Just "Added new card")


-- | Renders the new card page with an optional error

renderNewCard :: Maybe T.Text -> Handler App (AuthManager App) ()
renderNewCard newCardError = requireLogin $ newCardView
  where
    newCardView = heistLocal (I.bindSplices errs) $ render "new_card"
    errs = maybe mempty splice newCardError
    splice err = "newCardError" ## I.textSplice err


-- | A handler that shows the current user's cards
-- | route: "cards/list"

handleShowCards :: Handler App (AuthManager App) ()
handleShowCards = requireLogin $ do
  user <- currentUser
  cards <- maybe (return []) (runPersist . selectUserCards) (user >>= userId)
  renderWithSplices "cards" (splices cards)
  where
    splices cards = "cards" ## I.mapSplices (I.runChildrenWith . splicesFromCard) cards
    splicesFromCard c = do
      "front" ## I.textSplice ((userSrsCardFront c))
      "back" ## I.textSplice ((userSrsCardBack c))


-- | Add a new card to the database

insertNewCard :: MonadIO m => UserSrsCard -> E.SqlPersistT m ()
insertNewCard card = insert card >> return ()


-- | Selects the given user's cards

selectUserCards :: MonadIO m => UserId -> E.SqlPersistT m [UserSrsCard]
selectUserCards user_id = do
  cards <-
    E.select $
    E.from $ \card -> do
      E.where_ (card E.^. UserSrsCardUser_id E.==. E.val (unUid user_id))
      E.limit 3
      return card
  return $ E.entityVal <$> cards
