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
--  ( handleShowCards
--  , handleNewCard
--  , migrateCards
--  )
where

import Application
import Util
import Database.Persist.Sql
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
import qualified Data.ByteString as BS
import Data.Int


-- | The cards table / structure

share [mkPersist sqlSettings, mkMigrate "migrateCards"] [persistLowerCase|
UserSrsNote
  user_id T.Text
UserSrsNoteFields
  user_id T.Text
  note_id Int
  field_id Int
  name T.Text
UserSrsCard
  user_id T.Text
  front T.Text
  back T.Text
  ease Int
  interval Int
|]


-- | A handler that allows the current user to add new cards

handleNewCard :: Handler App (AuthManager App) ()
handleNewCard = method GET (renderNewCard Nothing)
            <|> method POST addNewCard
  where
    renderNewCard err = requireLogin $ renderWithError "new_card" err
    addNewCard = updateOrCreateCard Nothing Nothing success failure
    success = renderNewCard (Just "Added new card")
    failure = renderNewCard (Just "Failed to add card")


-- | A handler that allows a card to be edited

handleEditCard :: Handler App (AuthManager App) ()
handleEditCard = method GET renderEditCard
             <|> method POST editCard
  where
    renderEditCard = requireLogin $ do
      user <- currentUser
      let user_id = user >>= userId
      card_id <- getParam "id"
      let card_key = toSqlKey . read . cs <$> card_id :: Maybe (Key UserSrsCard)
      let query = (\x y -> selectList [UserSrsCardId ==. x, UserSrsCardUser_id ==. y] []) <$> card_key <*> (unUid <$> user_id)
      result <- maybe (return []) (\q -> runPersist q) query
      let card = Just $ head $ map E.entityVal result
      let card = case map E.entityVal result of
                    a:_ -> Just a
                    [] -> Nothing
      let user = userSrsCardUser_id <$> card
      let splices = mconcat [ ("front" ##) . I.textSplice . userSrsCardFront <$> card
                            , ("back" ##) . I.textSplice . userSrsCardBack <$> card
                            ]
      maybe (renderWithError "new_card" (Just "Failed to find specified card"))
            (\s -> heistLocal (I.bindSplices s) (render "edit_card"))
            splices
    -- commented version will currently create the card as new due to nothing/nothing
    -- needs to be updated to update the card, but only if it belongs to the right user
    editCard = return ()--updateOrCreateCard Nothing Nothing success failure
    success = renderWithError "edit_card" (Just "Card successfully updated")
    failure = renderWithError "new_card" (Just "Failed to update card")


-- | Updates or creates a card from the given parameters

updateOrCreateCard :: Maybe (Key UserSrsCard)
                   -> Maybe UserSrsCard
                   -> Handler App (AuthManager App) ()
                   -> Handler App (AuthManager App) ()
                   -> Handler App (AuthManager App) ()
updateOrCreateCard existingKey existingCard success failure = do
  card <- do
    user <- currentUser
    let user_id = user >>= userId
    front <- getParam "front"
    back <- getParam "back"
    return $ UserSrsCard
               <$> (unUid <$> user_id)
               <*> (cs <$> front)
               <*> (cs <$> back)
               <*> return 250
               <*> return 0
  maybe (failure) (\x -> (runPersist . insertOrUpdateCard existingKey $ x) >> success) card


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

insertOrUpdateCard :: MonadIO m => Maybe (Key UserSrsCard) -> UserSrsCard -> E.SqlPersistT m (Key UserSrsCard)
insertOrUpdateCard key card = maybe (insert card) (\k -> replace k card >> return k) key


-- | Selects the given user's cards

selectUserCards :: MonadIO m => UserId -> E.SqlPersistT m [UserSrsCard]
selectUserCards user_id = do
  cards <-
    E.select $
    E.from $ \card -> do
      E.where_ (card E.^. UserSrsCardUser_id E.==. E.val (unUid user_id))
      return card
  return $ E.entityVal <$> cards
