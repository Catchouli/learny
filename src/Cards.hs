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

import Safe
import Application
import Util
import Database.Persist.Sql
import Database.Persist.TH
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Heist
import Snap
import Snap.Snaplet.Persistent
import Control.Applicative
import Control.Category ((>>>))
import Data.String.Conversions
import Snap.Snaplet.Auth.Backends.Persistent
import Control.Monad.Trans
import Control.Monad.Trans.Either
import qualified Heist.Interpreted as I
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS


-- | The cards table / structure
-- | The following user-defined types are controlled here:
-- |
-- | Fact types:
-- |   Facts are generic containers of data.
-- |   Each fact type has 0 or more String fields.
-- |   Fact types can be edited to add more fields, modify the names of existing fields, or remove existing fields.
-- |   For example: Fact type Basic (front, back), meaning two text fields, "front" and "back"
-- |                Fact type Car (Make, Model, Year)
-- |
-- | Card types:
-- |   Card types are per-fact type, and specify a card format using the fields of their fact type.
-- |   The data on the front and back of the card is specified as a template, using the data stored in
-- |   the fact type. This way, multiple sets of cards can be automatically generated for a given set of facts.
-- |   For example: Card type Basic (front: "${front}", back: "${back}") meaning the front of the card shoud
-- |                contain only the 'front' field, and the back the 'back' field
-- |                Card type Year (front: "${make} - ${model}", back: "{year}") for learning the year
-- |
-- | Fact:
-- |   A fact is a set of textual strings linked to a field of a user-defined fact type.
-- |   Belongs to a deck.
-- |   When a fact is created, a card for each of the card types linked to its specified fact type is created.
-- |   This can not be handled at the database level so should be handled manually at the application level.
-- |
-- | Card:
-- |   A card is a combination of a fact type and a card type, and reprisents a front:back pair to be learned.
-- |   It should contain nothing except linking a card type and a fact type to some scheduling information.
-- |   A card is automatically created at the following times (this should be handled by application logic):
-- |   When a fact is created
-- |   When a new card type is created for a fact type
-- |   Should be destroyed when a card type is destroyed (warn the user)
-- |
-- | Deck:
-- |   A collection of cards. Has a parent, which is null if the deck is one of the root nodes of the tree.

share [mkPersist sqlSettings, mkMigrate "migrateCards"] [persistLowerCase|
UserSrsFactType
  user_id SnapAuthUserId
UserSrsFactField
  user_id SnapAuthUserId
  fact_type_id UserSrsFactTypeId
  name T.Text
UserSrsCardType
  user_id SnapAuthUserId
  fact_type_id UserSrsFactTypeId
  front_template T.Text
  back_template T.Text
UserSrsFactData
  user_id SnapAuthUserId
  fact_field_id UserSrsFactFieldId
  data T.Text
-- | Todo: scheduling info (maybe opaque serialized safecopy data to allow application defined data to be stored)
UserSrsCard
  user_id SnapAuthUserId
  fact_type_id UserSrsFactTypeId
  card_type_id UserSrsCardTypeId
UserSrsDeck
  user_id SnapAuthUserId
  name T.Text
|]


-- Handlers --

renderTemporary :: MonadSnap m => [Char] -> m ()
renderTemporary string = do params <- getParams
                            writeBS . BS.pack $ "handler: " ++ string ++ " params: " ++ show params

-- Decks

currentUserId :: Handler App (AuthManager App) (Maybe SnapAuthUserId)
currentUserId = do user <- currentUser
                   return $ user >>= userDBKey

handleReviewDeck :: Handler App (AuthManager App) ()
handleReviewDeck = method GET (renderTemporary "review_deck")

handleNewDeck :: Handler App (AuthManager App) ()
handleNewDeck = method GET (render "new_deck") <|> method POST addDeck
  where
    -- Adds the specified decks to this user's deck collection, based on the 'name' parameter
    addDeck = do
      result <- runEitherT $ do
        user <- currentUserIdE
        deckName <- getParamE "name" "No deck name specified."
        validateDeckName deckName
        let newDeck = UserSrsDeck user (cs deckName)
        lift . runPersist . insert $ newDeck
      case result of
           Left err -> renderWithErrors "new_deck" [T.pack err]
           Right _  -> handleListDecks (Just . Right $ "Successfully created deck.")

handleListDecks :: Maybe (Either String String) -> Handler App (AuthManager App) ()
handleListDecks message = requireLogin $ listDecks
  where
    -- Lists the current user's decks
    listDecks = do
      d <- getUsersDecks
      case d of
           Left err -> renderWithErrors "list_decks" [T.pack err]
           Right decks -> heistLocal (splices decks) (render "list_decks")
    -- Gets the current user's decks
    getUsersDecks = runEitherT $ do
      user <- currentUserIdE
      lift . runPersist $ selectList [UserSrsDeckUser_id ==. user] []
    -- Turns 'message' into a splice based on whether it's empty (no message),
    -- or whether it contans an error (left) or a success message (right)
    messageSplices = case message of
                            Nothing -> mempty
                            Just (Left err) -> messageSplice "errors" err
                            Just (Right msg) -> messageSplice "success" msg
    -- Builds the splices for listing decks
    splices decks = I.bindSplices . mconcat $ [spliceDeck decks, messageSplices]
    spliceDeck d = "decks" ## I.mapSplices (I.runChildrenWith . spliceFromDeck) d
    spliceFromDeck (Entity deckId deck) = do
      "id" ## I.textSplice (cs . show . fromSqlKey $ deckId)
      "name" ## I.textSplice (cs . userSrsDeckName $ deck)

handleEditDeck :: Handler App (AuthManager App) ()
handleEditDeck = method GET (showForm Nothing) <|> method POST updateDeck
  where
    -- Shows the edit form
    showForm formError = do
      deck <- runEitherT $ do
        user <- currentUserIdE
        deckId <- getParamE "id" "No deck id specified"
        getUserDeck user (toSqlKey . read . cs $ deckId)
      case deck of
           Left err -> handleListDecks (Just . Left $ err)
           Right ent -> heistLocal (formSplices ent formError) (render "edit_deck")
    -- Builds splices for the edit form using a deck record and an optional error or success message
    formSplices (Entity deckId deck) formError =
            I.bindSplices . mconcat $
              ["name" ## I.textSplice . userSrsDeckName $ deck
              , "id" ## I.textSplice . cs . show . fromSqlKey $ deckId
              , maybe mempty (messageSpliceE) (formError)
              ]
    -- Updates a deck's name based on the 'id' and 'name' parameters
    updateDeck = do
      result <- runEitherT $ do
        user <- currentUserIdE
        deckIdTxt <- getParamE "id" "No deck id specified."
        newDeckName <- getParamE "name" "No deck name specified."
        Entity deckId _ <- getUserDeck user (toSqlKey . read . cs $ deckIdTxt)
        validateDeckName . cs $ newDeckName
        lift . runPersist $ update deckId [UserSrsDeckName =. cs newDeckName]
      case result of
           Left err -> showForm (Just . Left $ err)
           Right () -> showForm (Just . Right $ "Deck updated successfully.")

handleRemoveDeck :: Handler App (AuthManager App) ()
handleRemoveDeck = method GET showDeleteForm
               <|> method POST deleteDeck
  where
    showDeleteForm = do
      result <- runEitherT $ do
        user <- currentUserIdE
        deckIdTxt <- getParamE "id" "No deck id specified."
        Entity deckKey deck <- getUserDeck user (toSqlKey . read . cs $ deckIdTxt)
        let deckName = userSrsDeckName deck
        let deckId = fromSqlKey deckKey
        return (deckId, deckName)
      case result of
           Left err             -> handleListDecks (Just . Left $ err)
           Right (deckId, name) -> heistLocal (I.bindSplices . mconcat $
                                      [ "name" ## I.textSplice name
                                      , "id" ## I.textSplice . cs . show $ deckId
                                      ])
                                    (render "delete_deck")
    deleteDeck = do
      result <- runEitherT $ do
        user <- currentUserIdE
        deckIdTxt <- getParamE "id" "No deck id specified."
        Entity deckId deck <- getUserDeck user (toSqlKey . read . cs $ deckIdTxt)
        let deckName = userSrsDeckName deck
        lift . runPersist $ delete deckId
        return . cs $ deckName
      case result of
           Left err   -> handleListDecks (Just . Left $ err)
           Right name -> handleListDecks (Just . Right $ "Deck " ++ name ++ " deleted.")

-- Facts

handleNewFact :: Handler App (AuthManager App) ()
handleNewFact = method GET (renderTemporary "new_fact")

handleListFacts :: Handler App (AuthManager App) ()
handleListFacts = method GET (renderTemporary "list_facts")

handleEditFact :: Handler App (AuthManager App) ()
handleEditFact = method GET (renderTemporary "edit_fact")

handleRemoveFact :: Handler App (AuthManager App) ()
handleRemoveFact = method GET (renderTemporary "remove_fact")

-- Fact types

handleNewFactType :: Handler App (AuthManager App) ()
handleNewFactType = method GET (renderTemporary "new_fact_type")

handleListFactTypes :: Handler App (AuthManager App) ()
handleListFactTypes = method GET (renderTemporary "list_fact_types")

handleEditFactType :: Handler App (AuthManager App) ()
handleEditFactType = method GET (renderTemporary "edit_fact_type")

handleRemoveFactType :: Handler App (AuthManager App) ()
handleRemoveFactType = method GET (renderTemporary "remove_fact_type")

-- Card types

handleNewCardType :: Handler App (AuthManager App) ()
handleNewCardType = method GET (renderTemporary "new_card_type")

handleListCardTypes :: Handler App (AuthManager App) ()
handleListCardTypes = method GET (renderTemporary "list_card_types")

handleEditCardType :: Handler App (AuthManager App) ()
handleEditCardType = method GET (renderTemporary "edit_card_type")

handleRemoveCardType :: Handler App (AuthManager App) ()
handleRemoveCardType = method GET (renderTemporary "remove_card_type")

-- Other

validateDeckName :: Monad m => String -> EitherT String m ()
validateDeckName str = do require (length str < 16) "Deck name must be less than 16 characters."
                          require (length str > 0) "Deck name must not be empty."
                          return ()

getUserDeck :: SnapAuthUserId
            -> UserSrsDeckId
            -> EitherT String (Handler App (AuthManager App)) (Entity UserSrsDeck)
getUserDeck user deckId = (lift . runPersist $ selectList [UserSrsDeckUser_id ==. user,
                                                           UserSrsDeckId ==. deckId]
                                                          [LimitTo 1])
                              >>= (headMay >>> maybeToEitherT "No such deck found.")

getParamE :: String -> String -> EitherT String (Handler App (AuthManager App)) String
getParamE n s = (lift . getParam $ cs n) >>= maybeToEitherT s >>= return . cs

currentUserIdE :: EitherT String (Handler App (AuthManager App)) SnapAuthUserId
currentUserIdE = (lift currentUserId) >>= maybeToEitherT "Not logged in."
