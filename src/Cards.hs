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
import Data.String
import Data.List (find)
import qualified Data.Map as M
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
  name T.Text
  UserFactTypeName user_id name
UserSrsFactField
  user_id SnapAuthUserId
  fact_type_id UserSrsFactTypeId
  name T.Text
  FactTypeField fact_type_id name
UserSrsCardType
  user_id SnapAuthUserId
  fact_type_id UserSrsFactTypeId
  name T.Text
  front_template T.Text
  back_template T.Text
  FactCardType fact_type_id name
UserSrsFact
  user_id SnapAuthUserId
UserSrsFactData
  user_id SnapAuthUserId
  fact_id UserSrsFactId
  fact_field_id UserSrsFactFieldId
  data T.Text
  FactDataField fact_field_id fact_id
-- | Todo: scheduling info (maybe opaque serialized safecopy data to allow application defined data to be stored)
UserSrsCard
  user_id SnapAuthUserId
  fact_type_id UserSrsFactTypeId
  card_type_id UserSrsCardTypeId
  FactCard fact_type_id card_type_id
UserSrsDeck
  user_id SnapAuthUserId
  name T.Text
  UserDeck user_id name
|]


-- Handlers --

renderTemporary :: HasHeist m => [Char] -> Handler m v ()
renderTemporary string = do params <- getParams
                            render "base"
                            writeBS . BS.pack $ "handler: " ++ string ++ " params: " ++ show params

-- Decks

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
    -- Builds the splices for listing decks
    splices decks = I.bindSplices . mconcat $ [spliceDeck decks, messageSplices message]
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
    -- Shows the form for deletion, passing the name and id to the page to verify with the user
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
    -- Actually deletes the deck
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
handleNewFactType = method GET (render "new_fact_type") <|> method POST addFactType
  where
    -- Adds a new fact type to this user's deck collection, based on the 'name' parameter
    addFactType = do
      result <- runEitherT $ do
        -- Get parameters
        user <- currentUserIdE
        name <- getParamE "name" "No name specified."
        validateFactTypeName name
        -- Create fact type
        let newFactType = UserSrsFactType user (cs name)
        newFactTypeId <- lift . runPersist $ insert newFactType
        -- Create default fields
        let field = UserSrsFactField user newFactTypeId
        let newFactTypeFields = [field "Front", field "Back"]
        lift . runPersist $ mapM_ insert newFactTypeFields
      case result of
           Left err -> renderWithErrors "new_fact_type" [T.pack err]
           Right _  -> handleListFactTypes (Just . Right $ "Successfully created fact type.")

handleListFactTypes :: Maybe (Either String String) -> Handler App (AuthManager App) ()
handleListFactTypes message = requireLogin $ listFactTypes
  where
    -- Lists the current user's fact types
    listFactTypes = do
      d <- runEitherT $ do
        user <- currentUserIdE
        lift . runPersist $ selectList [UserSrsFactTypeUser_id ==. user] []
      case d of
           Left err -> renderWithErrors "list_fact_types" [T.pack err]
           Right factTypes -> heistLocal (splices factTypes) (render "list_fact_types")
    splices factTypes = I.bindSplices . mconcat $ [spliceFactTypes factTypes, messageSplices message]
    spliceFactTypes d = "factTypes" ## I.mapSplices (I.runChildrenWith . spliceFromFactTypes) d
    spliceFromFactTypes (Entity deckId deck) = do
      "id" ## I.textSplice (cs . show . fromSqlKey $ deckId)
      "name" ## I.textSplice (cs . userSrsFactTypeName $ deck)

handleEditFactType :: Handler App (AuthManager App) ()
handleEditFactType = method GET (showForm Nothing) <|> method POST updateFactType
  where
    -- Shows the edit form
    showForm formError = do
      factType <- runEitherT $ do
        user <- currentUserIdE
        factId <- getParamE "id" "No fact id specified"
        factType@(Entity ftId _) <- getUserFactType user (toSqlKey . read . cs $ factId)
        fields <- lift . runPersist $ selectList [ UserSrsFactFieldUser_id ==. user
                                                 , UserSrsFactFieldFact_type_id ==. ftId
                                                 ] []
        return (factType, fields)
      case factType of
           Left err -> handleListFactTypes (Just . Left $ err)
           Right res -> heistLocal (formSplices res formError) (render "edit_fact_type")
    -- Builds splices for the edit form using a fact type record and an optional error or success message
    formSplices ((Entity factTypeId factType), fields) formError =
            I.bindSplices . mconcat $
              ["name" ## I.textSplice . userSrsFactTypeName $ factType
              , "id" ## I.textSplice . cs . show . fromSqlKey $ factTypeId
              , maybe mempty (messageSpliceE) (formError)
              , "fields" ## I.mapSplices (I.runChildrenWith . spliceFromField) fields
              ]
    spliceFromField (Entity fieldId field) =
            mconcat [ "field_id" ## I.textSplice (cs . show . fromSqlKey $ fieldId)
                    , "name" ## I.textSplice (cs . userSrsFactFieldName $ field)
                    ]

    -- Updates a fact type
    updateFactType = do
      result <- runEitherT $ do
        user <- currentUserIdE
        factTypeIdTxt <- getParamE "id" "No fact type id specified."

        -- Update name if specified
        newFactTypeName <- lift . getParam $ "name"
        case newFactTypeName of
          Just newName ->
            do
             Entity factTypeId _ <- getUserFactType user (toSqlKey . read . cs $ factTypeIdTxt)
             validateFactTypeName . cs $ newName
             lift . runPersist $ update factTypeId [UserSrsFactTypeName =. cs newName]
          Nothing -> return ()

        -- Update field names if specified
        allParams <- lift getParams 
        -- Filter parameters to get field names (parameters beginning with "field-name-"
        let newFieldNames = map (\(a,b) -> (drop 11 $ BS.unpack a, BS.unpack . head $ b)) $
                M.toList . M.filterWithKey (\k _ -> BS.take 11 k == "field-name-") $ allParams
        -- Find these fields in the database
        fields <- lift . runPersist $
          selectList [ UserSrsFactFieldUser_id ==. user
                     , UserSrsFactFieldId <-. (map (toSqlKey . read . fst) newFieldNames)] []
        -- For each field returned, update its name
        (flip mapM_) fields $ \(Entity fieldId _) ->
          do
            let maybeNewName = find (\(x,_) -> (toSqlKey . read) x == fieldId) newFieldNames
            case maybeNewName of
              Just (_, newName) -> do
                lift . runPersist $ update fieldId [UserSrsFactFieldName =. cs newName]
              Nothing -> return ()

      case result of
           Left err -> showForm (Just . Left $ err)
           Right () -> showForm (Just . Right $ "Fact type updated successfully.")


handleNewFactTypeField :: Handler App (AuthManager App) ()
handleNewFactTypeField = method POST addFactField
  where
    addFactField = do
      result <- runEitherT $ do
        user <- currentUserIdE
        factTypeIdTxt <- getParamE "fact_type_id" "No fact type id specified."
        Entity factTypeId _ <- getUserFactType user (toSqlKey . read . cs $ factTypeIdTxt)
        fieldCount <- lift . runPersist $ count [UserSrsFactFieldFact_type_id ==. factTypeId]
        lift . runPersist $
          insert $ UserSrsFactField user factTypeId (cs $ "Field " ++ show (fieldCount+1))
        return factTypeIdTxt
      case result of
        Left err -> handleListFactTypes (Just . Left $ err)
        Right factTypeId -> redirect . cs $ "/fact_types/edit/" ++ factTypeId

handleRemoveFactTypeField :: Handler App (AuthManager App) ()
handleRemoveFactTypeField = method GET showDeleteForm
                   <|> method POST deleteFactTypeField
  where
    -- Shows the form for deletion, passing the name and id to the page to verify with the user
    showDeleteForm = do
      result <- runEitherT $ do
        user <- currentUserIdE
        factFieldStr <- getParamE "field_id" "No fact field id specified."
        Entity factFieldKey factField <- getUserFactField user (toSqlKey . read . cs $ factFieldStr)
        let factFieldName = userSrsFactFieldName factField
        let factFieldId = fromSqlKey factFieldKey
        return (factFieldId, factFieldName)
      case result of
           Left err                 -> handleListFactTypes (Just . Left $ err)
           Right (factFieldId, name) -> heistLocal (I.bindSplices . mconcat $
                                          [ "name" ## I.textSplice name
                                          , "id" ## I.textSplice . cs . show $ factFieldId
                                          ])
                                        (render "delete_fact_field")
    -- Actually deletes the fact type field
    deleteFactTypeField = do
      result <- runEitherT $ do
        user <- currentUserIdE
        factTypeIdTxt <- getParamE "fact_type_id" "No fact type id specified."
        factFieldIdTxt <- getParamE "field_id" "No fact type id specified."
        Entity factFieldId factField <- getUserFactField user (toSqlKey . read . cs $ factFieldIdTxt)
        let factFieldName = userSrsFactFieldName factField
        lift . runPersist $ delete factFieldId
        return $ (factFieldName, factTypeIdTxt)
      case result of
           Left err   -> handleListFactTypes (Just . Left $ err)
           Right (_, typeId) -> redirect . cs $ "/fact_types/edit/" ++ typeId

handleRemoveFactType :: Handler App (AuthManager App) ()
handleRemoveFactType = method GET showDeleteForm
                   <|> method POST deleteFactType
  where
    -- Shows the form for deletion, passing the name and id to the page to verify with the user
    showDeleteForm = do
      result <- runEitherT $ do
        user <- currentUserIdE
        factTypeStr <- getParamE "id" "No fact type id specified."
        Entity factTypeKey factType <- getUserFactType user (toSqlKey . read . cs $ factTypeStr)
        let factTypeName = userSrsFactTypeName factType
        let factTypeId = fromSqlKey factTypeKey
        return (factTypeId, factTypeName)
      case result of
           Left err                 -> handleListFactTypes (Just . Left $ err)
           Right (factTypeId, name) -> heistLocal (I.bindSplices . mconcat $
                                          [ "name" ## I.textSplice name
                                          , "id" ## I.textSplice . cs . show $ factTypeId
                                          ])
                                        (render "delete_fact_type")
    -- Actually deletes the fact type
    deleteFactType = do
      result <- runEitherT $ do
        user <- currentUserIdE
        factTypeIdTxt <- getParamE "id" "No fact type id specified."
        Entity factTypeId factType <- getUserFactType user (toSqlKey . read . cs $ factTypeIdTxt)
        let factTypeName = userSrsFactTypeName factType
        lift . runPersist $ delete factTypeId
        return . cs $ factTypeName
      case result of
           Left err   -> handleListFactTypes (Just . Left $ err)
           Right name -> handleListFactTypes (Just . Right $ "Fact type " ++ name ++ " deleted.")

-- Card types

handleNewCardType :: Handler App (AuthManager App) ()
handleNewCardType = method GET (renderTemporary "new_card_type")

handleEditCardType :: Handler App (AuthManager App) ()
handleEditCardType = method GET (renderTemporary "edit_card_type")

handleRemoveCardType :: Handler App (AuthManager App) ()
handleRemoveCardType = method GET (renderTemporary "remove_card_type")

-- Other

currentUserId :: Handler App (AuthManager App) (Maybe SnapAuthUserId)
currentUserId = do user <- currentUser
                   return $ user >>= userDBKey

-- Turns 'message' into a splice based on whether it's empty (no message),
-- or whether it contans an error (left) or a success message (right)
-- Builds the splices for listing decks
messageSplices :: (Monad m, IsString k) => Maybe (Either String String) -> MapSyntax k (HeistT m m Template)
messageSplices message = case message of
                            Nothing -> mempty
                            Just (Left err) -> messageSplice "errors" err
                            Just (Right msg) -> messageSplice "success" msg

validateDeckName :: Monad m => String -> EitherT String m ()
validateDeckName str = do require (length str < 16) "Deck name must be less than 16 characters."
                          require (length str > 0) "Deck name must not be empty."

validateFactTypeName :: Monad m => String -> EitherT String m ()
validateFactTypeName str = do require (length str < 16) "Fact type name must be less than 16 characters."
                              require (length str > 0) "Fact type name must not be empty."

getUserDeck :: SnapAuthUserId
            -> UserSrsDeckId
            -> EitherT String (Handler App (AuthManager App)) (Entity UserSrsDeck)
getUserDeck user deckId = (lift . runPersist $ selectList [UserSrsDeckUser_id ==. user,
                                                           UserSrsDeckId ==. deckId]
                                                          [LimitTo 1])
                              >>= (headMay >>> maybeToEitherT "No such deck found.")

getUserFactType :: SnapAuthUserId
                -> UserSrsFactTypeId
                -> EitherT String (Handler App (AuthManager App)) (Entity UserSrsFactType)
getUserFactType user deckId = (lift . runPersist $ selectList [UserSrsFactTypeUser_id ==. user,
                                                               UserSrsFactTypeId ==. deckId]
                                                              [LimitTo 1])
                              >>= (headMay >>> maybeToEitherT "No such fact type found.")

getUserFactField :: SnapAuthUserId
                -> UserSrsFactFieldId
                -> EitherT String (Handler App (AuthManager App)) (Entity UserSrsFactField)
getUserFactField user deckId = (lift . runPersist $ selectList [UserSrsFactFieldUser_id ==. user,
                                                                UserSrsFactFieldId ==. deckId]
                                                               [LimitTo 1])
                              >>= (headMay >>> maybeToEitherT "No such fact field found.")

getParamE :: String -> String -> EitherT String (Handler App (AuthManager App)) String
getParamE n s = (lift . getParam $ cs n) >>= maybeToEitherT s >>= return . cs

currentUserIdE :: EitherT String (Handler App (AuthManager App)) SnapAuthUserId
currentUserIdE = (lift currentUserId) >>= maybeToEitherT "Not logged in."
