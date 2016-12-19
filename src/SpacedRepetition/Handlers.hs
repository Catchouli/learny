{-# LANGUAGE OverloadedStrings          #-}

module SpacedRepetition.Handlers
-- Decks
( handleReviewDeck
, handleNewDeck
, handleListDecks
, handleEditDeck
, handleRemoveDeck
-- Facts
, handleNewFact
, handleListFacts
, handleEditFact
, handleRemoveFact
-- Fact types
, handleNewFactType
, handleListFactTypes
, handleEditFactType
, handleRemoveFactType
-- Fields
, handleNewFactTypeField
, handleRemoveFactTypeField
-- Card types
, handleNewCardType
, handleEditCardType
, handleRemoveCardType
)
where

import SpacedRepetition.Cards
import Application                           ( App )
import Util                                  ( requireLogin, renderWithErrors
                                             , messageSpliceE, currentUserIdE
                                             , getParamE, messageSplices )
import Database.Persist.Sql                  ( Entity(..), fromSqlKey, toSqlKey )
import Snap.Snaplet.Heist                    ( render, heistLocal, HasHeist )
import Snap.Snaplet.Auth                     ( AuthManager )
import Heist                                 ( (##) )
import Snap                                  ( getParam, getParams, writeBS
                                             , redirect, method, Handler
                                             , Method(..) )
import Control.Applicative                   ( (<|>) )
import Control.Monad.Trans                   ( lift )
import Control.Monad.Trans.Either            ( runEitherT )
import Data.String.Conversions               ( cs )
import qualified Data.Map as M               ( toList, filterWithKey )
import qualified Heist.Interpreted as I      ( bindSplices, textSplice
                                             , mapSplices, runChildrenWith )
import qualified Data.Text as T              ( pack )
import qualified Data.ByteString.Char8 as BS ( pack, unpack, take )


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
        createDeck user (T.pack deckName)
      case result of
           Left err -> renderWithErrors "new_deck" [T.pack err]
           Right _  -> handleListDecks (Just . Right $ "Successfully created deck.")

handleListDecks :: Maybe (Either String String) -> Handler App (AuthManager App) ()
handleListDecks message = requireLogin $ listDecks
  where
    -- Lists the current user's decks
    listDecks = do
      decksE <- runEitherT $ currentUserIdE >>= getUserDecks
      case decksE of
           Left err -> renderWithErrors "list_decks" [T.pack err]
           Right decks -> heistLocal (splices decks) (render "list_decks")
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
        -- Update the deck name
        updateDeckName user deckId (cs newDeckName)
      case result of
           Left err -> showForm (Just . Left $ err)
           Right () -> showForm (Just . Right $ "Deck updated successfully.")

handleRemoveDeck :: Handler App (AuthManager App) ()
handleRemoveDeck = method GET showDeleteForm
               <|> method POST actuallyDeleteDeck
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
    actuallyDeleteDeck = do
      result <- runEitherT $ do
        user <- currentUserIdE
        deckIdTxt <- getParamE "id" "No deck id specified."
        Entity deckId deck <- getUserDeck user (toSqlKey . read . cs $ deckIdTxt)
        let deckName = userSrsDeckName deck
        -- Delete the deck
        deleteDeck user deckId
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
        name <- getParamE "name" "No name specified." >>= return . cs
        -- Create fact type, fields, and a default card type
        factTypeId <- createFactType user name
        createFactField user factTypeId "Front"
        createFactField user factTypeId "Back"
        createCardType user factTypeId "Default"
      case result of
           Left err -> renderWithErrors "new_fact_type" [T.pack err]
           Right _  -> handleListFactTypes (Just . Right $ "Successfully created fact type.")

handleListFactTypes :: Maybe (Either String String) -> Handler App (AuthManager App) ()
handleListFactTypes message = requireLogin $ listFactTypes
  where
    -- Lists the current user's fact types
    listFactTypes = do
      factTypesE <- runEitherT $ currentUserIdE >>= getUserFactTypes
      case factTypesE of
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
        fields <- getUserFactFields user ftId
        card_types <- getUserCardTypes user ftId
        return (factType, fields, card_types)
      case factType of
           Left err -> handleListFactTypes (Just . Left $ err)
           Right res -> heistLocal (formSplices res formError) (render "edit_fact_type")
    -- Builds splices for the edit form using a fact type record and an optional error or success message
    formSplices ((Entity factTypeId factType), fields, cardTypes) formError =
            I.bindSplices . mconcat $
              ["name" ## I.textSplice . userSrsFactTypeName $ factType
              , "id" ## I.textSplice . cs . show . fromSqlKey $ factTypeId
              , maybe mempty (messageSpliceE) (formError)
              , "fields" ## I.mapSplices (I.runChildrenWith . spliceFromField) fields
              , "card_types" ## I.mapSplices (I.runChildrenWith . spliceFromCardType) cardTypes
              ]
    spliceFromField (Entity fieldId field) =
            mconcat [ "field_id" ## I.textSplice (cs . show . fromSqlKey $ fieldId)
                    , "name" ## I.textSplice (cs . userSrsFactFieldName $ field)
                    ]
    spliceFromCardType (Entity cardTypeId cardType) =
            mconcat [ "card_type_id" ## I.textSplice (cs . show . fromSqlKey $ cardTypeId)
                    , "name" ## I.textSplice (cs . userSrsCardTypeName $ cardType)
                    ]

    -- Updates a fact type
    updateFactType = do
      result <- runEitherT $ do
        user <- currentUserIdE
        factTypeIdTxt <- getParamE "id" "No fact type id specified."

        -- Update name if specified
        newFactTypeName <- lift . getParam $ "name"
        case newFactTypeName of
          Just newName -> let factTypeId = toSqlKey . read . cs $ factTypeIdTxt
                          in updateFactTypeName user factTypeId (cs newName)
          Nothing      -> return ()

        -- Update field and card type names
        -- Start by getting all the parameters and check them for field and card type names
        allParams <- lift getParams 

        -- Update field names if specified
        -- Filter parameters to get field names (parameters beginning with "field-name-"
        let newFieldNames = map (\(a,b) -> (drop 11 $ BS.unpack a, BS.unpack . head $ b)) $
                M.toList . M.filterWithKey (\k _ -> BS.take 11 k == "field-name-") $ allParams
        -- For each field returned, update its name
        (flip mapM_) newFieldNames $ \(factFieldId, newName) ->
          do
            let fieldId = toSqlKey . read $ factFieldId
            _ <- getUserFactField user fieldId
            updateFactFieldName user fieldId (cs newName)

        -- Update card type names if specified
        -- Filter parameters to get field names (parameters beginning with "card-type-name-"
        let newCardTypeNames = map (\(a,b) -> (drop 15 $ BS.unpack a, BS.unpack . head $ b)) $
                M.toList . M.filterWithKey (\k _ -> BS.take 15 k == "card-type-name-") $ allParams
        -- For each card type returned, update its name
        (flip mapM_) newCardTypeNames $ \(cardTypeIdTxt, newName) ->
          do
            let cardTypeId = toSqlKey . read $ cardTypeIdTxt
            _ <- getUserCardType user cardTypeId
            updateCardTypeName user cardTypeId (cs newName)

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
        fieldCount <- getFactFieldCount user factTypeId
        createFactField user factTypeId (cs $ "Field " ++ show (fieldCount+1))
        return factTypeIdTxt
      case result of
        Left err -> handleListFactTypes (Just . Left $ err)
        Right factTypeId -> redirect . cs $ "/fact_types/edit/" ++ factTypeId

handleRemoveFactTypeField :: Handler App (AuthManager App) ()
handleRemoveFactTypeField = method GET showDeleteForm
                   <|> method POST actuallyDeleteFactTypeField
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
    actuallyDeleteFactTypeField = do
      result <- runEitherT $ do
        user <- currentUserIdE
        factTypeIdTxt <- getParamE "fact_type_id" "No fact type id specified."
        factFieldIdTxt <- getParamE "field_id" "No fact field id specified."
        Entity factFieldId factField <- getUserFactField user (toSqlKey . read . cs $ factFieldIdTxt)
        let factFieldName = userSrsFactFieldName factField
        deleteFactField user factFieldId
        return $ (factFieldName, factTypeIdTxt)
      case result of
           Left err   -> handleListFactTypes (Just . Left $ err)
           Right (_, typeId) -> redirect . cs $ "/fact_types/edit/" ++ typeId

handleRemoveFactType :: Handler App (AuthManager App) ()
handleRemoveFactType = method GET showDeleteForm
                   <|> method POST actuallyDeleteFactType
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
    actuallyDeleteFactType = do
      result <- runEitherT $ do
        user <- currentUserIdE
        factTypeIdTxt <- getParamE "id" "No fact type id specified."
        Entity factTypeId factType <- getUserFactType user (toSqlKey . read . cs $ factTypeIdTxt)
        let factTypeName = userSrsFactTypeName factType
        deleteFactType user factTypeId
        return . cs $ factTypeName
      case result of
           Left err   -> handleListFactTypes (Just . Left $ err)
           Right name -> handleListFactTypes (Just . Right $ "Fact type " ++ name ++ " deleted.")

-- Card types

handleNewCardType :: Handler App (AuthManager App) ()
handleNewCardType = method POST addCardType
  where
    addCardType = do
      result <- runEitherT $ do
        user <- currentUserIdE
        factTypeIdTxt <- getParamE "fact_type_id" "No fact type id specified."
        Entity factTypeId _ <- getUserFactType user (toSqlKey . read . cs $ factTypeIdTxt)
        cardTypeCount <- getCardTypeCount user factTypeId
        createCardType user factTypeId (cs $ "Card type " ++ show (cardTypeCount+1))
        return factTypeIdTxt
      case result of
        Left err -> handleListFactTypes (Just . Left $ err)
        Right factTypeId -> redirect . cs $ "/fact_types/edit/" ++ factTypeId

handleEditCardType :: Handler App (AuthManager App) ()
handleEditCardType = method GET (renderTemporary "edit_card_type")

handleRemoveCardType :: Handler App (AuthManager App) ()
handleRemoveCardType = method GET showDeleteForm
                   <|> method POST actuallyDeleteCardType
  where
    -- Shows the form for deletion, passing the name and id to the page to verify with the user
    showDeleteForm = do
      result <- runEitherT $ do
        user <- currentUserIdE
        cardTypeStr <- getParamE "id" "No card tdype id specified."
        Entity cardTypeKey cardType <- getUserCardType user (toSqlKey . read . cs $ cardTypeStr)
        let cardTypeName = userSrsCardTypeName cardType
        let cardTypeId = fromSqlKey cardTypeKey
        return (cardTypeId, cardTypeName)
      case result of
           Left err                  -> handleListFactTypes (Just . Left $ err)
           Right (cardTypeId, name)  -> heistLocal (I.bindSplices . mconcat $
                                          [ "name" ## I.textSplice name
                                          , "id" ## I.textSplice . cs . show $ cardTypeId
                                          ])
                                        (render "delete_card_type")
    -- Actually deletes the card type field
    actuallyDeleteCardType = do
      result <- runEitherT $ do
        user <- currentUserIdE
        factTypeIdTxt <- getParamE "fact_type_id" "No fact type id specified."
        cardTypeIdTxt <- getParamE "id" "No card type id specified."
        Entity cardTypeId cardType <- getUserCardType user (toSqlKey . read . cs $ cardTypeIdTxt)
        let cardTypeName = userSrsCardTypeName cardType
        deleteCardType user cardTypeId
        return $ (cardTypeName, factTypeIdTxt)
      case result of
           Left err          -> handleListFactTypes (Just . Left $ err)
           Right (_, typeId) -> redirect . cs $ "/fact_types/edit/" ++ typeId

