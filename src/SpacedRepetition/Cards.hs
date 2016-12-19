{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}

module SpacedRepetition.Cards
( registerUserCollection
, migrateCards
-- Decks
, createDeck
, updateDeckName
, deleteDeck
, getUserDeck
, getUserDecks
, userSrsDeckName
-- Fact Types
, createFactType
, updateFactTypeName
, deleteFactType
, getUserFactType
, getUserFactTypes
, userSrsFactTypeName
-- Fact Fields
, createFactField
, updateFactFieldName
, deleteFactField
, getUserFactField
, getUserFactFields
, getFactFieldCount
, userSrsFactFieldName
-- Card Types
, createCardType
, updateCardTypeName
, deleteCardType
, getUserCardType
, getUserCardTypes
, getCardTypeCount
, userSrsCardTypeName
)
where

import Safe                                  ( headMay )
import Application                           ( App )
import Util                                  ( require, maybeToEitherT )
import Database.Persist.Sql                  ( selectList, count, get
                                             , insert, delete, update
                                             , Entity(..), SelectOpt(LimitTo)
                                             , (==.), (=.) )
import Database.Persist.TH                   ( share, mkPersist, sqlSettings
                                             , mkMigrate, persistLowerCase )
import Snap.Snaplet.Auth                     ( AuthManager )
import Snap                                  ( Handler )
import Control.Category                      ( (>>>) )
import Control.Monad.Trans                   ( lift )
import Control.Monad.Trans.Either            ( EitherT )
import Snap.Snaplet.Persistent               ( runPersist )
import Snap.Snaplet.Auth.Backends.Persistent ( SnapAuthUserId )
import qualified Data.Text as T              ( Text, length )


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

type CollectionAction a = EitherT String (Handler App (AuthManager App)) a

-- Collection creation/manipulation function
-- These take in a user id, and should verify the given resource belongs to the user
-- returning a 'not found' error otherwise. This prevents any handlers from giving
-- a user access to another user's collection. This may later be extended to allow decks
-- to be made public, but that might have to be done though another mechanism to avoid
-- exposing scheduling information etc.


-- | Initialises a user's collection. Executed on first login, and should act on the current user

registerUserCollection :: Handler App (AuthManager App) ()
registerUserCollection = return ()


-- Decks --


-- | Validates the name of a deck, used on creation or modification

validateDeckName :: Monad m => T.Text -> EitherT String m ()
validateDeckName str = do require (T.length str < 16) "Deck name must be less than 16 characters."
                          require (T.length str > 0) "Deck name must not be empty."


-- | Verify whether a user has access to the given deck

verifyUserDeckAccess :: SnapAuthUserId -> UserSrsDeckId -> CollectionAction ()
verifyUserDeckAccess user deckId = do
  let accessError = "The specified deck was not found."
  deck <- (lift . runPersist $ get deckId) >>= maybeToEitherT accessError
  require (userSrsDeckUser_id deck == user) accessError


-- | Creates a new deck

createDeck :: SnapAuthUserId -> T.Text -> CollectionAction UserSrsDeckId
createDeck user deckName = validateDeckName deckName
                           >> (lift . runPersist $ insert (UserSrsDeck user deckName))


-- | Updates a deck's name

updateDeckName :: SnapAuthUserId -> UserSrsDeckId -> T.Text -> CollectionAction ()
updateDeckName user deckId newName = validateDeckName newName
                                  >> verifyUserDeckAccess user deckId
                                  >> (lift . runPersist $ update deckId [UserSrsDeckName =. newName])


-- | Deletes a deck

deleteDeck :: SnapAuthUserId -> UserSrsDeckId -> CollectionAction ()
deleteDeck user deckId = verifyUserDeckAccess user deckId
                      >> (lift . runPersist $ delete deckId)


-- | Gets a user's decks

getUserDecks :: SnapAuthUserId -> CollectionAction [Entity UserSrsDeck]
getUserDecks user = lift . runPersist $ selectList [UserSrsDeckUser_id ==. user] []


-- Fact type --

-- | Validates the name of a fact type

validateFactTypeName :: Monad m => T.Text -> EitherT String m ()
validateFactTypeName str = do require (T.length str < 16) "Fact type name must be less than 16 characters."
                              require (T.length str > 0) "Fact type name must not be empty."


-- | Verify whether a user has access to the given fact type

verifyUserFactTypeAccess :: SnapAuthUserId -> UserSrsFactTypeId -> CollectionAction ()
verifyUserFactTypeAccess user factTypeId = do
  let accessError = "The specified fact type was not found."
  factType <- (lift . runPersist $ get factTypeId) >>= maybeToEitherT accessError
  require (userSrsFactTypeUser_id factType == user) accessError


-- | Creates a new fact type

createFactType :: SnapAuthUserId -> T.Text -> CollectionAction UserSrsFactTypeId
createFactType user name = validateFactTypeName name
                           >> (lift . runPersist $ insert (UserSrsFactType user name))


-- | Updates a fact type's name

updateFactTypeName :: SnapAuthUserId -> UserSrsFactTypeId -> T.Text -> CollectionAction ()
updateFactTypeName user factTypeId newName = validateFactTypeName newName
                                          >> verifyUserFactTypeAccess user factTypeId
                                          >> (lift . runPersist $ update factTypeId [UserSrsFactTypeName =. newName])


-- | Deletes a fact type

deleteFactType :: SnapAuthUserId -> UserSrsFactTypeId -> CollectionAction ()
deleteFactType user factTypeId = verifyUserFactTypeAccess user factTypeId
                              >> (lift . runPersist $ delete factTypeId)


-- | Gets a user's fact types

getUserFactTypes :: SnapAuthUserId -> CollectionAction [Entity UserSrsFactType]
getUserFactTypes user = lift . runPersist $ selectList [UserSrsFactTypeUser_id ==. user] []


-- Fact fields --

-- | Validates the name of a fact field

validateFactFieldName :: Monad m => T.Text -> EitherT String m ()
validateFactFieldName str = do require (T.length str < 16) "Fact field name must be less than 16 characters."
                               require (T.length str > 0) "Fact field name must not be empty."


-- | Verify whether a user has access to the given fact field

verifyUserFactFieldAccess :: SnapAuthUserId -> UserSrsFactFieldId -> CollectionAction ()
verifyUserFactFieldAccess user factFieldId = do
  let accessError = "The specified fact field was not found."
  factField <- (lift . runPersist $ get factFieldId) >>= maybeToEitherT accessError
  require (userSrsFactFieldUser_id factField == user) accessError


-- | Creates a new fact field

createFactField :: SnapAuthUserId -> UserSrsFactTypeId -> T.Text -> CollectionAction UserSrsFactFieldId
createFactField user factType name = validateFactFieldName name
                                     >> (lift . runPersist $ insert (UserSrsFactField user factType name))


-- | Updates a fact field's name

updateFactFieldName :: SnapAuthUserId -> UserSrsFactFieldId -> T.Text -> CollectionAction ()
updateFactFieldName user factFieldId newName = validateFactFieldName newName
                                            >> verifyUserFactFieldAccess user factFieldId
                                            >> (lift . runPersist $ update factFieldId [UserSrsFactFieldName =. newName])


-- | Deletes a fact field

deleteFactField :: SnapAuthUserId -> UserSrsFactFieldId -> CollectionAction ()
deleteFactField user factFieldId = verifyUserFactFieldAccess user factFieldId
                               >> (lift . runPersist $ delete factFieldId)


-- | Gets a user's fact fields

getUserFactFields :: SnapAuthUserId -> UserSrsFactTypeId -> CollectionAction [Entity UserSrsFactField]
getUserFactFields user factType = lift . runPersist $ selectList [ UserSrsFactFieldUser_id ==. user
                                                                 , UserSrsFactFieldFact_type_id ==. factType
                                                                 ] []


-- | Get a fact type's fact field count

getFactFieldCount :: SnapAuthUserId -> UserSrsFactTypeId -> CollectionAction Int
getFactFieldCount user factType = verifyUserFactTypeAccess user factType
                               >> (lift . runPersist $ count [UserSrsFactFieldFact_type_id ==. factType])


-- Card types --

-- | Validates the name of a card type

validateCardTypeName :: Monad m => T.Text -> EitherT String m ()
validateCardTypeName str = do require (T.length str < 16) "Fact type name must be less than 16 characters."
                              require (T.length str > 0) "Fact type name must not be empty."


-- | Verify whether a user has access to the given card type

verifyUserCardTypeAccess :: SnapAuthUserId -> UserSrsCardTypeId -> CollectionAction ()
verifyUserCardTypeAccess user cardTypeId = do
  let accessError = "The specified card type was not found."
  cardType <- (lift . runPersist $ get cardTypeId) >>= maybeToEitherT accessError
  require (userSrsCardTypeUser_id cardType == user) accessError


-- | The default "front" template for cards

defaultCardTypeFront :: T.Text
defaultCardTypeFront = "${front}"


-- | The default "back" template for cards

defaultCardTypeBack :: T.Text
defaultCardTypeBack = "${front}"


-- | Creates a new card type

createCardType :: SnapAuthUserId -> UserSrsFactTypeId -> T.Text -> CollectionAction UserSrsCardTypeId
createCardType user factType name = validateCardTypeName name
                                     >> let newCardType = UserSrsCardType user factType name
                                                            defaultCardTypeFront defaultCardTypeBack
                                        in (lift . runPersist $ insert newCardType)


-- | Updates a card type's name

updateCardTypeName :: SnapAuthUserId -> UserSrsCardTypeId -> T.Text -> CollectionAction ()
updateCardTypeName user cardTypeId newName = validateCardTypeName newName
                                            >> verifyUserCardTypeAccess user cardTypeId
                                            >> (lift . runPersist $ update cardTypeId [UserSrsCardTypeName =. newName])


-- | Deletes a card type

deleteCardType :: SnapAuthUserId -> UserSrsCardTypeId -> CollectionAction ()
deleteCardType user cardTypeId = verifyUserCardTypeAccess user cardTypeId
                               >> (lift . runPersist $ delete cardTypeId)


-- | Gets a user's card types

getUserCardTypes :: SnapAuthUserId -> UserSrsFactTypeId -> CollectionAction [Entity UserSrsCardType]
getUserCardTypes user factType = lift . runPersist $ selectList [ UserSrsCardTypeUser_id ==. user
                                                                , UserSrsCardTypeFact_type_id ==. factType
                                                                ] []


-- | Get a card type's fact field count

getCardTypeCount :: SnapAuthUserId -> UserSrsFactTypeId -> CollectionAction Int
getCardTypeCount user factType = verifyUserFactTypeAccess user factType
                              >> (lift . runPersist $ count [UserSrsCardTypeFact_type_id ==. factType])


-- Old stuff to get a specific user's collection objects --

-- | Gets a user's deck from the given id

getUserDeck :: SnapAuthUserId
            -> UserSrsDeckId
            -> CollectionAction (Entity UserSrsDeck)
getUserDeck user deckId = (lift . runPersist $ selectList [UserSrsDeckUser_id ==. user,
                                                           UserSrsDeckId ==. deckId]
                                                          [LimitTo 1])
                              >>= (headMay >>> maybeToEitherT "No such deck found.")


-- | Gets a user's fact type from the given id

getUserFactType :: SnapAuthUserId
                -> UserSrsFactTypeId
                -> CollectionAction (Entity UserSrsFactType)
getUserFactType user deckId = (lift . runPersist $ selectList [UserSrsFactTypeUser_id ==. user,
                                                               UserSrsFactTypeId ==. deckId]
                                                              [LimitTo 1])
                              >>= (headMay >>> maybeToEitherT "No such fact type found.")


-- | Gets a user's fact field from the given id

getUserFactField :: SnapAuthUserId
                -> UserSrsFactFieldId
                -> CollectionAction (Entity UserSrsFactField)
getUserFactField user deckId = (lift . runPersist $ selectList [UserSrsFactFieldUser_id ==. user,
                                                                UserSrsFactFieldId ==. deckId]
                                                               [LimitTo 1])
                              >>= (headMay >>> maybeToEitherT "No such fact field found.")


-- | Gets a user's card type from a given id

getUserCardType :: SnapAuthUserId
                -> UserSrsCardTypeId
                -> CollectionAction (Entity UserSrsCardType)
getUserCardType user deckId = (lift . runPersist $ selectList [UserSrsCardTypeUser_id ==. user,
                                                                UserSrsCardTypeId ==. deckId]
                                                               [LimitTo 1])
                              >>= (headMay >>> maybeToEitherT "No such card type found.")
