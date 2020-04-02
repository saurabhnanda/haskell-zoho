{-# LANGUAGE DeriveAnyClass #-}
module Zoho.CRM.Notes where

import Zoho.OAuth as ZO
import Zoho.CRM.Common
import Zoho.Types
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.Aeson as Aeson
import Data.Aeson.TH as Aeson
import Data.String.Conv
import URI.ByteString as U
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Types
import Zoho.ZohoM as ZM
import Zoho.CRM.Records (RecordId)
import Data.Text (Text)
import Data.Aeson.Casing as Casing
import Control.Lens

data NoteSpecialFields = NoteSpecialFields
  { noteSeModule :: !(Maybe ApiName)
  , noteEditable :: !(Maybe Bool)
  , noteIsSharedToClient :: !(Maybe Bool)
  , noteVoiceNote :: !(Maybe Bool)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

emptyNoteSpecialFields :: NoteSpecialFields
emptyNoteSpecialFields = emptyZohoStructure

data Note = Note
  { noteMetaData :: !(Maybe MetaData)
  , noteNoteTile :: !(Maybe Text)
  , noteNoteContent :: !(Maybe Text)
  , noteId :: !(Maybe Text)
  , noteParentId :: !(Maybe (Reference "name"))
  , noteSpecialFields :: !(Maybe NoteSpecialFields)
  } deriving (Eq, Show, Generic, EmptyZohoStructure)

instance FromJSON Note where
  parseJSON = Aeson.withObject "Expecting object to parse into Note" $ \o -> do
    let v = Aeson.Object o
    noteMetaData <- parseJSON v
    noteSpecialFields <- parseJSON v
    noteNoteTile <- o .:? "Note_Title"
    noteNoteContent <- o .:? "Note_Content"
    noteParentId <- (o .:? "Parent_Id") >>= \case
      Nothing -> pure Nothing
      Just v2 -> parseJSON v2
    noteId <- o .:? "id"
    pure Note{..}


emptyNote :: Note
emptyNote  = emptyZohoStructure

data ListOptions = ListOptions
  { optPage :: Maybe Int
  , optPerPage :: Maybe Int
  } deriving (Eq, Show, Generic, EmptyZohoStructure)


emptyListOptions :: ListOptions
emptyListOptions = emptyZohoStructure

listRequest :: ListOptions
            -> Request
listRequest ListOptions{..} =
  ZO.prepareGet (ZO.mkApiEndpoint "/crm/v2/Notes") qparams []
  where
    qparams =
      applyOptionalQueryParam "per_page" (show <$> optPerPage) $
      applyOptionalQueryParam "page" (show <$> optPage) $
      []


list :: forall m . (HasZoho m)
     => ListOptions
     -> m (Either Error (PaginatedResponse "data" [Note]))
list listopts =
  ZM.runRequestAndParseOptionalResponse emptyPaginatedResponse Prelude.id $
  listRequest listopts

getSpecificRequest :: BS.ByteString
                   -> RecordId
                   -> Request
getSpecificRequest modApiName rid =
  let u = ZO.mkApiEndpoint ("/crm/v2/" <> modApiName <> "/" <> toS rid <> "/Notes")
  in ZO.prepareGet u [] []

getSpecific :: forall m . (HasZoho m)
            => BS.ByteString
            -> RecordId
            -> m (Either Error (PaginatedResponse "data" [Note]))
getSpecific modApiName rid =
  ZM.runRequestAndParseOptionalResponse emptyPaginatedResponse Prelude.id $
  getSpecificRequest modApiName rid


data NewNote = NewNote
  { newNoteTitle :: !(Maybe Text)
  , newNoteContent :: !Text
  , newParentId :: !RecordId
  , newSeModule :: !ApiName
  } deriving (Eq, Show)

instance ToJSON NewNote where
  toJSON NewNote{..} = Aeson.object
    [ "Note_Title" Aeson..= newNoteTitle
    , "Note_Content" Aeson..= newNoteContent
    , "Parent_Id" Aeson..= newParentId
    , "se_module" Aeson..= newSeModule
    ]

createNotesRequest :: [NewNote]
                   -> Request
createNotesRequest ns =
  ZO.prepareJSONPost (ZO.mkApiEndpoint "/crm/v2/Notes") [] [] $
  (ResponseWrapper ns  :: ResponseWrapper "data" [NewNote])


createNotes :: forall m . (HasZoho m)
            => [NewNote]
            -> m (Either Error [InsertResult])
createNotes ns = do
  (ZM.runRequestAndParseResponse $ createNotesRequest ns) >>= \case
    Left e ->
      pure $ Left e
    Right (r :: ResponseWrapper "data" [InsertResult]) ->
      pure $ Right $ unwrapResponse r


createNotesSpecificRequest :: BS.ByteString
                           -> RecordId
                           -> [NewNote]
                           -> Request
createNotesSpecificRequest modApiName rid ns =
  let u = ZO.mkApiEndpoint ("/crm/v2/" <> modApiName <> "/" <> toS rid <> "/Notes")
  in ZO.prepareJSONPost u [] [] $
     (ResponseWrapper ns  :: ResponseWrapper "data" [NewNote])


createNotesSpecific :: forall m . (HasZoho m)
                    => BS.ByteString
                    -> RecordId
                    -> [NewNote]
                    -> m (Either Error [InsertResult])
createNotesSpecific modApiName rid ns = do
  (ZM.runRequestAndParseResponse $ createNotesSpecificRequest modApiName rid ns) >>= \case
    Left e ->
      pure $ Left e
    Right (r :: ResponseWrapper "data" [InsertResult]) ->
      pure $ Right $ unwrapResponse r




$(deriveJSON (zohoPrefix (('$':) . Casing.snakeCase)) ''NoteSpecialFields)
$(makeLensesWith abbreviatedFields ''Note)
$(makeLensesWith abbreviatedFields ''NoteSpecialFields)
