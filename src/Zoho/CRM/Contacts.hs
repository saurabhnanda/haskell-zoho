{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module Zoho.CRM.Contacts
  ( module Zoho.Types
  , module Zoho.CRM.Contacts
  , module Common
  ) where

import Zoho.OAuth as ZO
import Network.OAuth.OAuth2 as O
import Network.Wreq as W hiding (Proxy(..))
import Zoho.CRM.Records as R
import Zoho.CRM.Common as Common
import Zoho.Types
import Data.Aeson as Aeson
import Data.Aeson.Casing as Casing
import Data.Aeson.TH
import Data.ByteString as BS
import Network.HTTP.Client (Manager, ManagerSettings(..), newManager)
import Data.Text (Text)
import Data.Time
import qualified Data.Aeson.Types as Aeson
import Control.Lens
import GHC.Generics
import Control.Lens
import Data.Aeson.Types as Aeson (Parser)
import Zoho.ZohoM as ZM
import Data.List.NonEmpty as NE
import Data.Proxy
import Zoho.CRM.Common.Utils (googleAdsJsonOptions)
import Zoho.Types (zohoPrefix, pascalSnakeCase)
import Prelude hiding (id)
-- import Zoho.CRM.Common as L (record)

type ContactId = Text

data Contact cf = Contact
  { contactVisitSummary :: Maybe VisitSummary
  , contactScoreSummary :: Maybe ScoreSummary
  , contactGoogleAdsInfo :: Maybe GoogleAdsInfo
  , contactMetaData :: Maybe MetaData
  , contactRecordMetaData :: Maybe RecordMetaData
  , contactOtherFields :: Maybe cf
  , contactId :: Maybe ContactId
  , contactLastName :: Maybe Text
  , contactLeadSource :: Maybe Text
  } deriving (Show, Generic, EmptyZohoStructure)

$(makeLensesWith abbreviatedFields ''Contact)

emptyContact :: Contact cf
emptyContact = emptyZohoStructure

contactParser :: (Aeson.Value -> Parser (Maybe cf)) -> Aeson.Value -> Parser (Contact cf)
contactParser otherParser v = do
  contactVisitSummary <- parseJSON v
  contactScoreSummary <- parseJSON v
  contactGoogleAdsInfo <- parseJSON v
  contactMetaData <- parseJSON v
  contactRecordMetaData <- parseJSON v
  contactOtherFields <- otherParser v
  case v of
    Aeson.Object o -> do
      contactId <- o .:? "id"
      contactLastName <- o .:? "Last_Name"
      contactLeadSource <- o .:? "Lead_Source"
      pure Contact{..}
    x -> fail "Expecting an Object to parse into a Contact"

instance (FromJSON cf) => FromJSON (Contact cf) where
  parseJSON = withObject "Exepcting a JSON object to parse into a Contact" $ \o -> do
    let x = Object o
    contactParser parseJSON x

instance (ToJSON cf) => ToJSON (Contact cf) where
  toJSON c@Contact{..} =
    unsafeMergeObjects (toJSON contactVisitSummary) $
    unsafeMergeObjects (toJSON contactScoreSummary) $
    unsafeMergeObjects (toJSON contactGoogleAdsInfo) $
    unsafeMergeObjects (toJSON contactMetaData) $
    unsafeMergeObjects (toJSON contactOtherFields) $
    unsafeMergeObjects (toJSON contactRecordMetaData) $
    unsafeMergeObjects gclid_ $
    object $
      omitNothing "id" contactId <>
      omitNothing "Last_Name" contactLastName <>
      omitNothing "Lead_Source" contactLeadSource
    where
      gclid_ =
        case c ^? googleAdsInfo . _Just . gclid . _Just of
          Nothing -> Aeson.object []
          Just g -> Aeson.object [ "$gclid" Aeson..= g ]
      omitNothing k v = case v of
        Nothing -> []
        Just x -> [k Aeson..= x]


list :: (FromJSON (Contact cf), HasZoho m)
     => ListOptions
     -> m (Either Error (PaginatedResponse "data" [Contact cf]))
list listopts = R.list "Contacts" listopts

getSpecific :: (FromJSON (Contact cf), HasZoho m)
            => ContactId
            -> m (Either Error (Maybe (Contact cf)))
getSpecific = R.getSpecific "Contacts"

insert :: (ToJSON cf, HasZoho m)
       => [Contact cf]
       -> TriggerSetting
       -> m (Either Error [InsertResult])
insert contacts tsetting =
  R.insert "Contacts" contacts tsetting

update :: (ToJSON cf, HasZoho m)
       => [Contact cf]
       -> TriggerSetting
       -> m (Either Error [UpdateResult])
update contacts tsetting =
  R.update "Contacts" contacts tsetting

upsert :: (ToJSON cf, HasZoho m)
       => [Contact cf]
       -> TriggerSetting
       -> DuplicateCheckFields
       -> m (Either Error [UpsertResult])
upsert contacts tsetting dupCheckFields =
  R.upsert "Contacts" contacts tsetting dupCheckFields

delete :: (HasZoho m)
       => NE.NonEmpty ContactId
       -> TriggerWorkflow
       -> m (Either Error [DeleteResult])
delete contacts wfTrigger =
  R.delete "Contacts" contacts wfTrigger

search :: (HasZoho m, FromJSON (Contact cf))
       => SearchQuery
       -> SearchOpts
       -> m (Either Error (PaginatedResponse "data" [Contact cf]))
search q opts = R.search "Contacts" q opts
