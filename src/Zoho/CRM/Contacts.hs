{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
module Zoho.CRM.Contacts where

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

data Approval = Approval
  { apDelegate :: Maybe Bool -- delegate
  , apApprove :: Maybe Bool
  , apReject :: Maybe Bool
  , apResubmit :: Maybe Bool
  } deriving (Eq, Show, Generic)

-- class GEmptyZohoStructure f where
--   gEmptyZohoStructure :: f p

-- instance (GEmptyZohoStructure f) => GEmptyZohoStructure (M1 i t f) where
--   gEmptyZohoStructure = (gEmptyZohoStructure :: f p)

-- instance (GEmptyZohoStructure f, GEmptyZohoStructure g) => GEmptyZohoStructure (f :*: g) where
--   gEmptyZohoStructure = (gEmptyZohoStructure :: f p) :*: (gEmptyZohoStructure :: g p)

-- instance (GEmptyZohoStructure f) => GEmptyZohoStructure (K1 i (f p)) where
--   gEmptyZohoStructure = gEmptyZohoStructure

-- instance GEmptyZohoStructure Maybe where
--   gEmptyZohoStructure = Nothing


instance EmptyZohoStructure Approval where
  emptyZohoStructure = Approval
    { apDelegate = Nothing
    , apApprove = Nothing
    , apReject = Nothing
    , apResubmit = Nothing
    }

emptyApproval :: Approval
emptyApproval = emptyZohoStructure


-- class GMonoid 
-- emptyApproval :: Approval
-- emptyApproval = Approval

data ContactSpecialFields = ContactSpecialFields
  { csfCurrencySymbol :: Maybe Text -- $currency_symbol
  , csfState :: Maybe Text -- $state
  , csfProcessFlow :: Maybe Bool -- $process_flow
  , csfApproved :: Maybe Bool -- $approved
  , csfApproval :: Approval  -- $approval
  , csfEditable :: Maybe Bool -- $editable

  -- TODO: Figure out what is "review" all about
  -- , csfReviewProcess :: Maybe _ -- $review_process
  -- , csvReview :: Maybe _ -- $review
  } deriving (Eq, Show)


instance EmptyZohoStructure ContactSpecialFields where
  emptyZohoStructure = ContactSpecialFields
    { csfCurrencySymbol = Nothing
    , csfState = Nothing
    , csfProcessFlow = Nothing
    , csfApproved = Nothing
    , csfApproval = emptyZohoStructure
    , csfEditable = Nothing
    }

emptyContactSpeicalFields :: ContactSpecialFields
emptyContactSpeicalFields = emptyZohoStructure

data ContactFixedFields = ContactFixedFields
  { cffLastName :: Maybe Text
  , cffOwner :: Maybe (Reference "name")
  , cffModifiedBy :: Maybe (Reference "name")
  , cffModifiedTime :: Maybe ZonedTime
  , cffCreatedTime :: Maybe ZonedTime
  , cffCreatedBy :: Maybe (Reference "name")
  , cffLeadSource :: Maybe Text
  , cffTag :: Maybe [Reference "name"]
  , cffLastActivityTime :: Maybe ZonedTime
  -- TODO: Figure out what is the structure of record image
  } deriving (Show)

instance EmptyZohoStructure ContactFixedFields where
  emptyZohoStructure = ContactFixedFields
    { cffLastName = Nothing
    , cffOwner = Nothing
    , cffModifiedBy = Nothing
    , cffModifiedTime = Nothing
    , cffCreatedTime = Nothing
    , cffCreatedBy = Nothing
    , cffLeadSource = Nothing
    , cffTag = Nothing
    , cffLastActivityTime = Nothing
    }

emptyContactFixedFields :: ContactFixedFields
emptyContactFixedFields = emptyZohoStructure

data Contact a = Contact
  { contactVisitSummary :: Maybe VisitSummary
  , contactScoreSummary :: Maybe ScoreSummary
  , contactGoogleAdsInfo :: Maybe GoogleAdsInfo
  , contactSpecialFields :: Maybe ContactSpecialFields
  , contactFixedFields :: Maybe ContactFixedFields
  , contactOtherFields :: Maybe a
  } deriving (Show)

instance EmptyZohoStructure () where
  emptyZohoStructure = ()

instance EmptyZohoStructure Aeson.Value where
  emptyZohoStructure = Aeson.Null

instance EmptyZohoStructure (Contact a) where
  emptyZohoStructure = Contact
    { contactVisitSummary = Nothing
    , contactScoreSummary = Nothing
    , contactGoogleAdsInfo = Nothing
    , contactSpecialFields = Nothing
    , contactFixedFields = Nothing
    , contactOtherFields = Nothing
    }

emptyContact :: Contact a
emptyContact = emptyZohoStructure

$(deriveJSON (Casing.aesonPrefix Casing.snakeCase) ''Approval)
$(deriveJSON (Casing.aesonPrefix (('$':) . Casing.snakeCase)) ''ContactSpecialFields)
$(deriveJSON (Casing.aesonPrefix pascalSnakeCase) ''ContactFixedFields)

instance (FromJSON a) => FromJSON (Contact a) where
  parseJSON = withObject "Exepcting a JSON object to parse into a Contact" $ \o -> do
    let x = Object o
    contactVisitSummary <- parseJSON x
    contactScoreSummary <- parseJSON x
    contactGoogleAdsInfo <- parseJSON x
    contactSpecialFields <- parseJSON x
    contactFixedFields  <- parseJSON x
    contactOtherFields <- parseJSON x
    pure Contact{..}

instance (ToJSON a) => ToJSON (Contact a) where
  toJSON Contact{..} =
    mergeObject (toJSON contactVisitSummary) $
    mergeObject (toJSON contactScoreSummary) $
    mergeObject (toJSON contactGoogleAdsInfo) $
    mergeObject (toJSON contactSpecialFields) $
    mergeObject (toJSON contactFixedFields) (toJSON contactOtherFields)
    where
      mergeObject (Aeson.Object x) (Aeson.Object y) = (Aeson.Object $ x <> y)
      mergeObject (Aeson.Object x) Aeson.Null = Aeson.Object x
      mergeObject Aeson.Null (Aeson.Object x) = Aeson.Object x
      mergeObject Aeson.Null Aeson.Null = Aeson.Null
      mergeObject x y = Prelude.error $  "unexpected " <> "\n" <> show x  <> "\n" <> show y

list :: (FromJSON a)
     => ListOptions
     -> Manager
     -> AccessToken
     -> IO (W.Response (Either String (PaginatedResponse "data" [Contact a])))
list = R.list "Contacts"

getSpecific :: (FromJSON a)
            => Text
            -> Manager
            -> AccessToken
            -> IO (W.Response (Either String (Maybe (Contact a))))
getSpecific = R.getSpecificRecord "Contacts"

-- insert :: (ToJSON a)
--        => [Contact a]
--        -> Manager
--        -> AccessToken
--        -> IO (W.Response (Either String [Aeson.Value]))
-- insert contacts mgr tkn = 

$(makeLensesWith abbreviatedFields ''Contact)
$(makeLensesWith abbreviatedFields ''ContactFixedFields)
$(makeLensesWith abbreviatedFields ''ContactSpecialFields)
