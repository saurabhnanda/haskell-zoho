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

data Approval = Approval
  { apDelegate :: Bool -- delegate
  , apApprove :: Bool
  , apReject :: Bool
  , apResubmit :: Bool
  } deriving (Eq, Show)

data ContactSpecialFields = ContactSpecialFields
  { csfCurrencySymbol :: Maybe Text -- $currency_symbol
  , csfState :: Maybe Text -- $state
  , csfProcessFlow :: Bool -- $process_flow
  , csfApproved :: Bool -- $approved
  , csfApproval :: Maybe Approval  -- $approval
  , csfEditable :: Bool -- $editable

  -- TODO: Figure out what is "review" all about
  -- , csfReviewProcess :: Maybe _ -- $review_process
  -- , csvReview :: Maybe _ -- $review
  } deriving (Eq, Show)

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
  --  , 
  } deriving (Show)

data Contact a = Contact
  { contactVisitSummary :: VisitSummary
  , contactScoreSummary :: ScoreSummary
  , contactGoogleAdsInfo :: GoogleAdsInfo
  , contactSpecialFields :: ContactSpecialFields
  , contactFixedFields :: ContactFixedFields
  , contactOtherFields :: a
  } deriving (Show)

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

list :: (FromJSON a)
     => ListOptions
     -> Manager
     -> AccessToken
     -> IO (W.Response (Either String (PaginatedResponse "data" [Contact a])))
list = R.list "Contacts"
