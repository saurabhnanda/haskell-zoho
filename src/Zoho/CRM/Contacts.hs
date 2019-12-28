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

data Contact = Contact
  { contactVisitSummary :: VisitSummary
  , contactScoreSummary :: ScoreSummary
  , contactGoogleAdsInfo :: GoogleAdsInfo
  , contactSpecialFields :: ContactSpecialFields
  } deriving (Show)

$(deriveJSON (Casing.aesonPrefix Casing.snakeCase) ''Approval)
$(deriveJSON (Casing.aesonPrefix (('$':) . Casing.snakeCase)) ''ContactSpecialFields)

instance FromJSON Contact where
  parseJSON = withObject "Exepcting a JSON object to parse into a Contact" $ \o -> do
    let x = Object o
    contactVisitSummary <- parseJSON x
    contactScoreSummary <- parseJSON x
    contactGoogleAdsInfo <- parseJSON x
    contactSpecialFields <- parseJSON x
    pure Contact{..}


list :: ListOptions
     -> Manager
     -> AccessToken
     -> IO (W.Response (Either String (PaginatedResponse "data" [Contact])))
list = R.list "Contacts"
