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

data Contact = Contact
  { contactVisitSummary :: VisitSummary
  , contactScoreSummary :: ScoreSummary
  } deriving (Show)

instance FromJSON Contact where
  parseJSON = withObject "Exepcting a JSON object to parse into a Contact" $ \o -> do
    contactVisitSummary <- parseJSON (Object o)
    contactScoreSummary <- parseJSON (Object o)
    pure Contact{..}

list :: ListOptions
     -> Manager
     -> AccessToken
     -> IO (W.Response (Either String (PaginatedResponse "data" [Contact])))
list = R.list "Contacts"
