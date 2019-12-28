module Zoho.CRM.Common where

import Zoho.OAuth as ZO
import Network.OAuth.OAuth2 as O
import Network.Wreq as W hiding (Proxy(..))
import Zoho.Types
import Data.Time
import Data.Aeson as Aeson
import Data.Aeson.TH
import Data.Aeson.Casing as Casing
import Data.Text as Text

data VisitSummary = VisitSummary
  { vsFirstVisitedTime :: Maybe ZonedTime
  , vsFirstVisitedURL :: Maybe Text
  , vsReferrer :: Maybe Text
  , vsLastVisitedTime :: Maybe ZonedTime
  , vsNumberOfChats :: Maybe Int
  , vsVisitorScore :: Maybe Text
  , vsAverageTimeSpentMinutes :: Maybe Float
  , vsDaysVisited :: Maybe Int
  } deriving (Show)

$(deriveJSON (Casing.aesonPrefix pascalSnakeCase) ''VisitSummary)
