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

data ScoreSummary = ScoreSummary
  { ssScore :: Maybe Int
  , ssPositiveScore :: Maybe Int
  , ssNegativeScore :: Maybe Int
  , ssTouchPointScore :: Maybe Int
  , ssPositiveTouchPointScore :: Maybe Int
  , ssNegativeTouchPointScore :: Maybe Int
  } deriving (Eq, Show)

$(deriveJSON (Casing.aesonPrefix pascalSnakeCase) ''VisitSummary)
$(deriveJSON (Casing.aesonPrefix pascalSnakeCase) ''ScoreSummary)

